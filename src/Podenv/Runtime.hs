{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | This module contains the podman/bubblewrap context wrapper
module Podenv.Runtime (
    createLocalhostRunEnv,
    createHeadlessRunEnv,
    getPodmanPodStatus,
    deletePodmanPod,
    listRunningApps,
    getAppID,

    -- * Podman helpers
    podman,
    podmanRunArgs,

    -- * Bubblewrap helpers
    bwrap,
    bwrapRunArgs,

    -- * data type and lenses
    RunEnv (..),
    ExecMode (..),
    module Podenv.Context,
    GlobalEnv (..),
    Second (..),
    defaultGlobalEnv,
) where

import Control.Concurrent (threadDelay)
import Control.Exception (try)
import Data.Aeson qualified as Aeson
import Data.Digest.Pure.SHA qualified as SHA
import Data.List qualified
import Data.Map.Strict qualified as Map
import Data.Set qualified
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Time.Clock qualified as Clock
import Podenv.Capability (AppMode (..))
import Podenv.Capability qualified
import Podenv.Config (Config, select)
import Podenv.Context
import Podenv.Dhall
import Podenv.Env
import Podenv.Image
import Podenv.Notifications
import Podenv.Prelude
import System.Directory (doesDirectoryExist, getModificationTime, renameFile)
import System.Exit (ExitCode (..))
import System.Posix.Files qualified
import System.Process.Typed qualified as P

newtype Second = Second Natural deriving newtype (Ord, Eq, Num, Show)

timeDiff :: Clock.UTCTime -> Clock.UTCTime -> Second
timeDiff now old = Second (truncate pico)
  where
    pico = Clock.nominalDiffTimeToSeconds (Clock.diffUTCTime now old)

data RunEnv = RunEnv
    { showBuildInfo :: Runtime -> Text
    , showCmd :: ExecMode -> Context -> ContextEnvT Text
    , buildRuntime :: Runtime -> ContextEnvT (Maybe RuntimeCache)
    -- ^ Prepare the runtime, and maybe return the path to the app
    , updateRuntime :: Runtime -> ContextEnvT ()
    , getRuntimeAge :: Runtime -> ContextEnvT (Maybe Second)
    , appToContext :: AppMode -> ApplicationResource -> IO Context
    , execute :: ExecMode -> Context -> ContextEnvT (Maybe Text)
    -- ^ Execute the runtime, and return the command output when ExecMode is Read
    }

data RuntimeCache = AppCache Text | ShellCache Text | PathsCache [Text]

data RuntimeBackend
    = Podman ImageName
    | Bubblewrap FilePath

data ExecMode = Foreground | Background | Read deriving (Eq, Show)

ensureResolvConf :: FilePath -> IO (Context -> Context)
ensureResolvConf fp
    -- When using host rootfs, then we need to mount /etc/resolv.conf target when it is a symlink
    | fp == "/" = do
        symlink <- System.Posix.Files.isSymbolicLink <$> System.Posix.Files.getSymbolicLinkStatus "/etc/resolv.conf"
        if symlink
            then do
                realResolvConf <- getSymlinkPath
                pure $ addMount realResolvConf (roHostPath realResolvConf)
            else pure id
    -- Otherwise we can just mount it directly
    | otherwise = pure $ addMount "/etc/resolv.conf" (roHostPath "/etc/resolv.conf")
  where
    getSymlinkPath = do
        realResolvConf <- System.Posix.Files.readSymbolicLink "/etc/resolv.conf"
        pure
            $ if "../" `isPrefixOf` realResolvConf
                then drop 2 realResolvConf
                else realResolvConf

data ContainerBuildRuntime = CBR
    { cbrInfo :: Text
    , cbrBuild :: ContextEnvT ()
    , cbrUpdate :: ContextEnvT ()
    , cbrAge :: ContextEnvT (Maybe Second)
    }

createLocalhostRunEnv :: AppEnv 'UnknownHome -> RunEnv
createLocalhostRunEnv appEnv = RunEnv{..}
  where
    appToContext amode ar = do
        let appRuntimeBackend = getRuntimeBackend (ar ^. arApplication . appRuntime)
        setResolv <- case appRuntimeBackend of
            Bubblewrap fp | ar ^. arApplication . appCapabilities . capNetwork -> ensureResolvConf fp
            _ -> pure id
        let ensureCommand ctx = case appRuntimeBackend of
                Bubblewrap _ | null (ctx ^. ctxCommand) -> ctx & ctxCommand .~ ["/bin/sh"]
                _ -> ctx
        ctx <- runAppEnv appEnv ar $ Podenv.Capability.prepare amode
        pure $ ctx & setResolv . ensureCommand

    getRuntimeAge = \case
        Container cb | isJust (cb ^. cbImage_update) -> cbrAge $ manageContainer cb
        _ -> pure Nothing

    getRuntimeBackend = \case
        Image x -> Podman $ ImageName x
        Rootfs root -> Bubblewrap $ toString root
        Container cb -> Podman . mkImageName $ cb
        Nix _ -> Bubblewrap "/"
        DevShell _ -> Bubblewrap "/"
        Shell _ -> Bubblewrap "/"

    showBuildInfo = \case
        Image iname -> "image:" <> iname
        Rootfs fp -> "rootfs:" <> fp
        Container cb -> cbrInfo $ manageContainer cb
        Nix expr -> "nix:" <> show expr
        DevShell expr -> "devshell:" <> show expr
        Shell expr -> "shell:" <> show expr

    buildRuntime = \case
        Container cb -> Nothing <$ cbrBuild (manageContainer cb)
        Nix installable -> Just . AppCache <$> prepareNix installable
        DevShell devShell -> Just . ShellCache <$> prepareNixDevShell devShell
        Shell installables -> Just . PathsCache <$> prepareNixShell installables
        _ -> pure Nothing

    updateRuntime = \case
        Image iname -> error $ "todo: podman pull " <> show iname
        Container cb -> do
            -- ensure the image exist
            cbrBuild $ manageContainer cb
            cbrUpdate $ manageContainer cb
        Nix expr -> cleanNix $ nixFileName expr
        DevShell expr -> cleanNix $ nixDevShellFileName expr
        Shell exprs -> mapM_ cleanNix (nixShellFileName <$> exprs)
        Rootfs _ -> pure ()
      where
        cleanNix fileName = do
            debug $ "Deleting cached result: " <> fileName
            deleteCachedResult $ toString fileName

    execute :: ExecMode -> Context -> ContextEnvT (Maybe Text)
    execute em ctx = do
        re <- ask
        traverse_ (liftIO . ensureHostDirectory (volumesDir re)) (Map.elems $ ctx ^. ctxMounts)
        -- Always ensure the runtime is built before running it
        buildOutput <- buildRuntime $ ctx ^. ctxRuntime
        -- Replace the nix run command with a cached program path output
        let swapNixRun path = \case
                ("/nix/var/nix/profiles/nix-install/bin/nix" : "run" : rest) ->
                    -- remove everything until '--'
                    path : drop 1 (dropWhile (/= "--") rest)
                xs -> xs
        let swapNixDevelop path = \case
                ("/nix/var/nix/profiles/nix-install/bin/nix" : "develop" : rest) ->
                    -- remove everything until '--command'
                    "bash" : "--rcfile" : path : "-i" : case drop 1 (dropWhile (/= "--command") rest) of
                        [] -> []
                        xs -> ["-c", Text.unwords $ quoteArguments xs]
                xs -> xs
        let swapNixShell = \case
                ("/nix/var/nix/profiles/nix-install/bin/nix" : "shell" : rest) ->
                    -- remove everything until '--command'
                    "bash" : "-i" : case drop 1 (dropWhile (/= "--command") rest) of
                        [] -> []
                        xs -> ["-c", Text.unwords $ quoteArguments xs]
                xs -> xs
        ctx' <- case buildOutput of
            Nothing -> pure ctx
            Just (AppCache path) -> do
                debug $ "Using cached path: " <> path
                pure $ ctx & ctxCommand %~ swapNixRun path
            Just (ShellCache path) -> do
                debug $ "Using cached shell: " <> path
                let addCache = case appEnv ^. envHostHomeDir of
                        Nothing -> id
                        Just f -> directMount (f </> ".cache/podenv")
                pure $ ctx & addCache . (ctxCommand %~ swapNixDevelop path)
            Just (PathsCache paths) -> do
                debug $ "Using cached paths: " <> Text.unwords paths
                let pathEnv = Text.intercalate ":" paths
                let doAddPathEnv = \case
                        Just prev -> Just $ pathEnv <> ":" <> prev
                        Nothing -> Just pathEnv
                let addPathEnv = ctxEnviron %~ Map.alter doAddPathEnv "PATH"
                pure $ ctx & addPathEnv . (ctxCommand %~ swapNixShell)

        case getRuntimeBackend (ctx ^. ctxRuntime) of
            Podman image -> Nothing <$ executePodman em ctx' image
            Bubblewrap fp -> case em of
                Foreground -> Nothing <$ executeBubblewrap ctx' fp
                Background -> error "NotImplemented"
                Read -> Just <$> readBubblewrap ctx' fp

    showCmd em ctx = do
        re <- ask
        pure $ case getRuntimeBackend (ctx ^. ctxRuntime) of
            Podman image -> show . P.proc "podman" $ podmanRunArgs re em ctx image
            Bubblewrap fp -> case em of
                Foreground -> show . P.proc "bwrap" $ bwrapRunArgs re ctx fp
                Background -> error "NotImplemented"
                Read -> error "Read NotImplemented"

    manageContainer :: ContainerBuild -> ContainerBuildRuntime
    manageContainer cb = CBR{..}
      where
        cbrInfo = "# Containerfile " <> imageName <> "\n" <> fileContent <> "\n"
        cbrBuild = do
            imageReady <- liftIO $ checkImageExist imageName
            unless imageReady $ do
                debug $ "Building image: " <> imageName
                buildImage imageName fileName fileContent (cb ^. cbImage_volumes)

        cbrAge
            | isJust (cb ^. cbImage_update) = liftIO do
                cacheDir <- getCacheDir
                let localContainerFile = cacheDir </> fileName
                exist <- doesPathExist localContainerFile
                if not exist
                    then pure Nothing
                    else do
                        now <- Clock.getCurrentTime
                        age <- getModificationTime localContainerFile
                        -- check update file too
                        let localContainerUpdateFile = cacheDir </> (fileName <> "-update")
                        existUpdate <- doesPathExist localContainerUpdateFile
                        ageUpdate <-
                            if existUpdate
                                then getModificationTime localContainerUpdateFile
                                else pure age
                        pure $ Just $ timeDiff now (max age ageUpdate)
            | otherwise = pure Nothing

        cbrUpdate = case cb ^. cbImage_update of
            Nothing -> error "The container is missing the `image_update` attribute"
            Just cmd ->
                buildImage
                    imageName
                    (fileName <> "-update")
                    (unlines ["FROM " <> imageName, "RUN " <> cmd])
                    (cb ^. cbImage_volumes)

        fileContent = cb ^. cbContainerfile
        ImageName imageName = mkImageName cb
        fileName = "Containerfile_" <> toString (imageNameToFP imageName)
          where
            imageNameToFP = Text.replace "/" "_" . Text.replace ":" "-"

    nixFileName :: Text -> Text
    nixFileName installable =
        mconcat
            [ "nix_"
            , Text.take 10 . toText . SHA.showDigest . SHA.sha1 . encodeUtf8 $ installableLoc
            ]
      where
        installableLoc
            | "." `Text.isPrefixOf` installable = toText (appEnv ^. envHostCwd) <> installable
            | otherwise = installable
    nixDevShellFileName i = nixFileName i <> " devshell"
    nixShellFileName i = nixFileName i <> " shell"

    prepareNixShell :: [Text] -> ContextEnvT [Text]
    prepareNixShell installables = do
        paths <- traverse prepareNix installables
        pure $ map (Text.dropEnd 1 . Text.dropWhileEnd (/= '/')) paths

    -- The goal of 'prepareNixDevShell' is to convert a devShell into a rcscript
    prepareNixDevShell :: Text -> ContextEnvT Text
    prepareNixDevShell installable =
        Text.pack . fst <$> do
            withCachedResult (toString fileName) $ do
                ensureNixInstalled

                ctx <- liftIO $ mkBuildCtx fileName installable $ [toText nixCommandPath] <> ["print-dev-env"] <> nixArgs installable
                execute Read ctx >>= \case
                    Nothing -> error "The impossible have happened"
                    Just e -> pure e
      where
        fileName = nixDevShellFileName installable

    -- The goal of 'prepareNix' is to convert an installable string into a local executable path
    prepareNix :: Text -> ContextEnvT Text
    prepareNix installable = do
        withCachedResult' (toString fileName) $ do
            ensureNixInstalled
            case decodeInstallable installable of
                InstallableApp -> do
                    debug $ "Discovering flake app path " <> installable <> ", cache:" <> fileName
                    evalApp installable
                InstallablePkg -> do
                    debug $ "Discovering flake package path " <> installable <> ", cache:" <> fileName
                    evalPackage installable
                InstallableAttr name -> do
                    debug $ "Discovering flake attr path " <> installable <> ", cache:" <> fileName
                    discoverFlake name
                InstallableDefault -> do
                    debug $ "Discovering flake default path " <> installable <> ", cache:" <> fileName
                    discoverFlake "default"
      where
        baseFlake = Text.takeWhile (/= '#') installable

        -- Resolve the flake output path according to the 'flake run' documentation
        discoverFlake :: Text -> ContextEnvT Text
        discoverFlake name
            | not isNixpkgs = do
                -- Try "apps.<system>.<name>"
                eApp <- tryRun $ evalApp $ mkPath "#apps."
                case eApp of
                    Left (P.ExitCodeException{}) -> do
                        -- Try "packages.<system>.<name>"
                        debug $ name <> " is not an app, trying as a package"
                        ePkg <- tryRun $ evalPackage $ mkPath "#packages."
                        case ePkg of
                            Left (P.ExitCodeException{}) -> do
                                -- Try "legacyPackages.<system>.<name>"
                                debug $ name <> " is not a package, trying as legacyPackages"
                                evalLegacyPackage
                            Right fp -> pure fp
                    Right fp -> pure fp
            | -- try legacyPackages directly for nixpkgs
              otherwise =
                evalLegacyPackage
          where
            isNixpkgs = "nixpkgs" `Text.isPrefixOf` installable || "github:nixos/nixpkgs" `Text.isPrefixOf` Text.toLower installable
            evalLegacyPackage = evalPackage $ mkPath "#legacyPackages."
            -- TODO: support aarch
            system = "x86_64-linux"
            mkPath kind = mconcat [baseFlake, kind, system, ".", name]

        -- Get the location of the app program attribute
        evalApp :: Text -> ContextEnvT Text
        evalApp installableFull = do
            let installableProgram = installableFull <> ".program"
            path <- evalRun installableProgram

            -- ensure the app is available in the store (eval may not produce the executable)
            -- this is a bit weird because flakes #apps. are not derivation, and nix run doesn't have a '--dry-run' option.
            -- so we can force the build of the app program by computing it's hash!
            ctx <- liftIO $ buildCtx $ [toText nixCommandPath] <> ["eval", "--raw", "--apply", "builtins.hashFile \"sha1\""] <> nixArgs installableProgram
            void $ execute Read ctx

            -- Return the final path
            pure path

        -- Get the location of the package executable path
        evalPackage :: Text -> ContextEnvT Text
        evalPackage installableFull = do
            path <- evalRun installableFull
            pname <- evalRun $ installableFull <> ".pname"

            -- ensure the package is available in the store (eval may not produce the executable)
            ctx <- liftIO $ buildCtx $ [toText nixCommandPath] <> ["build", "--no-link"] <> nixArgs installableFull
            void $ execute Foreground ctx

            -- Return the final path
            pure $ path <> "/bin/" <> pname

        fileName = nixFileName installable
        evalRun arg = do
            let args = [toText nixCommandPath] <> ["eval", "--raw"] <> nixArgs arg
            ctx <- liftIO $ buildCtx args
            execute Read ctx >>= \case
                Nothing -> error "The impossible has happened"
                Just output -> pure output

        -- The location where we expect to find the `nix` command
        buildCtx = mkBuildCtx fileName installable
    builderApp = defaultApp (Rootfs "/") & (appCapabilities . capNetwork) .~ True
    mkBuildCtx fileName installable args = do
        let builderName = Name $ "builder-" <> fileName
        ctx <- runAppEnv appEnv (defaultAppRes builderApp) $ \ar -> do
            setNix <- Podenv.Capability.setNix installable
            setNix <$> Podenv.Capability.prepare (Regular args) ar
        setResolv <- ensureResolvConf "/"
        pure $ ctx & setResolv . (ctxName ?~ builderName)

    nixStore re = Podenv.Runtime.volumesDir re </> "nix-store"
    ensureNixInstalled = do
        re <- ask
        let store = nixStore re
        nixInstalled <- liftIO $ doesSymlinkExist $ store </> nixCommandProfile
        unless nixInstalled $ do
            debug $ toText $ store </> nixCommandProfile <> " does not exists, installing nix.setup"

            -- ensure we are not somehow overwritting an existing store
            storeExist <- liftIO $ doesDirectoryExist $ store </> "store"
            when storeExist $ error $ "existing nix-store is invalid, try removing " <> toText (nixStore re)

            let cfg = fromMaybe (error "Need config") $ config re
                nixSetupApp = case Podenv.Config.select cfg ["nix.setup"] of
                    Left e -> error e
                    Right (_, setupApp) -> setupApp
            debug "Installing nix-store with nix.setup"
            ctx <- liftIO $ appToContext (Regular []) nixSetupApp
            void $ execute Foreground ctx

quoteArguments :: [Text] -> [Text]
quoteArguments [] = []
quoteArguments (x : xs)
    | x == "" = "\"\"" : quoteArguments xs
    | Just _ <- Text.findIndex (== ' ') x = mconcat ["\"", x, "\""] : quoteArguments xs
    | otherwise = x : quoteArguments xs

data InstallableKind = InstallableApp | InstallablePkg | InstallableAttr Text | InstallableDefault

decodeInstallable :: Text -> InstallableKind
decodeInstallable installable
    | "apps." `Text.isPrefixOf` attr = InstallableApp
    | "packages." `Text.isPrefixOf` attr = InstallablePkg
    | "" == attr = InstallableDefault
    | otherwise = InstallableAttr attr
  where
    attr = Text.drop 1 $ Text.dropWhile (/= '#') installable

createHeadlessRunEnv :: Config -> AppEnv 'UnknownHome -> RunEnv
createHeadlessRunEnv cfg appEnv =
    headlessRun
        { showCmd = headlessShow
        , execute = headlessExecute
        , appToContext = appToHeadlessContext
        }
  where
    headlessRun = createLocalhostRunEnv headlessEnv

    getApp ns name = do
        pure $ case Podenv.Config.select cfg [name] of
            Left e -> error $ "Can't find " <> name <> ": " <> e
            Right ([], app) -> app & (arMetadata . metaNamespace) .~ ns
            Right (xs, _) -> error $ "Apps has argument?! " <> show xs

    getHeadlessApp ns displayAppName = do
        displayApp <- getApp ns displayAppName
        let setCap = arApplication . appCapabilities %~ (capWayland .~ True) . (capX11 .~ True)
            setName =
                (arMetadata . metaName)
                    ?~ ( "headless-" <> case displayAppName of
                            "default" -> "display"
                            _ -> displayAppName
                       )
        liftIO $ appToHeadlessContext (Regular []) (displayApp & setCap . setName)

    headlessShow em ctx = do
        displayCtx <- getHeadlessApp (ctx ^. ctxNamespace) "default"
        vncCtx <- getHeadlessApp (ctx ^. ctxNamespace) "vnc"
        displayCmd <- showCmd headlessRun Background displayCtx
        vncCmd <- showCmd headlessRun Background vncCtx
        appCmd <- showCmd headlessRun em ctx
        pure $ "Display " <> displayCmd <> "\nVnc " <> vncCmd <> "\n" <> appCmd

    headlessEnv =
        appEnv
            & (envHostDisplay .~ ":0")
            . (envHostWaylandSocket ?~ SocketName "wayland-1")

    appToHeadlessContext amode ar = do
        headlessContext <$> appToContext headlessRun amode ar

    headlessContext :: Context -> Context
    headlessContext ctx = ctx & ctxMounts %~ Map.fromList . map replaceVolume . Map.toList
      where
        nsPrefix = case ctx ^. ctxNamespace of
            Nothing -> ""
            Just ns -> ns <> "-"
        mkVolume n = Volume $ nsPrefix <> n
        replaceVolume :: (FilePath, Volume) -> (FilePath, Volume)
        replaceVolume ("/tmp/.X11-unix", _) = ("/tmp/.X11-unix", MkVolume RW (mkVolume "headless-x11"))
        replaceVolume ("/run/user/1000/wayland-1", _) = ("/run/user/1000", MkVolume RW (mkVolume "headless-xdg"))
        replaceVolume ("/run/user/1000/pulse", _) = ("/run/user/1000/pulse", MkVolume RW (mkVolume "headless-pulse"))
        replaceVolume v = v

    headlessExecute em ctx = do
        let localRun = createLocalhostRunEnv appEnv
        displayCtx <- getHeadlessApp (ctx ^. ctxNamespace) "default"
        vncCtx <- getHeadlessApp (ctx ^. ctxNamespace) "vnc"
        clientApp <- getApp (ctx ^. ctxNamespace) "vnc-viewer"
        clientCtx <-
            liftIO
                $ appToContext
                    localRun
                    (Regular ["localhost"])
                    (clientApp & arNetwork .~ Shared "container:vnc")
        void $ execute localRun Background displayCtx
        void $ execute localRun Background vncCtx
        void $ execute localRun Background clientCtx
        void $ execute localRun em ctx
        pure Nothing

-- | Build a container image
buildImage :: Text -> FilePath -> Text -> [Text] -> ContextEnvT ()
buildImage imageName fileName containerfile volumes = do
    hostUid <- liftIO getRealUserID
    cacheDir <- liftIO getCacheDir
    liftIO $ createDirectoryIfMissing True cacheDir
    let want = fileName <> ".want"
        wantfp = cacheDir </> want
    liftIO $ Text.writeFile wantfp containerfile
    -- podman build does not support regular volume, lets ensure absolute path
    volumesArgs <- traverse (liftIO . mkVolumeArg cacheDir) volumes
    let buildArgs =
            ["build"]
                <> ["-t", toString imageName]
                <> ["--build-arg", "USER_UID=" <> show hostUid]
                <> map toString volumesArgs
                <> ["-f", want, cacheDir]
        cmd = Podenv.Runtime.podman buildArgs
    -- putTextLn $ "Building " <> imageName <> " with " <> toText want <> ": " <> show cmd
    runProcess cmd
    -- save that the build succeeded
    liftIO $ renameFile wantfp (cacheDir </> fileName)
  where
    mkVolumeArg :: FilePath -> Text -> IO Text
    mkVolumeArg cacheDir volume = do
        createDirectoryIfMissing True hostPath
        pure $ "-v=" <> toText hostPath <> ":" <> containerPath <> ":Z"
      where
        (p1, p2) = Text.break (== ':') volume
        hostPath = cacheDir </> toString p1
        containerPath = Text.drop 1 p2

checkImageExist :: Text -> IO Bool
checkImageExist imageName = do
    res <- P.runProcess (Podenv.Runtime.podman ["image", "exists", Text.unpack imageName])
    pure $ res == ExitSuccess

deleteCachedResult :: (MonadIO m) => FilePath -> m ()
deleteCachedResult fileName = do
    cacheDir <- liftIO getCacheDir
    liftIO $ deleteFileM $ cacheDir </> fileName

withCachedResult' :: (MonadIO m) => FilePath -> m Text -> m Text
withCachedResult' fileName action = snd <$> withCachedResult fileName action

withCachedResult :: (MonadIO m) => FilePath -> m Text -> m (FilePath, Text)
withCachedResult fileName action = do
    cacheDir <- liftIO getCacheDir
    liftIO $ createDirectoryIfMissing True cacheDir
    let cachePath = cacheDir </> fileName
    liftIO (readFileM cachePath) >>= \case
        -- A new cache
        "" -> do
            result <- action
            liftIO $ Text.writeFile cachePath result
            pure (cachePath, result)
        current -> pure (cachePath, current)

-- | Create host directory and set SELinux label if needed
ensureHostDirectory :: FilePath -> Volume -> IO ()
ensureHostDirectory volumesDir (MkVolume _ (Volume volumeName)) =
    ensureHostDirectory' $ volumesDir </> toString volumeName
ensureHostDirectory _ (MkVolume _ (HostPath fp)) | (last <$> nonEmpty fp) == Just '/' = ensureHostDirectory' fp
ensureHostDirectory _ _ = pure ()

ensureHostDirectory' :: FilePath -> IO ()
ensureHostDirectory' fp = do
    exist <- doesPathExist fp
    unless exist $ do
        createDirectoryIfMissing True fp
        P.runProcess_ $ P.proc "/bin/chcon" ["system_u:object_r:container_file_t:s0", fp]
        -- Ensure x11 directory has the sticky bit
        when ("-x11" `Data.List.isSuffixOf` fp) $ P.runProcess_ $ P.proc "/bin/chmod" ["1777", fp]

executeBubblewrap :: Context -> FilePath -> ContextEnvT ()
executeBubblewrap ctx fp = do
    re <- ask
    let args = bwrapRunArgs re ctx fp
    let cmd = bwrap args
    debug $ show cmd
    runProcess cmd

readBubblewrap :: Context -> FilePath -> ContextEnvT Text
readBubblewrap ctx fp = do
    re <- ask
    let args = bwrapRunArgs re ctx fp
    let cmd = bwrap args
    debug $ show cmd
    readProcess cmd

tryRun :: (Exception e) => ContextEnvT a -> ContextEnvT (Either e a)
tryRun (ReaderT inner) = ReaderT \env -> try $ inner env

bwrap :: [String] -> P.ProcessConfig () () ()
bwrap = P.setDelegateCtlc True . P.proc "bwrap"

commonArgs :: Context -> [Text]
commonArgs ctx =
    concatMap (\c -> ["--cap-add", show c]) $ sort $ Data.Set.toList (ctx ^. ctxSyscaps)

bwrapRunArgs :: GlobalEnv -> Context -> FilePath -> [String]
bwrapRunArgs GlobalEnv{..} ctx fp = toString <$> args
  where
    userArg = case ctx ^. ctxRunAs of
        Just RunAsRoot -> ["--unshare-user", "--uid", "0"]
        Just (RunAsUID _) -> []
        Just RunAsAnyUID -> ["--unshare-user", "--uid", show $ ctx ^. ctxAnyUid]
        Nothing -> []

    networkArg = case ctx ^. ctxNetwork of
        Just Host -> []
        Just (Shared _name) -> error "Shared netns not implemented"
        Just Private -> [] -- TODO: implement private network namespace
        Nothing -> ["--unshare-net"]

    volumeArg :: (FilePath, Volume) -> [Text]
    volumeArg (destPath, MkVolume mode vtype) = case vtype of
        HostPath hostPath -> [volumeMode mode, toText hostPath, toText destPath]
        Volume x -> [volumeMode mode, toText $ volumesDir </> toString x, toText destPath]
        TmpFS -> ["--tmpfs", toText destPath]
      where
        volumeMode = \case
            RO -> "--ro-bind"
            RW -> "--bind"

    rootMounts = case fp of
        "/" ->
            doBind "usr"
                <> doBind "lib"
                <> doBind "lib64"
                <> doBind "bin"
                <> doBind "sbin"
                <> doBind "etc"
        c : _ | c `notElem` ['/', ':'] -> toText <$> [bindMode, toString volumesDir </> fp, "/"]
        _ -> doBind ""

    sysMounts
        | Data.Set.null (ctx ^. ctxDevices) = []
        | otherwise = ["--ro-bind", "/sys", "/sys"]

    bindMode
        | ctx ^. ctxRO = "--ro-bind"
        | otherwise = "--bind"
    doBind p = toText <$> [bindMode, fp </> p, "/" </> p]
    ipcNS
        | ctx ^. ctxHostIPC = []
        | otherwise = ["--unshare-ipc"]
    args =
        userArg
            <> ["--die-with-parent", "--unshare-pid", "--unshare-uts"]
            <> ipcNS
            <> networkArg
            <> commonArgs ctx
            <> rootMounts
            <> ["--proc", "/proc"]
            <> ["--dev", "/dev"]
            <> ["--perms", "01777", "--tmpfs", "/tmp"]
            <> concatMap volumeArg (Map.toAscList (ctx ^. ctxMounts))
            <> concatMap (\d -> ["--dev-bind", toText d, toText d]) (ctx ^. ctxDevices)
            <> sysMounts
            <> ["--clearenv"]
            <> concatMap (\(k, v) -> ["--setenv", toText k, v]) (Map.toAscList (ctx ^. ctxEnviron))
            <> cond (not (ctx ^. ctxTerminal)) ["--new-session"]
            <> maybe [] (\wd -> ["--chdir", toText wd]) (ctx ^. ctxWorkdir)
            <> ctx
            ^. ctxCommand

data GlobalEnv = GlobalEnv
    { verbose :: Bool
    , config :: Maybe Config
    , notifier :: Notifier
    , volumesDir :: FilePath
    -- ^ The host location of the volumes directory, default to ~/.local/share/podenv/volumes
    }

runProcess :: P.ProcessConfig a b c -> ContextEnvT ()
runProcess p = do
    res <- liftIO $ P.runProcess p
    case res of
        ExitSuccess -> pure ()
        ExitFailure code -> do
            logMessage Error $ "Command failed (" <> show code <> ") :" <> show p
            fail "process error"

readProcess :: P.ProcessConfig a b c -> ContextEnvT Text
readProcess p = do
    output <- liftIO $ P.readProcessStdout_ p
    pure $ decodeUtf8 output

defaultGlobalEnv :: FilePath -> GlobalEnv
defaultGlobalEnv = GlobalEnv True Nothing defaultNotifier

type ContextEnvT a = ReaderT GlobalEnv IO a

logMessage :: Severity -> Text -> ContextEnvT ()
logMessage sev msg = do
    notifier <- asks notifier
    liftIO $ sendMessage notifier sev msg

debug :: Text -> ContextEnvT ()
debug msg = do
    isVerbose <- asks verbose
    when isVerbose $ logMessage Info msg

cond :: Bool -> [a] -> [a]
cond b xs = if b then xs else []

infraName :: Text -> Name
infraName ns = Name $ "infra-" <> ns

podmanArgs :: Context -> [Text]
podmanArgs ctx = cond (ctx ^. ctxInteractive) ["-i", "--detach-keys", ""] <> cond (ctx ^. ctxTerminal) ["-t"]

podmanRunArgs :: GlobalEnv -> ExecMode -> Context -> ImageName -> [String]
podmanRunArgs gl rmode ctx image = toString <$> args
  where
    networkArg = case ctx ^. ctxNetwork of
        Just Host -> ["--network", "host"]
        Just (Shared name) ->
            let netName
                    | "container:" `Text.isPrefixOf` name = name
                    | otherwise = "container:" <> unName (infraName name)
             in ["--network", netName]
        Just Private -> []
        Nothing -> ["--network", "none"]

    volumeArg :: (FilePath, Volume) -> [Text]
    volumeArg (fp, MkVolume mode vtype) = case vtype of
        HostPath x -> volume (toText x)
        Volume x -> volume (toText $ volumesDir gl </> toString x)
        TmpFS -> ["--mount", "type=tmpfs,destination=" <> toText fp]
      where
        volume hp = ["--volume", hp <> ":" <> toText fp <> showVolumeMode mode]
        showVolumeMode = \case
            RO -> ":ro"
            RW -> ""

    -- The goal here is to ensure host files created by the container are readable by the host user.
    userArg = case ctx ^. ctxRunAs of
        Just RunAsRoot -> ["--user", "0"]
        Just (RunAsUID uid) -> ["--user", show uid, "--userns", "keep-id"]
        Just RunAsAnyUID ->
            let x = ctx ^. ctxAnyUid
             in ["--user", show x, "--uidmap", show x <> ":0:1", "--uidmap", "0:1:" <> show x]
        Nothing -> []

    labelArg = concatMap mkLabel (Map.toList $ ctx ^. ctxLabels)
      where
        mkLabel (k, v) = ["--label", k <> "=" <> v]

    ipcNS
        | ctx ^. ctxHostIPC = ["--ipc=host"]
        | otherwise = []

    args =
        ["run"]
            <> podmanArgs ctx
            <> ["--detach" | rmode == Background]
            <> cond (ctx ^. ctxPrivileged) ["--privileged"]
            <> ["--rm"]
            <> cond (ctx ^. ctxRO) ["--read-only=true"]
            <> cond (not (ctx ^. ctxSELinux)) ["--security-opt", "label=disable"]
            <> userArg
            <> ipcNS
            <> maybe [] (\n -> ["--hostname", n]) (ctx ^. ctxHostname)
            <> networkArg
            <> commonArgs ctx
            <> concatMap (\d -> ["--device", toText d]) (ctx ^. ctxDevices)
            <> maybe [] (\wd -> ["--workdir", toText wd]) (ctx ^. ctxWorkdir)
            <> concatMap (\(k, v) -> ["--env", toText $ k <> "=" <> v]) (Map.toAscList (ctx ^. ctxEnviron))
            <> concatMap volumeArg (("/tmp", MkVolume RW TmpFS) : Map.toAscList (ctx ^. ctxMounts))
            <> maybe [] (\name -> ["--name", unName name]) (ctx ^. ctxName)
            <> labelArg
            <> [unImageName image]
            <> (ctx ^. ctxCommand)

podman :: [String] -> P.ProcessConfig () () ()
podman = P.setDelegateCtlc True . P.proc "podman"

data PodmanStatus
    = -- | The container does not exists, it needs to be created
      NotFound
    | -- | The container is already running
      Running
    | -- | The container ran and it is now stopped
      Unknown Text
    deriving (Show, Eq)

getPodmanPodStatus :: (MonadIO m) => Name -> m PodmanStatus
getPodmanPodStatus (Name cname) = do
    (_, stdout', _) <- P.readProcess (podman ["inspect", Text.unpack cname, "--format", "{{.State.Status}}"])
    pure $ case stdout' of
        "" -> NotFound
        "running\n" -> Running
        other -> Unknown (Text.dropWhileEnd (== '\n') $ decodeUtf8 other)

deletePodmanPod :: Name -> ContextEnvT ()
deletePodmanPod (Name cname) =
    runProcess (podman ["rm", toString cname])

ensureInfraNet :: Text -> ContextEnvT ()
ensureInfraNet ns = do
    debug $ "Ensuring infra net for: " <> show ns
    let infraPod = infraName ns
    infraStatus <- getPodmanPodStatus infraPod
    case infraStatus of
        Running -> pure ()
        _ -> do
            when (infraStatus /= NotFound)
                $
                -- Try to delete any left-over infra container
                deletePodmanPod infraPod

            let cmd =
                    podman
                        $ map toString
                        $ ["run", "--rm", "--name", unName infraPod]
                        <> ["--detach"]
                        <> ["ubi8"]
                        <> ["sleep", "infinity"]
            debug $ show cmd
            P.runProcess_ cmd

executePodman :: ExecMode -> Context -> ImageName -> ContextEnvT ()
executePodman rm ctx image = do
    re <- ask
    case ctx ^. ctxNetwork of
        Just (Shared ns) -> ensureInfraNet ns
        _ -> pure ()

    argsM <- case ctx ^. ctxName of
        Just name -> do
            status <- getPodmanPodStatus name
            debug $ "Podman status of " <> unName name <> ": " <> show status
            let cfail err = liftIO . mayFail . Left $ unName name <> ": " <> err
            case status of
                NotFound -> pure $ Just $ podmanRunArgs re rm ctx image
                Running -> case rm of
                    Foreground -> cfail "container is already running, use `exec` to join, or `--name new` to start a new instance"
                    Background -> pure Nothing
                    Read -> error "Read NotImplemented"
                Unknown _ -> do
                    deletePodmanPod name
                    pure $ Just $ podmanRunArgs re rm ctx image
        Nothing -> pure $ Just $ podmanRunArgs re rm ctx image
    case argsM of
        Just args -> do
            let cmd = podman args
            debug $ show cmd
            case rm of
                Foreground -> runProcess cmd
                Read -> error "Read NotImplemented"
                Background -> do
                    name <- executeBackgroundPodman args
                    let waitForRunning count
                            | count == 0 = error "Container failed to start"
                            | otherwise = do
                                debug $ "Getting status of " <> unName name
                                liftIO $ threadDelay 50_000
                                status <- getPodmanPodStatus name
                                case status of
                                    Running -> pure ()
                                    _ -> waitForRunning (count - 1)
                    waitForRunning (3 :: Int)
        Nothing -> pure ()

executeBackgroundPodman :: [String] -> ContextEnvT Name
executeBackgroundPodman args = do
    (Text.dropWhileEnd (== '\n') . decodeUtf8 -> out, _) <- P.readProcess_ $ P.proc "podman" args
    case Text.length out of
        64 -> pure $ Name out
        _ -> do
            logMessage Error $ "Unknown name: " <> out
            fail "process error"

data PodmanProc = PodmanProc
    { ppId :: Text
    , ppStatus :: Text
    , ppImage :: Text
    , ppLabels :: Map Text Text
    }
    deriving (Show, Eq, Generic)

instance Aeson.FromJSON PodmanProc where
    parseJSON = Aeson.genericParseJSON $ Aeson.defaultOptions{Aeson.fieldLabelModifier = drop 2}

getPodmanProcs :: IO [PodmanProc]
getPodmanProcs = do
    (out, _) <- P.readProcess_ cmd
    case Aeson.eitherDecode out of
        Left e -> error $ "Can't read " <> show cmd <> " output: " <> show e
        Right x -> pure x
  where
    cmd = "podman ps --filter label=podenv.selector --format json"

getAppName :: Map Text Text -> Text
getAppName = fromMaybe "unknown" . Map.lookup "podenv.selector"

formatPodmanProc :: PodmanProc -> Text
formatPodmanProc PodmanProc{..} =
    Text.unwords
        [ppId, ppImage, ppStatus, getAppName ppLabels]

-- | List podman container started by podenv. TODO: add bwrap process
listRunningApps :: IO [Text]
listRunningApps = fmap formatPodmanProc <$> getPodmanProcs

getAppID :: Text -> IO Text
getAppID name = go =<< getPodmanProcs
  where
    go [] = pure ""
    go (p : rest)
        | getAppName (ppLabels p) == name = pure (ppId p)
        | otherwise = go rest
