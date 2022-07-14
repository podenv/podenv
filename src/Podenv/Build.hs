{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | This modules contains logic to perform application runtime build
module Podenv.Build
  ( prepare,
    BuildEnv (..),
    containerBuildRuntime,
    nixRuntime,
  )
where

import Control.Monad qualified
import Data.Digest.Pure.SHA qualified as SHA
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Podenv.Config qualified
import Podenv.Dhall
import Podenv.Prelude
import Podenv.Runtime (ImageName (..))
import Podenv.Runtime qualified
import System.Directory (doesDirectoryExist, renameFile)
import System.Exit (ExitCode (ExitSuccess))
import System.Process.Typed qualified as P

-- | Helper function to run a standalone app, usefull for local build
type AppRunner = Application -> IO ()

-- | A build env contains action to be performed before preparation and execution
data BuildEnv = BuildEnv
  { beInfos :: Text,
    -- | Builds the runtime
    beEnsure :: AppRunner -> IO (),
    -- | Updates the runtime
    beUpdate :: AppRunner -> IO ()
  }

defaultBuildEnv :: Text -> BuildEnv
defaultBuildEnv beInfos = BuildEnv {..}
  where
    beEnsure = const $ pure ()
    beUpdate = const $ pure ()

-- | Create the build env
prepare :: Podenv.Runtime.RuntimeEnv -> Application -> IO (BuildEnv, Application)
prepare re app = case runtime app of
  Image name -> pure (defaultBuildEnv name, addArgs app)
  Container cb -> pure (prepareContainer cb, addArgs app)
  Rootfs fp -> pure (defaultBuildEnv fp, addArgs app)
  Nix expr -> prepareNix re app expr
  where
    addArgs = appCommand %~ (<> Podenv.Runtime.extraArgs re)

containerBuildRuntime :: ContainerBuild -> Podenv.Runtime.RuntimeContext
containerBuildRuntime = Podenv.Runtime.Container . mkImageName

mkImageName :: ContainerBuild -> ImageName
mkImageName containerBuild = ImageName $ "localhost/" <> name
  where
    -- The image name can be set by the container build,
    -- otherwise it default to the Containerfile hash
    name = fromMaybe imageHash (containerBuild ^. cbImage_name)
    imageHash = toText . SHA.showDigest . SHA.sha256 . encodeUtf8 $ containerBuild ^. cbContainerfile

-- | Container build env
prepareContainer :: ContainerBuild -> BuildEnv
prepareContainer containerBuild = BuildEnv {..}
  where
    -- buildenv basic info:
    beInfos = "# Containerfile " <> imageName <> "\n" <> fileContent
    fileContent = containerBuild ^. cbContainerfile

    ImageName imageName = mkImageName containerBuild
    fileName = "Containerfile_" <> toString (imageNameToFP imageName)
      where
        imageNameToFP = Text.replace "/" "_" . Text.replace ":" "-"

    beEnsure = const $ do
      imageReady <- checkImageExist imageName
      unless imageReady $
        buildImage imageName fileName fileContent (containerBuild ^. cbImage_volumes)

    beUpdate = const $ case containerBuild ^. cbImage_update of
      Nothing -> error "The container is missing the `image_update` attribute"
      Just cmd -> do
        buildImage
          imageName
          (fileName <> "-update")
          (unlines ["FROM " <> imageName, "RUN " <> cmd])
          (containerBuild ^. cbImage_volumes)

-- | Nix runtime re-use the host root filesystem, prepareNix added the nix-store volume.
nixRuntime :: Podenv.Runtime.RuntimeContext
nixRuntime = Podenv.Runtime.Bubblewrap "/"

getCertLocation :: IO (Maybe FilePath)
getCertLocation = runMaybeT $ Control.Monad.msum $ [checkEnv] <> (checkPath <$> paths)
  where
    checkEnv :: MaybeT IO FilePath
    checkEnv = do
      env <- lift $ lookupEnv "NIX_SSL_CERT_FILE"
      case env of
        Just fp -> checkPath fp
        Nothing -> mzero
    checkPath :: FilePath -> MaybeT IO FilePath
    checkPath fp = do
      exist <- lift $ doesPathExist fp
      unless exist mzero
      pure fp
    -- Copied from profile.d/nix.sh
    paths =
      [ "/etc/pki/tls/certs/ca-bundle.crt",
        "/etc/ssl/certs/ca-certificates.crt",
        "/etc/ssl/ca-bundle.pem",
        "/etc/ssl/certs/ca-bundle.crt"
      ]

-- | Nix build env
prepareNix :: Podenv.Runtime.RuntimeEnv -> Application -> Flakes -> IO (BuildEnv, Application)
prepareNix re app flakes = do
  certs <- toText . fromMaybe (error "Can't find ca-bundle") <$> getCertLocation
  -- TODO: check howto re-use the host /nix
  pure
    ( BuildEnv
        { beInfos = "# Nix expr:\n" <> Text.unwords nixArgs,
          beEnsure = beEnsure certs,
          beUpdate = const $ error "Nix update is not implemented"
        },
      updateApp certs app
    )
  where
    name = app ^. appName
    fileName = toString $ "nix_" <> name

    -- The location where we expect to find the `nix` command
    nixStore = Podenv.Runtime.volumesDir re </> "nix-store"
    nixCommandProfile = "var/nix/profiles/nix-install"
    nixCommandPath = "/nix/" <> nixCommandProfile <> "/bin/nix"
    nixFlags = ["--extra-experimental-features", "nix-command flakes"]

    -- The nix command args
    nixExtraArgs = case nixpkgs flakes of
      Just pin | not (all (Text.isPrefixOf pin) (installables flakes)) -> ["--override-input", "nixpkgs", pin]
      _ -> []
    nixArgs = nixExtraArgs <> installables flakes

    beEnsure certs runApp = do
      built <- checkIfBuilt fileName (show nixArgs)
      unless built $ do
        ensureNixInstalled
        _ <- runApp (buildApp certs)

        -- save that the build succeeded
        cacheDir <- getCacheDir
        Text.writeFile (cacheDir </> fileName) (show nixArgs)

    debug = when (Podenv.Runtime.verbose re) . hPutStrLn stderr . mappend "[+] "

    ensureNixInstalled = do
      debug $ "Checking if " <> nixStore </> nixCommandProfile <> " exists"
      nixInstalled <- doesSymlinkExist $ nixStore </> nixCommandProfile
      unless nixInstalled $ do
        debug $ "Checking if " <> nixStore </> "store" <> " exists"
        storeExist <- doesDirectoryExist $ nixStore </> "store"
        when storeExist $ error $ "existing nix-store is invalid, try removing " <> toText nixStore

        podenv <- getExecutablePath
        debug $ "[+] Installing nix-store with " <> podenv <> " nix.setup"
        P.runProcess_ $ P.setDelegateCtlc True $ P.proc podenv ["nix.setup"]

    -- The Application to build the expression, it is executed in advance to separate build and execution
    buildApp certs =
      Podenv.Config.defaultApp
        { runtime = Rootfs "/",
          name = "build-" <> name,
          volumes = [toText nixStore <> ":/nix", "nix-setup-home:~/", "nix-cache:~/.cache/nix"],
          environ = ["NIX_SSL_CERT_FILE=" <> certs, "LC_ALL=C.UTF-8", "TERM=xterm"],
          command =
            [toText nixCommandPath, "--verbose"]
              <> nixFlags
              <> ["build", "--no-link"]
              <> nixArgs
        }
        & (appCapabilities . capNetwork .~ True)

    runCommand =
      [toText nixCommandPath] <> nixFlags <> case app ^. appCommand of
        [] ->
          ["run"] <> nixArgs <> case Podenv.Runtime.extraArgs re of
            [] -> []
            xs -> ["--"] <> xs
        appArgs -> ["shell"] <> nixArgs <> ["--command"] <> appArgs <> Podenv.Runtime.extraArgs re
    addCommand = appCommand .~ runCommand
    addVolumes = appVolumes %~ mappend ["nix-store:/nix", "nix-cache:~/.cache/nix", "nix-config:~/.config/nix"]
    addEnvirons certs =
      appEnviron
        %~ mappend
          [ "NIX_SSL_CERT_FILE=" <> certs,
            "TERM=xterm",
            "LC_ALL=C.UTF-8",
            "PATH=/nix/var/nix/profiles/nix-install/bin:/bin"
          ]
    updateApp certs = addCommand . addVolumes . addEnvirons certs

-- | Build a container image
buildImage :: Text -> FilePath -> Text -> [Text] -> IO ()
buildImage imageName fileName containerfile volumes = do
  uid <- getRealUserID
  cacheDir <- getCacheDir
  createDirectoryIfMissing True cacheDir
  let want = fileName <> ".want"
      wantfp = cacheDir </> want
  Text.writeFile wantfp containerfile
  -- podman build does not support regular volume, lets ensure absolute path
  volumesArgs <- traverse (mkVolumeArg cacheDir) volumes
  let buildArgs =
        ["build"]
          <> ["-t", toString imageName]
          <> ["--build-arg", "USER_UID=" <> show uid]
          <> map toString volumesArgs
          <> ["-f", want, cacheDir]
      cmd = Podenv.Runtime.podman buildArgs
  -- putTextLn $ "Building " <> imageName <> " with " <> toText want <> ": " <> show cmd
  P.runProcess_ cmd

  -- save that the build succeeded
  renameFile wantfp (cacheDir </> fileName)
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

checkIfBuilt :: FilePath -> Text -> IO Bool
checkIfBuilt filename expected = do
  cacheDir <- getCacheDir
  current <- readFileM (cacheDir </> filename)
  pure $ current == expected
