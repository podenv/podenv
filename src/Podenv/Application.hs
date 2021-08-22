{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

-- | This module contains the capability logic.
-- The goal is to convert an Application into a Context
--
-- This module performs read-only IO
module Podenv.Application
  ( prepare,
    capsAll,
    Cap (..),
  )
where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Podenv.Dhall
import Podenv.Prelude
import qualified Podenv.Runtime as Ctx

-- | The reader env required to prepare the Context
data AppEnv = AppEnv
  { _hostXdgRunDir :: Maybe FilePath,
    _hostHomeDir :: Maybe FilePath,
    _hostCwd :: FilePath,
    _hostUid :: UserID,
    _appHomeDir :: Maybe FilePath
  }
  deriving (Show)

type AppEnvT a = ReaderT AppEnv IO a

-- | Converts an Application into a Context
prepare :: Application -> IO Ctx.Context
prepare app = do
  appEnv <-
    AppEnv
      <$> lookupEnv "XDG_RUNTIME_DIR"
      <*> lookupEnv "HOME"
      <*> getCurrentDirectory
      <*> getRealUserID
      <*> pure (toString <$> containerHome)
  runReaderT (doPrepare app {home = containerHome}) appEnv
  where
    containerHome =
      -- The root cap needs to be applied first as it modifies the App
      if app ^. appCapabilities . capRoot
        then Just "/root"
        else app ^. appHome

doPrepare :: Application -> AppEnvT Ctx.Context
doPrepare app = do
  uid <- asks _hostUid
  let baseCtx =
        (Ctx.defaultContext (app ^. appName) image)
          { Ctx._uid = uid,
            Ctx._namespace = app ^. appNamespace
          }

  ctx <-
    foldM addVolume baseCtx (app ^. appVolumes)
      >>= flip (foldM setCaps) capsAll

  setCommand <-
    if app ^. appCapabilities . capHostfile
      then resolveFileArgs $ app ^. appCommand
      else pure $ Ctx.command .~ app ^. appCommand
  pure (setCommand . modifiers $ ctx)
  where
    image = case app ^. appRuntime of
      Image x -> Ctx.ImageName x
      _ -> error "Can't prepare a non runtime Image application"

    modifiers :: Ctx.Context -> Ctx.Context
    modifiers = disableSelinux . setRunAs . addSysCaps . addEnvs . setPort . ensureWorkdir

    ensureWorkdir = case app ^. appHome of
      (Just x) -> Ctx.workdir `setWhenNothing` toString x
      _ -> id

    setPort = case app ^. appProvide of
      Just (Tcp p) -> Ctx.ports %~ (Ctx.PortTcp p :)
      Nothing -> id

    -- Some capabilities do not work with selinux
    noSelinuxCaps = [capWayland, capX11]
    hasPrivCap = any (\l -> app ^. appCapabilities . l) noSelinuxCaps
    -- When using host device, selinux also needs to be disabled
    hasDevice ctx = ctx ^. Ctx.devices /= mempty
    -- When using direct host path, its simpler to disable selinux too. That can be improved though
    isHostPath = \case
      Ctx.MkVolume _ (Ctx.HostPath _) -> True
      _anyOtherVolume -> False
    hasHostPath ctx = any isHostPath (toList $ ctx ^. Ctx.mounts)
    disableSelinux ctx
      | hasPrivCap || hasDevice ctx || hasHostPath ctx = ctx & Ctx.selinux .~ False
      | otherwise = ctx

    setRunAs = case app ^. appHome of
      -- To keep it simple, when the app home is in `/home`, assume we share the host uid.
      Just h | "/home" `Text.isPrefixOf` h -> Ctx.runAs `setWhenNothing` Ctx.RunAsHostUID
      _ -> id

    addSysCaps ctx = foldr addSysCap ctx (app ^. appSyscaps)
    addSysCap :: Text -> Ctx.Context -> Ctx.Context
    addSysCap syscap = case readMaybe (toString $ "CAP_" <> syscap) of
      Nothing -> error $ "Can't read syscap: " <> show syscap
      Just c -> Ctx.syscaps %~ (c :)

    addEnvs ctx = foldr addEnv ctx (app ^. appEnviron)
    addEnv :: Text -> Ctx.Context -> Ctx.Context
    addEnv env =
      let (k, v) = Text.breakOn "=" env
       in Ctx.addEnv k (Text.drop 1 v)

    setCaps = capContextApply $ app ^. appCapabilities
    capContextApply :: Capabilities -> Ctx.Context -> Cap -> AppEnvT Ctx.Context
    capContextApply appCaps ctx Cap {..} =
      if appCaps ^. capLens
        then capSet ctx
        else pure ctx

-- | CapInfo describes a capability and how it modify the runtime context
data Cap = Cap
  { capName :: Text,
    capDescription :: Text,
    -- | How to get the capability value from the user provided record:
    capLens :: Lens' Capabilities Bool,
    -- | How the capability change the context:
    capSet :: Ctx.Context -> AppEnvT Ctx.Context
  }

-- | The main list of capabilities
capsAll, capsToggle :: [Cap]
capsAll = capsToggle
capsToggle =
  [ Cap "root" "run as root" capRoot (contextSet Ctx.runAs (Just Ctx.RunAsRoot)),
    Cap "terminal" "allocate a tty" capTerminal setTerminal,
    Cap "interactive" "interactive mode" capInteractive (contextSet Ctx.interactive True),
    Cap "dbus" "share session dbus socket" capDbus setDbus,
    Cap "wayland" "share wayland socket" capWayland setWayland,
    Cap "pipewire" "share pipewire socket" capPipewire setPipewire,
    Cap "video" "share video devices" capVideo setVideo,
    Cap "dri" "share graphic device" capDri (pure . Ctx.addDevice "/dev/dri"),
    Cap "kvm" "share kvm device" capKvm (pure . Ctx.addDevice "/dev/kvm"),
    Cap "tun" "share tun device" capTun (pure . Ctx.addDevice "/dev/net/tun"),
    Cap "alsa" "share alsa devices" capAlsa (pure . Ctx.addDevice "/dev/snd"),
    Cap "pulseaudio" "share pulseaudio socket" capPulseaudio setPulseaudio,
    Cap "ssh" "share ssh agent and keys" capSsh setSsh,
    Cap "gpg" "share gpg agent and keys" capGpg setGpg,
    Cap "x11" "share x11 socket" capX11 setX11,
    Cap "cwd" "mount cwd" capCwd setCwd,
    Cap "keep" "keep after run" capKeep (contextSet Ctx.keep True),
    Cap "network" "enable network" capNetwork (contextSet Ctx.network Ctx.Private),
    Cap "host-network" "enable host network" capHost_network (contextSet Ctx.network Ctx.Host),
    Cap "hostfile" "mount command file arg" capHostfile pure
  ]

setTerminal :: Ctx.Context -> AppEnvT Ctx.Context
setTerminal ctx =
  pure $ ctx & (Ctx.interactive .~ True) . (Ctx.terminal .~ True)

setWayland :: Ctx.Context -> AppEnvT Ctx.Context
setWayland ctx = do
  let skt = "wayland-0" -- TODO discover skt name
  shareSkt <- addXdgRun skt
  pure $
    ctx
      & Ctx.directMount "/etc/machine-id"
        . shareSkt
        . Ctx.addEnv "GDK_BACKEND" "wayland"
        . Ctx.addEnv "QT_QPA_PLATFORM" "wayland"
        . Ctx.addEnv "WAYLAND_DISPLAY" (toText skt)
        . Ctx.addMount "/dev/shm" Ctx.tmpfs

setPipewire :: Ctx.Context -> AppEnvT Ctx.Context
setPipewire ctx = do
  let skt = "pipewire-0" -- TODO discover skt name
  shareSkt <- addXdgRun skt
  pure $
    ctx
      & Ctx.directMount "/etc/machine-id"
        . shareSkt

setDbus :: Ctx.Context -> AppEnvT Ctx.Context
setDbus ctx = do
  let skt = "bus" -- TODO discover skt name
  (sktPath, shareSkt) <- addXdgRun' skt
  pure $
    ctx
      & Ctx.directMount "/etc/machine-id"
        . Ctx.addEnv "DBUS_SESSION_BUS_ADDRESS" ("unix:path=" <> toText sktPath)
        . shareSkt

setVideo :: Ctx.Context -> AppEnvT Ctx.Context
setVideo ctx = do
  devices <- liftIO $ listDirectory "/dev"
  let addDevices =
        map (Ctx.addDevice . toString . mappend "/dev/") $
          filter ("video" `Text.isPrefixOf`) $ map toText devices
  pure $ foldr (\c a -> c a) ctx addDevices

setPulseaudio :: Ctx.Context -> AppEnvT Ctx.Context
setPulseaudio ctx = do
  shareSkt <- addXdgRun "pulse"
  pure $
    ctx
      & Ctx.directMount "/etc/machine-id"
        . shareSkt
        . Ctx.addEnv "PULSE_SERVER" "/run/user/pulse/native"

getHomes :: Text -> AppEnvT (FilePath, FilePath)
getHomes help = do
  hostHomeDir <- fromMaybe (error $ "Need HOME for " <> help) <$> asks _hostHomeDir
  appHomeDir <- fromMaybe (error $ "Application need home for " <> help) <$> asks _appHomeDir
  pure (hostHomeDir, appHomeDir)

mountHomeConfig :: Text -> FilePath -> AppEnvT (Ctx.Context -> Ctx.Context)
mountHomeConfig help fp = do
  (hostHomeDir, appHomeDir) <- getHomes help
  pure $ Ctx.addMount (appHomeDir </> fp) (Ctx.roHostPath $ hostHomeDir </> fp)

setAgent :: String -> IO (Ctx.Context -> Ctx.Context)
setAgent var = do
  value <- lookupEnv var
  pure $ case value of
    Nothing -> id
    Just path -> Ctx.addEnv (toText var) (toText path) . Ctx.directMount (takeDirectory path)

setSshControlPath :: AppEnvT (Ctx.Context -> Ctx.Context)
setSshControlPath = do
  config <- readConfig
  uid <- asks _hostUid
  pure $ case getControlPath (words config) of
    Just cp ->
      case takeDirectory $ toString $ Text.replace "%i" (show uid) cp of
        "/tmp" -> id
        d -> Ctx.directMount d
    Nothing -> id
  where
    readConfig = do
      homeDir <- fromMaybe (error "Need HOME for ssh") <$> asks _hostHomeDir
      let configPath = homeDir </> ".ssh" </> "config"
      exist <- liftIO $ doesFileExist configPath
      bool (pure "") (liftIO $ Text.readFile configPath) exist
    getControlPath = \case
      ("ControlPath" : x : _) -> Just x
      (_ : xs) -> getControlPath xs
      [] -> Nothing

setSsh :: Ctx.Context -> AppEnvT Ctx.Context
setSsh ctx = do
  shareConfig <- mountHomeConfig "ssh" ".ssh"
  shareAgent <- liftIO $ setAgent "SSH_AUTH_SOCK"
  shareControlPath <- setSshControlPath
  pure $ ctx & shareAgent . shareConfig . shareControlPath

setGpg :: Ctx.Context -> AppEnvT Ctx.Context
setGpg ctx = do
  shareConfig <- mountHomeConfig "gpg" ".gnupg"
  shareGpg <- addXdgRun "gnupg"
  pure $ ctx & shareGpg . shareConfig

setX11 :: Ctx.Context -> AppEnvT Ctx.Context
setX11 ctx = do
  display <- liftIO $ getEnv "DISPLAY"
  pure $
    ctx
      & Ctx.directMount "/tmp/.X11-unix" . Ctx.addEnv "DISPLAY" (toText display) . Ctx.addMount "/dev/shm" Ctx.tmpfs

setCwd :: Ctx.Context -> AppEnvT Ctx.Context
setCwd ctx = do
  cwd <- asks _hostCwd
  pure $ ctx & Ctx.addMount "/data" (Ctx.rwHostPath cwd) . (Ctx.workdir ?~ "/data")

addVolume :: Ctx.Context -> Text -> AppEnvT Ctx.Context
addVolume ctx volume = do
  containerPath' <- resolveContainerPath containerPath
  hostPath' <- resolveVolume hostPath
  pure $ Ctx.addMount containerPath' hostPath' ctx
  where
    (hostPath, containerPath) = case Text.breakOn ":" volume of
      (x, "") -> (x, x)
      (x, y) -> (x, Text.drop 1 y)

resolveFileArgs :: [Text] -> AppEnvT (Ctx.Context -> Ctx.Context)
resolveFileArgs args = do
  fps <- traverse checkExist args
  pure $ foldr ((.) . addFileArg) id fps
  where
    addCommand :: Text -> Ctx.Context -> Ctx.Context
    addCommand arg = Ctx.command %~ (arg :)
    checkExist :: Text -> AppEnvT (Either Text FilePath)
    checkExist arg = do
      fpM <- resolveHostPath arg
      case fpM of
        Nothing -> pure $ Left arg
        Just fp -> do
          exist <- liftIO $ doesPathExist fp
          unless exist (liftIO $ putTextLn $ "Warning, arg path does not exist: " <> arg)
          pure $ bool (Left arg) (Right fp) exist
    addFileArg :: Either Text FilePath -> Ctx.Context -> Ctx.Context
    addFileArg (Left arg) = addCommand arg
    addFileArg (Right fp)
      | hasTrailingPathSeparator fp = error "Directory filearg ar not supported"
      | otherwise =
        let cfp = "/data" </> takeFileName fp
         in addCommand (toText cfp) . Ctx.addMount cfp (Ctx.rwHostPath fp)

-- | Helper functions to manipulate paths
getXdgRuntimeDir :: AppEnvT FilePath
getXdgRuntimeDir = fromMaybe (error "Need XDG_RUNTIME_DIR") <$> asks _hostXdgRunDir

fixPath :: Text -> FilePath
fixPath = toString . Text.drop 1 . Text.dropWhile (/= '/')

resolveContainerPath :: Text -> AppEnvT FilePath
resolveContainerPath path
  | path == "~" || "~/" `Text.isPrefixOf` path = do
    appHome' <- fromMaybe (error "Need app home") <$> asks _appHomeDir
    pure $ appHome' </> fixPath path
  | "/" `Text.isPrefixOf` path = pure $ toString path
  | otherwise = error $ "Invalid container path: " <> path

resolveHostPath :: Text -> AppEnvT (Maybe FilePath)
resolveHostPath path
  | "~/" `Text.isPrefixOf` path = do
    envHome' <- fromMaybe (error "Need HOME") <$> asks _hostHomeDir
    pure $ Just (envHome' </> fixPath path)
  | "./" `Text.isPrefixOf` path = do
    curDir' <- asks _hostCwd
    pure $ Just (curDir' </> fixPath path)
  | "/" `Text.isPrefixOf` path = pure $ Just (toString path)
  | otherwise = pure Nothing

resolveHostPath' :: Text -> AppEnvT FilePath
resolveHostPath' path = do
  pathM <- resolveHostPath path
  case pathM of
    Just x -> pure x
    Nothing -> error $ "Invalid host path: " <> path

resolveVolume :: Text -> AppEnvT Ctx.Volume
resolveVolume name = case Text.uncons name of
  Nothing -> error "Empty host path"
  Just (x, _xs)
    -- TODO: handle suffix such as :Z
    | x `elem` ['~', '.', '/'] -> Ctx.rwHostPath <$> resolveHostPath' name
    | otherwise -> pure $ Ctx.MkVolume Ctx.RW (Ctx.Volume name)

-- | Helper function to share a XDG_RUNTIME_DIR path
addXdgRun :: FilePath -> AppEnvT (Ctx.Context -> Ctx.Context)
addXdgRun fp = snd <$> addXdgRun' fp

-- | Returns the container path and the context setter for xdg
addXdgRun' :: FilePath -> AppEnvT (FilePath, Ctx.Context -> Ctx.Context)
addXdgRun' fp = do
  hostXdg <- getXdgRuntimeDir
  let containerPath = runDir </> fp
      hostPath = hostXdg </> fp
  pure
    ( containerPath,
      Ctx.addEnv "XDG_RUNTIME_DIR" (toText runDir)
        -- Podman creates parent directory as root, ensure user can r/w xdgdir using tmpfs
        . Ctx.addMount runDir Ctx.tmpfs
        . Ctx.addMount containerPath (Ctx.rwHostPath hostPath)
    )
  where
    runDir = "/run/user"

-- | Helper for capabilities that are directly represented in the context
contextSet :: Lens' Ctx.Context a -> a -> Ctx.Context -> AppEnvT Ctx.Context
contextSet lens value ctx = pure ((lens .~ value) ctx)
