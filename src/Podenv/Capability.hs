{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

-- | This module contains the capability logic.
-- The goal is to convert an Application into a Context
module Podenv.Capability
  ( prepare,
    capsAll,
    Cap (..),
    setNix,
  )
where

import Data.Map qualified
import Data.Set qualified as Set
import Data.Text qualified as Text
import Podenv.Context as Ctx
import Podenv.Dhall
import Podenv.Env
import Podenv.Image
import Podenv.Prelude

-- | Converts an Application into a Context
prepare :: AppMode -> Application -> Ctx.Name -> AppEnvT Ctx.Context
prepare mode app name = do
  hostUID <- asks _hostUid
  let ctx =
        Ctx.defaultContext name
          & (Ctx.uid .~ hostUID)
            . (Ctx.namespace .~ app ^. appNamespace)

  setVolumes <- addVolumes (app ^. appVolumes)

  setCaps <- addCaps (app ^. appCapabilities)

  setHome <- do
    appHome <- asks _appHomeDir
    let ensureHome = case appHome of
          Just fp ->
            let volumeName = fromMaybe (Ctx.unName name) (app ^. appNamespace)
             in Ctx.addMount (toString fp) (Ctx.MkVolume Ctx.RW (Ctx.Volume $ volumeName <> "-home"))
          Nothing -> id
        ensureWorkdir ctx' = case appHome of
          (Just x) | isMounted (toString x) ctx' -> ctx' & Ctx.workdir `setWhenNothing` toString x
          _ -> ctx'
        setHome = case appHome of
          Just x -> Ctx.addEnv "HOME" (toText x)
          _ -> id
        setRunAs = case appHome of
          -- To keep it simple, when the app home is in `/home`, assume we share the host uid.
          Just h | "/home" `Text.isPrefixOf` toText h -> Ctx.runAs `setWhenNothing` Ctx.RunAsHostUID
          _ -> id

    pure $ setHome . ensureWorkdir . ensureHome . setRunAs

  setNixEnv <- case app ^. appRuntime of
    Nix _ -> setNix
    _ -> pure id

  let mkCommand = \extraArgs -> case app ^. appRuntime of
        Nix flakes ->
          [toText nixCommandPath] <> nixFlags <> case app ^. appCommand of
            [] ->
              ["run"] <> nixArgs flakes <> case extraArgs of
                [] -> []
                xs -> ["--"] <> xs
            appArgs -> ["shell"] <> nixArgs flakes <> ["--command"] <> appArgs <> extraArgs
        _ -> (app ^. appCommand) <> extraArgs

  setCommand <- case mode of
    Regular extraArgs
      | app ^. appCapabilities . capHostfile -> resolveFileArgs (mkCommand extraArgs)
      | otherwise -> pure $ Ctx.command .~ mkCommand extraArgs
    Shell -> pure $ Ctx.command .~ ["/bin/sh"]

  pure (disableSelinux . setHome . setCommand . modifiers . setCaps . setVolumes . setNixEnv $ ctx)
  where
    modifiers :: Ctx.Context -> Ctx.Context
    modifiers = addSysCaps . addEnvs

    -- Check if path is part of a mount point
    isMounted :: FilePath -> Ctx.Context -> Bool
    isMounted fp ctx = any isPrefix mountPaths
      where
        mountPaths = Data.Map.keys $ ctx ^. Ctx.mounts
        isPrefix x = fp `isPrefixOf` x

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

    addSysCaps ctx = foldr addSysCap ctx (app ^. appSyscaps)
    addSysCap :: Text -> Ctx.Context -> Ctx.Context
    addSysCap syscap = case readMaybe (toString $ "CAP_" <> syscap) of
      Nothing -> error $ "Can't read syscap: " <> show syscap
      Just c -> Ctx.syscaps %~ Set.insert c

    addEnvs ctx = foldr setEnv ctx (app ^. appEnviron)
    setEnv :: Text -> Ctx.Context -> Ctx.Context
    setEnv env =
      let (k, v) = Text.breakOn "=" env
       in Ctx.addEnv k (Text.drop 1 v)

-- | CapInfo describes a capability and how it modify the runtime context
data Cap = Cap
  { capName :: Text,
    capDescription :: Text,
    -- | How to get the capability value from the user provided record:
    capLens :: Lens' Capabilities Bool,
    -- | How the capability change the context:
    capSet :: AppEnvT (Ctx.Context -> Ctx.Context)
  }

setNix :: AppEnvT (Ctx.Context -> Ctx.Context)
setNix = do
  certs <- toText . fromMaybe (error "Can't find ca-bundle") <$> getCertLocation
  setNixVolumes <- addVolumes ["nix-store:/nix", "nix-cache:~/.cache/nix", "nix-config:~/.config/nix"]
  let setEnv =
        Ctx.addEnv "NIX_SSL_CERT_FILE" certs
          . Ctx.addEnv "TERM" "xterm"
          . Ctx.addEnv "LC_ALL" "C.UTF-8"
          . Ctx.addEnv "PATH" "/nix/var/nix/profiles/nix-install/bin:/bin"

  pure $ setEnv . setNixVolumes

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
    Cap "dri" "share graphic device" capDri setDri,
    Cap "kvm" "share kvm device" capKvm (pure $ Ctx.addDevice "/dev/kvm"),
    Cap "tun" "share tun device" capTun (pure $ Ctx.addDevice "/dev/net/tun"),
    Cap "alsa" "share alsa devices" capAlsa (pure $ Ctx.addDevice "/dev/snd"),
    Cap "pulseaudio" "share pulseaudio socket" capPulseaudio setPulseaudio,
    Cap "ssh" "share ssh agent and keys" capSsh setSsh,
    Cap "gpg" "share gpg agent and keys" capGpg setGpg,
    Cap "x11" "share x11 socket" capX11 setX11,
    Cap "cwd" "mount cwd" capCwd setCwd,
    Cap "network" "enable network" capNetwork (contextSet Ctx.network True),
    Cap "hostfile" "mount command file arg" capHostfile (pure id),
    Cap "rw" "mount rootfs rw" capRw (contextSet Ctx.ro False),
    Cap "privileged" "run with extra privileges" capPrivileged (contextSet Ctx.privileged True)
  ]

setTerminal :: AppEnvT (Ctx.Context -> Ctx.Context)
setTerminal =
  pure $ (Ctx.interactive .~ True) . (Ctx.terminal .~ True) . Ctx.addEnv "TERM" "xterm-256color"

setWayland :: AppEnvT (Ctx.Context -> Ctx.Context)
setWayland = do
  sktM <- asks _hostWaylandSocket
  case sktM of
    Nothing -> setX11
    Just skt -> setWayland' skt

setWayland' :: SocketName -> AppEnvT (Ctx.Context -> Ctx.Context)
setWayland' (SocketName skt) = do
  shareSkt <- addXdgRun skt
  pure $
    Ctx.directMount "/etc/machine-id"
      . shareSkt
      . Ctx.addEnv "GDK_BACKEND" "wayland"
      . Ctx.addEnv "QT_QPA_PLATFORM" "wayland"
      . Ctx.addEnv "WAYLAND_DISPLAY" (toText skt)
      . Ctx.addEnv "XDG_SESSION_TYPE" "wayland"
      . Ctx.addMount "/dev/shm" Ctx.tmpfs

setPipewire :: AppEnvT (Ctx.Context -> Ctx.Context)
setPipewire = do
  let skt = "pipewire-0" -- TODO discover skt name
  shareSkt <- addXdgRun skt
  pure $ Ctx.directMount "/etc/machine-id" . shareSkt

setDbus :: AppEnvT (Ctx.Context -> Ctx.Context)
setDbus = do
  let skt = "bus" -- TODO discover skt name
  (sktPath, shareSkt) <- addXdgRun' skt
  pure $
    Ctx.directMount "/etc/machine-id"
      . Ctx.addEnv "DBUS_SESSION_BUS_ADDRESS" ("unix:path=" <> toText sktPath)
      . shareSkt

setVideo :: AppEnvT (Ctx.Context -> Ctx.Context)
setVideo = do
  deviceList <- getVideoDevices
  let addDevices =
        map (Ctx.addDevice . mappend "/dev/") deviceList
  pure $ foldr (.) id addDevices

setDri :: AppEnvT (Ctx.Context -> Ctx.Context)
setDri = do
  nvidia <- isNVIDIAEnabled
  pure $
    if nvidia
      then Ctx.addDevice "/dev/nvidiactl" . Ctx.addDevice "/dev/nvidia0"
      else Ctx.addDevice "/dev/dri"

setPulseaudio :: AppEnvT (Ctx.Context -> Ctx.Context)
setPulseaudio = do
  shareSkt <- addXdgRun "pulse"
  huid <- asks _hostUid
  let pulseServer = "/run/user/" <> show huid <> "/pulse/native"
  pure $
    Ctx.directMount "/etc/machine-id"
      . shareSkt
      . Ctx.addEnv "PULSE_SERVER" pulseServer

getHomes :: Text -> AppEnvT (FilePath, FilePath)
getHomes help = do
  hostDir <- fromMaybe (error $ "Need HOME for " <> help) <$> asks _hostHomeDir
  appDir <- fromMaybe (error $ "Application need home for " <> help) <$> asks _appHomeDir
  pure (hostDir, appDir)

mountHomeConfig :: Text -> FilePath -> AppEnvT (Ctx.Context -> Ctx.Context)
mountHomeConfig help fp = do
  (hostDir, appDir) <- getHomes help
  pure $ Ctx.addMount (appDir </> fp) (Ctx.roHostPath $ hostDir </> fp)

setAgent :: String -> Maybe String -> AppEnvT (Ctx.Context -> Ctx.Context)
setAgent var value = do
  pure $ case value of
    Nothing -> id
    Just path -> Ctx.addEnv (toText var) (toText path) . Ctx.directMount (takeDirectory path)

setSsh :: AppEnvT (Ctx.Context -> Ctx.Context)
setSsh = setAgent "SSH_AUTH_SOCK" =<< asks _hostSSHAgent

setGpg :: AppEnvT (Ctx.Context -> Ctx.Context)
setGpg = do
  shareConfig <- mountHomeConfig "gpg" ".gnupg"
  shareGpg <- addXdgRun "gnupg"
  pure $ shareGpg . shareConfig

setX11 :: AppEnvT (Ctx.Context -> Ctx.Context)
setX11 = do
  display <- asks _hostDisplay
  pure $
    Ctx.directMount "/tmp/.X11-unix" . Ctx.addEnv "DISPLAY" (toText display) . Ctx.addMount "/dev/shm" Ctx.tmpfs

setCwd :: AppEnvT (Ctx.Context -> Ctx.Context)
setCwd = do
  cwd <- asks _hostCwd
  pure $ Ctx.addMount "/data" (Ctx.rwHostPath cwd) . (Ctx.workdir ?~ "/data")

addVolumes :: [Text] -> AppEnvT (Ctx.Context -> Ctx.Context)
addVolumes volumes = do
  ops <- traverse addVolume volumes
  pure $ foldr (>>>) id ops

addVolume :: Text -> AppEnvT (Ctx.Context -> Ctx.Context)
addVolume volume = do
  containerPath' <- resolveContainerPath containerPath
  hostPath' <- resolveVolume hostPath
  pure $ Ctx.addMount containerPath' hostPath'
  where
    (hostPath, containerPath) = case Text.breakOn ":" volume of
      (x, "") -> (x, x)
      (x, y) -> (x, Text.drop 1 y)

addCaps :: Capabilities -> AppEnvT (Ctx.Context -> Ctx.Context)
addCaps appCaps = do
  caps <- traverse (addCap appCaps) capsAll
  pure $ foldr (.) id caps

addCap :: Capabilities -> Cap -> AppEnvT (Ctx.Context -> Ctx.Context)
addCap appCaps Cap {..}
  | appCaps ^. capLens = capSet
  | otherwise = pure id

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
        Just fp -> pure $ Right fp

    addFileArg :: Either Text FilePath -> Ctx.Context -> Ctx.Context
    addFileArg (Left arg) = addCommand arg
    addFileArg (Right fp)
      | hasTrailingPathSeparator fp = error "Directory filearg are not supported"
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
  huid <- asks _hostUid
  let containerPath = runDir </> fp
      hostPath = hostXdg </> fp
      runBaseDir = "/run/user"
      runDir = runBaseDir <> "/" <> show huid
  pure
    ( containerPath,
      Ctx.addEnv "XDG_RUNTIME_DIR" (toText runDir)
        -- Podman creates parent directory as root, ensure user can r/w xdgdir using tmpfs
        . Ctx.addMount runBaseDir Ctx.tmpfs
        . Ctx.addMount containerPath (Ctx.rwHostPath hostPath)
    )

-- | Helper for capabilities that are directly represented in the context
contextSet :: Lens' Ctx.Context a -> a -> AppEnvT (Ctx.Context -> Ctx.Context)
contextSet lens value = pure (lens .~ value)