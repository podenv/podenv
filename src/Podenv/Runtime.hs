{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | This module contains the podman/bubblewrap context wrapper
module Podenv.Runtime
  ( execute,
    showRuntimeCmd,

    -- * Podman helpers
    podman,
    podmanRunArgs,

    -- * Bubblewrap helpers
    bwrap,
    bwrapRunArgs,

    -- * data type and lenses
    module Podenv.Context,
    RuntimeEnv (..),
    defaultRuntimeEnv,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Set
import qualified Data.Text as Text
import Podenv.Config (defaultSystemConfig)
import Podenv.Context
import Podenv.Dhall (SystemConfig (..), sysDns)
import Podenv.Prelude
import qualified System.Process.Typed as P

execute :: RuntimeEnv -> Context -> IO ()
execute re ctx = do
  traverse_ (ensureHostDirectory (volumesDir re)) (Map.elems $ ctx ^. mounts)
  runReaderT (doExecute ctx) re

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
    P.runProcess_ $ P.proc "chcon" ["system_u:object_r:container_file_t:s0", fp]

doExecute :: Context -> ContextEnvT ()
doExecute ctx = case ctx ^. runtimeCtx of
  Container image -> executePodman ctx image
  Bubblewrap fp -> executeBubblewrap ctx fp

executeBubblewrap :: Context -> FilePath -> ContextEnvT ()
executeBubblewrap ctx fp = do
  re <- ask
  let args = bwrapRunArgs re ctx fp
  let cmd = bwrap args
  debug $ show cmd
  P.runProcess_ cmd

bwrap :: [String] -> P.ProcessConfig () () ()
bwrap = P.setDelegateCtlc True . P.proc "bwrap"

commonArgs :: Context -> [Text]
commonArgs Context {..} =
  concatMap (\c -> ["--cap-add", Text.drop 4 (show c)]) _syscaps

bwrapRunArgs :: RuntimeEnv -> Context -> FilePath -> [String]
bwrapRunArgs RuntimeEnv {..} ctx@Context {..} fp = toString <$> args
  where
    userArg = case ctx ^. runAs of
      Just RunAsRoot -> ["--unshare-user", "--uid", "0"]
      Just RunAsHostUID -> []
      Just RunAsAnyUID -> ["--unshare-user", "--uid", show $ ctx ^. anyUid]
      Nothing -> []

    networkArg
      | _network = case _namespace of
        Just "host" -> []
        Just _ns -> error "Shared netns not implemented"
        Nothing -> [] -- TODO: implement private network namespace
      | otherwise = ["--unshare-net"]

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
      | Data.Set.null _devices = []
      | otherwise = ["--ro-bind", "/sys", "/sys"]

    bindMode
      | ctx ^. ro = "--ro-bind"
      | otherwise = "--bind"
    doBind p = toText <$> [bindMode, fp </> p, "/" </> p]
    args =
      userArg
        <> ["--die-with-parent", "--unshare-pid", "--unshare-ipc", "--unshare-uts"]
        <> networkArg
        <> commonArgs ctx
        <> rootMounts
        <> ["--proc", "/proc"]
        <> ["--dev", "/dev"]
        <> ["--perms", "01777", "--tmpfs", "/tmp"]
        <> concatMap volumeArg (Map.toAscList _mounts)
        <> concatMap (\d -> ["--dev-bind", toText d, toText d]) _devices
        <> sysMounts
        <> ["--clearenv"]
        <> concatMap (\(k, v) -> ["--setenv", toText k, v]) (Map.toAscList _environ)
        <> cond (not _terminal) ["--new-session"]
        <> maybe [] (\wd -> ["--chdir", toText wd]) _workdir
        <> _command

showRuntimeCmd :: RuntimeEnv -> Context -> Text
showRuntimeCmd re ctx = case ctx ^. runtimeCtx of
  Container image -> show . P.proc "podman" $ podmanRunArgs re ctx image
  Bubblewrap fp -> show . P.proc "bwrap" $ bwrapRunArgs re ctx fp

data RuntimeEnv = RuntimeEnv
  { verbose :: Bool,
    system :: SystemConfig,
    volumesDir :: FilePath
  }
  deriving (Show)

defaultRuntimeEnv :: FilePath -> RuntimeEnv
defaultRuntimeEnv = RuntimeEnv True defaultSystemConfig

type ContextEnvT a = ReaderT RuntimeEnv IO a

debug :: Text -> ContextEnvT ()
debug msg = do
  isVerbose <- asks verbose
  when isVerbose $ liftIO $ hPutStrLn stderr ("[+] " <> toString msg)

cond :: Bool -> [a] -> [a]
cond b xs = if b then xs else []

infraName :: Text -> Text
infraName ns = ns <> "-ns"

podmanArgs :: Context -> [Text]
podmanArgs Context {..} = cond _interactive ["-i", "--detach-keys", ""] <> cond _terminal ["-t"]

podmanRunArgs :: RuntimeEnv -> Context -> ImageName -> [String]
podmanRunArgs RuntimeEnv {..} ctx@Context {..} image = toString <$> args
  where
    portArgs = concatMap publishArg _ports
    publishArg port = ["--publish", showPort port]
    showPort port = show $ case port of
      -- podman does not seem to distinguish protocol
      PortTcp p -> p
      PortUdp p -> p

    hostnameArg = ["--hostname", unName _name]
    networkArg
      | _network =
        hostnameArg <> case _namespace of
          Just "host" -> ["--network", "host"]
          Just ns -> ["--network", "container:" <> infraName ns]
          Nothing -> maybe [] (\dns -> ["--dns=" <> dns]) (system ^. sysDns) <> portArgs
      | otherwise = ["--network", "none"]

    volumeArg :: (FilePath, Volume) -> [Text]
    volumeArg (fp, MkVolume mode vtype) = case vtype of
      HostPath x -> volume (toText x)
      Volume x -> volume (toText $ volumesDir </> toString x)
      TmpFS -> ["--mount", "type=tmpfs,destination=" <> toText fp]
      where
        volume hp = ["--volume", hp <> ":" <> toText fp <> showVolumeMode mode]
        showVolumeMode = \case
          RO -> ":ro"
          RW -> ""

    -- The goal here is to ensure host files created by the container are readable by the host user.
    userArg = case ctx ^. runAs of
      Just RunAsRoot -> ["--user", "0"]
      Just RunAsHostUID -> ["--user", show (ctx ^. uid), "--userns", "keep-id"]
      Just RunAsAnyUID ->
        let x = ctx ^. anyUid
         in ["--user", show x, "--uidmap", show x <> ":0:1", "--uidmap", "0:1:" <> show x]
      Nothing -> []

    nameArg = ["--name", unName _name]

    args =
      ["run"]
        <> podmanArgs ctx
        <> maybe [] (\h -> ["--hostname", h]) _hostname
        <> cond _privileged ["--privileged"]
        <> ["--rm"]
        <> cond _ro ["--read-only=true"]
        <> cond (not _selinux) ["--security-opt", "label=disable"]
        <> userArg
        <> networkArg
        <> commonArgs ctx
        <> concatMap (\d -> ["--device", toText d]) _devices
        <> maybe [] (\wd -> ["--workdir", toText wd]) _workdir
        <> concatMap (\(k, v) -> ["--env", toText $ k <> "=" <> v]) (Map.toAscList _environ)
        <> concatMap volumeArg (Map.toAscList _mounts)
        <> nameArg
        <> [unImageName image]
        <> _command

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

getPodmanStatus :: String -> ContextEnvT PodmanStatus
getPodmanStatus cname = do
  (_, stdout', _) <- P.readProcess (podman ["inspect", cname, "--format", "{{.State.Status}}"])
  pure $ case stdout' of
    "" -> NotFound
    "running\n" -> Running
    other -> Unknown (Text.dropWhileEnd (== '\n') $ decodeUtf8 other)

ensureInfraNet :: Text -> ContextEnvT ()
ensureInfraNet ns = do
  debug $ "Ensuring infra net for: " <> show ns
  let pod = infraName ns
  infraStatus <- getPodmanStatus (toString pod)
  case infraStatus of
    Running -> pure ()
    _ -> do
      when (infraStatus /= NotFound) $
        -- Try to delete any left-over infra container
        P.runProcess_ (podman ["rm", toString pod])

      system' <- asks system
      let cmd =
            podman $
              map toString $
                ["run", "--rm", "--name", pod]
                  <> ["--detach"]
                  <> maybe [] (\dns -> ["--dns=" <> dns]) (system' ^. sysDns)
                  <> ["ubi8"]
                  <> ["sleep", "infinity"]
      debug $ show cmd
      P.runProcess_ cmd

executePodman :: Context -> ImageName -> ContextEnvT ()
executePodman ctx image = do
  re <- ask
  case (ctx ^. namespace, ctx ^. network) of
    (Just ns, True) | ns /= mempty -> ensureInfraNet ns
    _ -> pure ()

  status <- getPodmanStatus cname
  debug $ "Podman status of " <> toText cname <> ": " <> show status
  let cfail err = liftIO . mayFail . Left $ toText cname <> ": " <> err
  args <-
    case status of
      NotFound -> pure $ podmanRunArgs re ctx image
      Running -> cfail "container is already running, use `exec` to join, or `--name new` to start a new instance"
      Unknown _ -> recreateContainer re
  let cmd = podman args
  debug $ show cmd
  P.runProcess_ cmd
  where
    cname = toString (unName $ ctx ^. name)
    -- Delete a non-kept container and return the run args
    recreateContainer re = do
      P.runProcess_ (podman ["rm", cname])
      pure $ podmanRunArgs re ctx image
