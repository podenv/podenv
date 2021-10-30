{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | This module contains the podman/kubectl context wrapper
module Podenv.Runtime
  ( execute,

    -- * Podman helpers
    podman,
    podmanRunArgs,
    showPodmanCmd,

    -- * data type and lenses
    module Podenv.Context,
    RuntimeEnv (..),
    defaultRuntimeEnv,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Podenv.Config (defaultSystemConfig)
import Podenv.Context
import Podenv.Dhall (SystemConfig (..), sysDns)
import Podenv.Prelude
import System.IO (hPutStr)
import qualified System.Process.Typed as P

execute :: RuntimeEnv -> Context -> IO ()
execute re ctx = runReaderT (doExecute ctx) re

doExecute :: Context -> ContextEnvT ()
doExecute = executePodman

showPodmanCmd :: RuntimeEnv -> Context -> Text
showPodmanCmd re = show . P.proc "podman" . podmanRunArgs re

data RuntimeEnv = RuntimeEnv
  { verbose :: Bool,
    system :: SystemConfig
  }
  deriving (Show)

defaultRuntimeEnv :: RuntimeEnv
defaultRuntimeEnv = RuntimeEnv True defaultSystemConfig

type ContextEnvT a = ReaderT RuntimeEnv IO a

debug :: Text -> ContextEnvT ()
debug msg = do
  isVerbose <- asks verbose
  when isVerbose $ liftIO $ hPutStr stderr ("[+] " <> toString msg <> "\n")

cond :: Bool -> [a] -> [a]
cond b xs = if b then xs else []

infraName :: Text -> Text
infraName ns = ns <> "-ns"

podmanArgs :: Context -> [Text]
podmanArgs Context {..} = cond _interactive ["-i", "--detach-keys", ""] <> cond _terminal ["-t"]

podmanRunArgs :: RuntimeEnv -> Context -> [String]
podmanRunArgs RuntimeEnv {..} ctx@Context {..} = toString <$> args
  where
    portArgs = concatMap publishArg _ports
    publishArg port = ["--publish", showPort port]
    showPort port = show $ case port of
      -- podman does not seem to distinguish protocol
      PortTcp p -> p
      PortUdp p -> p

    hostnameArg = ["--hostname", _name]
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
      Volume x -> volume x
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

    args =
      ["run"]
        <> podmanArgs ctx
        <> maybe [] (\h -> ["--hostname", h]) _hostname
        <> cond (_detach) ["--detach"]
        <> cond (not _keep) ["--rm"]
        <> cond (not _selinux) ["--security-opt", "label=disable"]
        <> userArg
        <> networkArg
        <> concatMap (\c -> ["--cap-add", Text.drop 4 (show c)]) _syscaps
        <> concatMap (\d -> ["--device", toText d]) _devices
        <> maybe [] (\wd -> ["--workdir", toText wd]) _workdir
        <> concatMap (\(k, v) -> ["--env", toText $ k <> "=" <> v]) (Map.toAscList _environ)
        <> concatMap volumeArg (Map.toAscList _mounts)
        <> cond (_name /= mempty) ["--name", _name]
        <> [unImageName _image]
        <> _command

podmanExecArgs :: Context -> [String]
podmanExecArgs ctx = toString <$> args
  where
    args = ["exec"] <> podmanArgs ctx <> [ctx ^. name] <> cmd
    cmd = case ctx ^. command of
      [] -> ["/bin/bash"]
      x -> x

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

executePodman :: Context -> ContextEnvT ()
executePodman ctx = do
  re <- ask
  case (ctx ^. namespace, ctx ^. network) of
    (Just ns, True) | ns /= mempty -> ensureInfraNet ns
    _ -> pure ()

  status <- getPodmanStatus cname
  debug $ "Podman status of " <> toText cname <> ": " <> show status
  let cfail err = liftIO . mayFail . Left $ toText cname <> ": " <> err
  args <-
    if ctx ^. keep
      then case status of
        NotFound -> createContainer re
        Running -> pure $ podmanExecArgs ctx
        Unknown "configured" -> startContainer
        Unknown "exited" -> startContainer
        Unknown cstate -> cfail $ "unknown container status: " <> cstate
      else case status of
        NotFound -> pure $ podmanRunArgs re ctx
        Running -> cfail "container is already running, use `exec` to join, or `--name new` to start a new instance"
        Unknown _ -> recreateContainer re
  let cmd = podman args
  debug $ show cmd
  P.runProcess_ cmd
  where
    cname = toString (ctx ^. name)
    -- Create a kept container with sleep and return the exec arg
    createContainer re = do
      let infraCtx = (detach .~ True) . (command .~ ["sleep", "infinity"])
          infraCmd = podman $ podmanRunArgs re (infraCtx ctx)
      debug $ "Starting infra container: " <> show infraCmd
      P.runProcess_ $ infraCmd
      pure $ podmanExecArgs ctx

    -- Start the kept container and return the exec arg
    startContainer = do
      P.runProcess_ (podman ["start", cname])
      pure $ podmanExecArgs ctx

    -- Delete a non-kept container and return the run args
    recreateContainer re = do
      P.runProcess_ (podman ["rm", cname])
      pure $ podmanRunArgs re ctx
