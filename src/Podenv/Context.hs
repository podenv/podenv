{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
-- makeLenses does not produce those
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

-- | Runtime Context data types and lenses
module Podenv.Context (
    DirMode (..),
    Name (..),
    VolumeType (..),
    Volume (..),
    RunAs (..),
    Context,

    -- * Helpers
    defaultContext,
    addMount,
    addEnv,
    addDevice,
    directMount,
    tmpfs,
    roHostPath,
    rwHostPath,

    -- * Lenses
    ctxName,
    ctxLabels,
    ctxHostname,
    ctxNamespace,
    ctxWorkdir,
    ctxRunAs,
    ctxCommand,
    ctxMounts,
    ctxDevices,
    ctxAllDevices,
    ctxSELinux,
    ctxSyscaps,
    ctxInteractive,
    ctxNetwork,
    ctxHostIPC,
    ctxRO,
    ctxPrivileged,
    ctxTerminal,
    ctxEnviron,
    ctxAnyUid,
    ctxRuntime,
) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Lens.Family.TH (makeLenses)
import Podenv.Dhall (Network, Runtime)
import Podenv.Prelude
import System.Linux.Capabilities (Capability)

data DirMode = RO | RW
    deriving (Show, Eq)

data VolumeType = HostPath FilePath | TmpFS | Volume Text
    deriving (Show, Eq)

data Volume = MkVolume DirMode VolumeType
    deriving (Show, Eq)

data RunAs = RunAsRoot | RunAsUID UserID | RunAsAnyUID
    deriving (Show, Eq)

newtype Name = Name {unName :: Text}
    deriving (Show, Eq)

-- | The application context to be executed by podman or kubectl
data Context = Context
    { _ctxName :: Maybe Name
    , _ctxNamespace :: Maybe Text
    , _ctxLabels :: Map Text Text
    , _ctxHostname :: Maybe Text
    -- ^ network namespace name
    , _ctxNetwork :: Maybe Network
    , _ctxRunAs :: Maybe RunAs
    , _ctxSELinux :: Bool
    , _ctxAnyUid :: UserID
    -- ^ the unique uid for this container
    , _ctxHostIPC :: Bool
    , _ctxCommand :: [Text]
    -- ^ container command
    , _ctxWorkdir :: Maybe FilePath
    , _ctxEnviron :: Map Text Text
    -- ^ container env
    , _ctxMounts :: Map FilePath Volume
    -- ^ container volumes
    , _ctxSyscaps :: Set Capability
    , _ctxRO :: Bool
    , _ctxDevices :: Set FilePath
    , _ctxAllDevices :: Bool
    -- ^ container devices
    , _ctxInteractive :: Bool
    , _ctxTerminal :: Bool
    , _ctxPrivileged :: Bool
    , _ctxRuntime :: Runtime
    }
    deriving (Show, Eq)

$(makeLenses ''Context)

defaultContext :: Runtime -> Context
defaultContext _ctxRuntime =
    Context
        { _ctxName = Nothing
        , _ctxCommand = []
        , _ctxNamespace = Nothing
        , _ctxLabels = mempty
        , -- todo keep track of fresh uid
          _ctxAnyUid = 4242
        , _ctxSELinux = True
        , _ctxHostname = Nothing
        , _ctxNetwork = Nothing
        , _ctxRunAs = Nothing
        , _ctxEnviron = mempty
        , _ctxMounts = mempty
        , _ctxDevices = mempty
        , _ctxSyscaps = mempty
        , _ctxAllDevices = False
        , _ctxHostIPC = False
        , _ctxRO = True
        , _ctxWorkdir = Nothing
        , _ctxInteractive = False
        , _ctxTerminal = False
        , _ctxPrivileged = False
        , _ctxRuntime
        }

rwHostPath :: FilePath -> Volume
rwHostPath = MkVolume RW . HostPath

roHostPath :: FilePath -> Volume
roHostPath = MkVolume RO . HostPath

tmpfs :: Volume
tmpfs = MkVolume RW TmpFS

-- Env and mounts head value takes priority
addEnv :: Text -> Text -> Context -> Context
addEnv k v = ctxEnviron %~ Map.insertWith (\_n o -> o) k v

addMount :: FilePath -> Volume -> Context -> Context
addMount containerPath hostPath = ctxMounts %~ Map.insertWith (\_n o -> o) containerPath hostPath

directMount :: FilePath -> Context -> Context
directMount fp = addMount fp (rwHostPath fp)

addDevice :: FilePath -> Context -> Context
addDevice dev = ctxDevices %~ Set.insert dev
