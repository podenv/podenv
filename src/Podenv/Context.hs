{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
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
module Podenv.Context where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Lens.Family.TH (makeLenses)
import Podenv.Prelude
import System.Linux.Capabilities (Capability)

newtype ImageName = ImageName {unImageName :: Text}
  deriving (Show)

data Network = None | Private | Host
  deriving (Show)

data Mode = RO | RW
  deriving (Show)

data VolumeType = HostPath FilePath | TmpFS | Volume Text
  deriving (Show)

data Volume = MkVolume Mode VolumeType
  deriving (Show)

data RunAs = RunAsRoot | RunAsHostUID | RunAsAnyUID
  deriving (Show)

data Port = PortTcp Natural | PortUdp Natural
  deriving (Show)

-- | The application context to be executed by podman or kubectl
data Context = Context
  { -- | identifier
    _name :: Text,
    _namespace :: Maybe Text,
    -- | container image name
    _image :: ImageName,
    -- | network namespace name
    _network :: Network,
    _ports :: [Port],
    _runAs :: Maybe RunAs,
    _selinux :: Bool,
    -- | the unique uid for this container
    _anyUid :: UserID,
    -- | host uid
    _uid :: UserID,
    -- | container command
    _command :: [Text],
    _workdir :: Maybe FilePath,
    -- | container env
    _environ :: Map Text Text,
    -- | container volumes
    _mounts :: Map FilePath Volume,
    _syscaps :: [Capability],
    -- | container devices
    _devices :: Set FilePath,
    _hostname :: Maybe Text,
    _interactive :: Bool,
    _terminal :: Bool,
    _keep :: Bool
  }
  deriving (Show)

$(makeLenses ''Context)

defaultContext :: Text -> ImageName -> Context
defaultContext _name _image =
  Context
    { _name,
      _image,
      _command = [],
      _uid = 0,
      _namespace = Nothing,
      -- todo keep track of fresh uid
      _anyUid = 4242,
      _selinux = True,
      _network = None,
      _ports = mempty,
      _runAs = Nothing,
      _environ = mempty,
      _mounts = mempty,
      _devices = mempty,
      _syscaps = mempty,
      _workdir = Nothing,
      _hostname = Nothing,
      _interactive = False,
      _terminal = False,
      _keep = False
    }

rwHostPath :: FilePath -> Volume
rwHostPath = MkVolume RW . HostPath

roHostPath :: FilePath -> Volume
roHostPath = MkVolume RO . HostPath

tmpfs :: Volume
tmpfs = MkVolume RW TmpFS

-- Env and mounts head value takes priority
addEnv :: Text -> Text -> Context -> Context
addEnv k v = environ %~ Map.insertWith (\_n o -> o) k v

addMount :: FilePath -> Volume -> Context -> Context
addMount containerPath hostPath = mounts %~ Map.insertWith (\_n o -> o) containerPath hostPath

directMount :: FilePath -> Context -> Context
directMount fp = addMount fp (rwHostPath fp)

addDevice :: FilePath -> Context -> Context
addDevice dev = devices %~ Set.insert dev
