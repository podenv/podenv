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
module Podenv.Context where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Lens.Family.TH (makeLenses)
import Podenv.Prelude
import System.Linux.Capabilities (Capability)

data DirMode = RO | RW
  deriving (Show, Eq)

data VolumeType = HostPath FilePath | TmpFS | Volume Text
  deriving (Show, Eq)

data Volume = MkVolume DirMode VolumeType
  deriving (Show, Eq)

data RunAs = RunAsRoot | RunAsHostUID | RunAsAnyUID
  deriving (Show, Eq)

data Port = PortTcp Natural | PortUdp Natural
  deriving (Show, Eq)

newtype Name = Name {unName :: Text}
  deriving (Show, Eq)

-- | The application context to be executed by podman or kubectl
data Context = Context
  { -- | identifier
    _name :: Name,
    _namespace :: Maybe Text,
    -- | network namespace name
    _network :: Bool,
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
    _syscaps :: Set.Set Capability,
    _ro :: Bool,
    -- | container devices
    _devices :: Set FilePath,
    _hostname :: Maybe Text,
    _interactive :: Bool,
    _terminal :: Bool,
    _privileged :: Bool
  }
  deriving (Show, Eq)

$(makeLenses ''Context)

defaultContext :: Name -> Context
defaultContext _name =
  Context
    { _name,
      _command = [],
      _uid = 0,
      _namespace = Nothing,
      -- todo keep track of fresh uid
      _anyUid = 4242,
      _selinux = True,
      _network = False,
      _ports = mempty,
      _runAs = Nothing,
      _environ = mempty,
      _mounts = mempty,
      _devices = mempty,
      _syscaps = mempty,
      -- TODO: make ro work for podman
      _ro = True,
      _workdir = Nothing,
      _hostname = Nothing,
      _interactive = False,
      _terminal = False,
      _privileged = False
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
