{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
-- makeLenses does not produce those
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

-- | This module defines Haskell data types and lenses for the podenv dhall schemas.
module Podenv.Dhall where

import Data.Char (toUpper)
import Data.Text ()
import qualified Dhall.TH
import Lens.Family.TH (makeLensesBy)

-- | When updating the schemas, bump the version and branch the hub
version :: Word
version = 1

-- | Generate Haskell Types from Dhall Types.
-- See: https://hackage.haskell.org/package/dhall-1.39.0/docs/Dhall-TH.html
Dhall.TH.makeHaskellTypes
  ( let mainPath name = "(./hub/schemas/" <> name <> ".dhall).Type"
        main' cname name = Dhall.TH.SingleConstructor cname cname $ mainPath name
        main name = main' name name
     in [ main "Capabilities",
          main "Application",
          main "ContainerBuild",
          main' "SystemConfig" "System",
          Dhall.TH.MultipleConstructors "Provider" "./hub/schemas/Provider.dhall",
          Dhall.TH.MultipleConstructors "Runtime" "./hub/schemas/Runtime.dhall"
        ]
  )

$(makeLensesBy (\n -> Just $ "cap" <> [toUpper $ head n] <> tail n) ''Capabilities)

$(makeLensesBy (\n -> Just $ "app" <> [toUpper $ head n] <> tail n) ''Application)

$(makeLensesBy (\n -> Just $ "cb" <> [toUpper $ head n] <> tail n) ''ContainerBuild)

deriving instance Show Runtime

deriving instance Eq Runtime

deriving instance Show Provider

deriving instance Eq Provider

deriving instance Show ContainerBuild

deriving instance Eq ContainerBuild

deriving instance Show Capabilities

deriving instance Eq Capabilities

deriving instance Show Application

deriving instance Eq Application

$(makeLensesBy (\n -> Just $ "sys" <> [toUpper $ head n] <> tail n) ''SystemConfig)

deriving instance Show SystemConfig

deriving instance Eq SystemConfig
