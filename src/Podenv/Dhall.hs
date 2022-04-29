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
import Data.Void
import Dhall.Core (Expr ())
import qualified Dhall.TH
import Lens.Family.TH (makeLensesBy)

-- | The hub submodule commit, this is only used for the PODENV environment value
hubCommit :: Expr Void Void
hubCommit = $(Dhall.TH.staticDhallExpression "env:HUB_COMMIT as Text ? ./.git/modules/hub/HEAD as Text")

-- | Embed static dhall code
podenvPackage :: Expr Void Void
podenvPackage = $(Dhall.TH.staticDhallExpression "./hub/package.dhall")

appType, appDefault, runtimeType, containerBuildDefault :: Expr Void Void
appType = $(Dhall.TH.staticDhallExpression "(./hub/schemas/Application.dhall).Type")
runtimeType = $(Dhall.TH.staticDhallExpression "./hub/schemas/Runtime.dhall")
containerBuildDefault = $(Dhall.TH.staticDhallExpression "(./hub/schemas/ContainerBuild.dhall).default")
appDefault =
  $( let package = "(./hub/schemas/package.dhall)"
      in Dhall.TH.staticDhallExpression (package <> ".Application.default // { runtime = " <> package <> ".Image \"\" }")
   )

capsDefault :: Expr Void Void
capsDefault = $(Dhall.TH.staticDhallExpression "(./hub/schemas/Capabilities.dhall).default")

systemConfigDefault :: Expr Void Void
systemConfigDefault = $(Dhall.TH.staticDhallExpression "(./hub/schemas/System.dhall).default")

-- | Generate Haskell Types from Dhall Types.
-- See: https://hackage.haskell.org/package/dhall-1.39.0/docs/Dhall-TH.html
Dhall.TH.makeHaskellTypes
  ( let mainPath name = "(./hub/schemas/" <> name <> ".dhall).Type"
        main' cname name = Dhall.TH.SingleConstructor cname cname $ mainPath name
        main name = main' name name
     in [ main "Capabilities",
          main "Application",
          main "ContainerBuild",
          main "Flakes",
          main' "SystemConfig" "System",
          Dhall.TH.MultipleConstructors "Runtime" "./hub/schemas/Runtime.dhall"
        ]
  )

$(makeLensesBy (\n -> Just $ "cap" <> [toUpper $ head n] <> tail n) ''Capabilities)

$(makeLensesBy (\n -> Just $ "app" <> [toUpper $ head n] <> tail n) ''Application)

$(makeLensesBy (\n -> Just $ "cb" <> [toUpper $ head n] <> tail n) ''ContainerBuild)

deriving instance Show Runtime

deriving instance Eq Runtime

deriving instance Show ContainerBuild

deriving instance Eq ContainerBuild

deriving instance Show Flakes

deriving instance Eq Flakes

deriving instance Show Capabilities

deriving instance Eq Capabilities

deriving instance Show Application

deriving instance Eq Application

$(makeLensesBy (\n -> Just $ "sys" <> [toUpper $ head n] <> tail n) ''SystemConfig)

deriving instance Show SystemConfig

deriving instance Eq SystemConfig
