{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- makeLenses does not produce those
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

-- | This module defines Haskell data types and lenses for the podenv dhall schemas.
module Podenv.Dhall.TH where

import Data.Either.Validation
import Data.Void
import Dhall qualified
import Dhall.Core (Expr ())
import Dhall.Core qualified as Dhall
import Dhall.TH qualified
import Lens.Family.TH (makeLensesBy)
import Podenv.Prelude

-- | Embed static dhall code.
podenvPackage :: Expr Void Void
podenvPackage = $(Dhall.TH.staticDhallExpression "./hub/package.dhall")

appType, appDefault, runtimeType, containerBuildDefault, capsDefault :: Expr Void Void
appType = $(Dhall.TH.staticDhallExpression "(./hub/schemas/Application.dhall).Type")
runtimeType = $(Dhall.TH.staticDhallExpression "./hub/schemas/Runtime.dhall")
containerBuildDefault = $(Dhall.TH.staticDhallExpression "(./hub/schemas/ContainerBuild.dhall).default")
appDefault = $(Dhall.TH.staticDhallExpression "(./hub/schemas/Application.dhall).default")
capsDefault = $(Dhall.TH.staticDhallExpression "(./hub/schemas/Capabilities.dhall).default")

{- | Generate Haskell Types from Dhall Types.
See: https://hackage.haskell.org/package/dhall-1.40.0/docs/Dhall-TH.html
-}
Dhall.TH.makeHaskellTypes
    ( let mainPath name = "(./hub/schemas/" <> name <> ".dhall).Type"
          main' cname name = Dhall.TH.SingleConstructor cname cname $ mainPath name
          main name = main' name name
       in [ main "Capabilities"
          , main "Application"
          , main "ApplicationResource"
          , main "ContainerBuild"
          , Dhall.TH.SingleConstructor "LabelKV" "LabelKV" "{mapKey : Text, mapValue : Text}"
          , main "Metadata"
          , Dhall.TH.MultipleConstructors "Runtime" "./hub/schemas/Runtime.dhall"
          , Dhall.TH.MultipleConstructors "Network" "./hub/schemas/Network.dhall"
          ]
    )

$(makeLensesBy (lensName "cap") ''Capabilities)

$(makeLensesBy (lensName "app") ''Application)

$(makeLensesBy (lensName "ar") ''ApplicationResource)

$(makeLensesBy (lensName "meta") ''Metadata)

$(makeLensesBy (lensName "cb") ''ContainerBuild)

deriving instance Show Runtime

deriving instance Eq Runtime

deriving instance Show ContainerBuild

deriving instance Eq ContainerBuild

deriving instance Show Capabilities

deriving instance Eq Capabilities

deriving instance Show Application

deriving instance Eq Application

deriving instance Show LabelKV

deriving instance Eq LabelKV

deriving instance Show Metadata

deriving instance Eq Metadata

deriving instance Show ApplicationResource

deriving instance Eq ApplicationResource

deriving instance Show Network

deriving instance Eq Network

extractDhallDefault :: (HasCallStack, Dhall.FromDhall a) => Expr Void Void -> a
extractDhallDefault def = case Dhall.extract Dhall.auto (Dhall.renote def) of
    Success app -> app
    Failure v -> error $ "Invalid defaults: " <> show v
