{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Podenv.Image (
    ImageName (..),
    Containerfile (..),
    mkImageName,
    nixCommandProfile,
    nixCommandPath,
    nixArgs,
) where

import Data.Digest.Pure.SHA qualified as SHA
import Podenv.Dhall
import Podenv.Prelude

newtype ImageName = ImageName {unImageName :: Text}
    deriving (Show, Eq)

newtype Containerfile = Containerfile Text
    deriving (Show, Eq)

mkImageName :: ContainerBuild -> ImageName
mkImageName containerBuild = ImageName $ "localhost/" <> name
  where
    -- The image name can be set by the container build,
    -- otherwise it default to the Containerfile hash
    name = fromMaybe imageHash (containerBuild ^. cbImage_name)
    imageHash = toText . SHA.showDigest . SHA.sha256 . encodeUtf8 $ containerBuild ^. cbContainerfile

nixCommandProfile, nixCommandPath :: FilePath
nixCommandProfile = "var/nix/profiles/nix-install"
nixCommandPath = "/nix/" <> nixCommandProfile <> "/bin/nix"

nixFlags :: [Text]
nixFlags = ["--extra-experimental-features", "nix-command flakes"]

nixArgs :: Text -> [Text]
nixArgs installable = impureArgs <> nixFlags <> [installable]
  where
    -- impure is necessary for nixGL to discover the nvidia driver
    impureArgs = ["--impure"]
