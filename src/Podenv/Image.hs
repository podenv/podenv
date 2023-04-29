{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Podenv.Image (
    ImageName (..),
    Containerfile (..),
    mkImageName,
    nixCommandProfile,
    nixCommandPath,
    nixFlags,
    nixArgs,
) where

import Data.Digest.Pure.SHA qualified as SHA
import Data.Text qualified as Text
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

defaultNixCache, defaultNixCachePublicKey :: Text
defaultNixCache = "https://cache.nixos.org"
defaultNixCachePublicKey = "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="

{- | Get the cache url from its public key

>>> cacheUrlFromKey "podenv.cachix.org-1:FA80Dv5XSHxzMYOg1wEANhDN7h43nN8twBJiYNzcDGY="
"https://podenv.cachix.org"
-}
cacheUrlFromKey :: Text -> Text
cacheUrlFromKey cacheKey = case rest of
    "" -> error $ ".cachix.org-1 not found in flake cache: '" <> cacheKey <> "'"
    _ -> "https://" <> cacheUrlPrefix <> ".cachix.org"
  where
    (cacheUrlPrefix, rest) = Text.breakOn ".cachix.org-1:" cacheKey

nixArgs :: Flakes -> [Text]
nixArgs flakes = nixExtraArgs <> nixCacheArgs <> installables flakes
  where
    nixCacheArgs = case cache flakes of
        Just cacheKey ->
            [ "--option"
            , "binary-caches"
            , Text.unwords [defaultNixCache, cacheUrlFromKey cacheKey]
            , "--option"
            , "trusted-public-keys"
            , Text.unwords [defaultNixCachePublicKey, cacheKey]
            ]
        Nothing -> []
    nixExtraArgs = case nixpkgs flakes of
        Just pin | not (all (Text.isPrefixOf pin) (installables flakes)) -> ["--override-input", "nixpkgs", pin]
        _ -> []
