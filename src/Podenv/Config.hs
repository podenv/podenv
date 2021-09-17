{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | This module contains the logic to load the dhall configuration
module Podenv.Config
  ( load,
    select,
    Config (..),
    Atom (..),
    defaultConfigPath,
    defaultApp,
    Builder (..),
    BuilderNix (..),
    BuilderContainer (..),
    loadSystem,
    defaultSystemConfig,
  )
where

import Control.Exception (bracket_)
import qualified Data.Digest.Pure.SHA as SHA
import Data.Either.Validation
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Dhall
import qualified Dhall.Core as Dhall
import qualified Dhall.Freeze as Dhall
import qualified Dhall.Import (writeExpressionToSemanticCache)
import qualified Dhall.Map as DM
import qualified Dhall.Src
import qualified Dhall.TH
import Podenv.Dhall (Application (name, runtime), ContainerBuild, Runtime (..), SystemConfig, appName, appRuntime, version)
import Podenv.Prelude
import System.Directory
import System.Environment (setEnv, unsetEnv)
import qualified Text.Show

data Config
  = -- | A standalone application, e.g. defaultSelector
    ConfigDefault Application
  | -- | A single application
    ConfigApplication Atom
  | -- | A collection of applications
    ConfigApplications [(Text, Atom)]

data Atom
  = -- | A literal application
    Lit Application
  | -- | A paremeterized application
    LamArg ArgName (Text -> Application)
  | -- | A functional application
    LamApp (Application -> Application)

newtype ArgName = ArgName Text

instance Text.Show.Show ArgName where
  show (ArgName n) = toString n

-- | Config load entrypoint
load :: Maybe Text -> Text -> IO Config
load selectorM exprTxt = case defaultSelector of
  Just c -> pure $ ConfigDefault c
  Nothing -> ensureDefault exprTxt >> load' <$> withDhallEnv (Dhall.inputExpr exprTxt)
  where
    defaultSelector :: Maybe Application
    defaultSelector = case selectorM of
      Just s
        | "image:" `Text.isPrefixOf` s -> Just $ imageApp s
        | "nix:" `Text.isPrefixOf` s -> Just $ nixApp s
      _ -> Nothing
    imageApp x = defaultApp {name = "image-" <> mkName x, runtime = Image $ Text.drop (Text.length "image:") x}
    nixApp x = defaultApp {name = "nix-" <> mkName x, runtime = Nix $ Text.drop (Text.length "nix:") x}
    mkName = Text.take 6 . toText . SHA.showDigest . SHA.sha1 . encodeUtf8

-- | Inject the package.dhall into the environ so that config can use `env:PODENV`
withDhallEnv :: IO a -> IO a
withDhallEnv =
  bracket_
    (setEnv "PODENV" (toString $ Dhall.pretty dhallPackage))
    (unsetEnv "PODENV")

dhallPackage :: Dhall.Expr Void Void
dhallPackage = $(Dhall.TH.staticDhallExpression "./package.dhall")

-- | Pure config load
load' :: DhallExpr -> Config
load' expr = case loadConfig "" expr of
  Success [(selector, Lit app)] -> ConfigApplication $ Lit (ensureName selector app)
  Success [(_, x)] -> ConfigApplication x
  Success xs -> ConfigApplications xs
  Failure x -> error $ show x

-- | When an application doesn't have a name, set it to the selector path
ensureName :: Text -> Application -> Application
ensureName x app = case app ^. appName of
  "" -> app & appName .~ x
  _ -> app

-- | The main config load function. It recursively descend the
-- tree by extending the selector name.
loadConfig :: Text -> DhallExpr -> DhallParser [(Text, Atom)]
loadConfig baseSelector expr = case expr of
  -- When the node is a function, assume it is an app.
  Dhall.Lam {} -> (\app -> [(baseSelector, app)]) <$> loadApp expr
  Dhall.RecordLit kv
    | -- When the node has a "name" attribute, assume it is an app.
      DM.member "name" kv ->
      (\app -> [(baseSelector, app)]) <$> loadApp expr
    | -- Otherwise, traverse each attributes
      otherwise ->
      concat <$> traverse (uncurry loadCollection) (DM.toList kv)
    where
      loadCollection n e
        -- Skip leaf starting with `use`, otherwise they can be used and likely fail with:
        -- FromDhall: You cannot decode a function if it does not have the correct type
        | "use" `Text.isPrefixOf` n = pure []
        | otherwise = loadConfig (mkSelector n) (Dhall.recordFieldValue e)
      mkSelector name
        | baseSelector == mempty = name
        | name == "default" = baseSelector
        | otherwise = baseSelector <> "." <> name
  _ -> error $ "Bad config: " <> Text.take 256 (show expr)

data Builder
  = NixBuilder BuilderNix
  | ContainerBuilder BuilderContainer

data BuilderNix = BuilderNix
  { bnName :: Text,
    buildNixApp :: Application
  }

data BuilderContainer = BuilderContainer
  { bcName :: Text,
    bcBuild :: ContainerBuild
  }

-- | Select the application, returning the unused cli args.
select :: Config -> [Text] -> ([Text], (Maybe Builder, Application))
select config args = case config of
  -- config default is always selected, drop the first args
  ConfigDefault app -> (tail (fromList args), ensureBuilder [] app)
  -- config has only one application, don't touch the args
  ConfigApplication atom -> selectApp [] args atom
  -- config has some applications, the first arg is a selector
  ConfigApplications atoms -> case args of
    (selector : xs) ->
      let app = fromMaybe (error $ selector <> ": not found") $ lookup selector atoms
          name' = selector -- todo mappend the extra selector arg
       in fmap (ensureName name') <$> selectApp atoms xs app
    [] -> error "Multiple apps configured, provides a selector"
  where
    -- When an application define a builder, lookup its definition
    ensureBuilder :: [(Text, Atom)] -> Application -> (Maybe Builder, Application)
    ensureBuilder atoms app =
      let builderM = case app ^. appRuntime of
            Image name
              | name == mempty -> error "Empty image"
              | otherwise -> Nothing
            Nix {} -> Just $ case lookup "nix.setup" atoms of
              Just (Lit x) -> NixBuilder $ BuilderNix "nix.setup" x
              Just _ -> error "Invalid nix.setup"
              Nothing -> NixBuilder defaultNixBuilder
            Container cb -> containerBuilder cb
          containerBuilder cb = Just $ ContainerBuilder $ BuilderContainer "<inline>" cb
       in (builderM, app)

    selectApp atoms args' atom = case atom of
      -- App is not a function, don't touch the arg
      Lit x -> (args', ensureBuilder atoms x)
      -- App needs an argument, the tail is the arg
      LamArg arg f -> case args' of
        (x : xs) -> (xs, ensureBuilder atoms (f x))
        [] -> error $ "Missing argument: " <> show arg
      LamApp f -> case args' of
        (x : xs) ->
          -- Recursively select the app to eval arg `mod app arg` as `mod (app arg)`
          -- e.g. LamApp should be applied at the end.
          let (rest, (_, app)) = selectApp atoms xs (fromMaybe (error $ "Unknown lam arg: " <> x) $ lookup x atoms)
           in (rest, ensureBuilder atoms (f app))
        [] -> error "Missing app argument"

defaultConfigPath :: Text
defaultConfigPath = "~/.config/podenv/config.dhall"

ensureDefault :: Text -> IO ()
ensureDefault expr
  | expr /= defaultConfigPath = pure ()
  | otherwise = do
    baseDir <- getConfigDir
    createDirectoryIfMissing True baseDir
    maybeWrite (baseDir </> "config.dhall") (pure $ unlines defaultConfigContent)
    maybeWrite (baseDir </> "Hub.dhall") (freezeHub baseDir)
  where
    -- write unless file already exists
    maybeWrite fp action = do
      exist <- doesFileExist fp
      -- TODO: check if Hub.dhall contents is using an old schemas version
      unless exist $ do
        content <- action
        Text.writeFile fp content
    hubUrl =
      let path = Dhall.File (Dhall.Directory ["schemas-v" <> show Podenv.Dhall.version, "hub", "podenv"]) "package.dhall"
       in Dhall.URL Dhall.HTTPS "raw.githubusercontent.com" path Nothing Nothing
    hubImport = Dhall.Import (Dhall.ImportHashed Nothing (Dhall.Remote hubUrl)) Dhall.Code
    freezeHub baseDir = do
      putTextLn $ "Initializing " <> toText (baseDir </> "Hub.dhall") <> ", this may take some time..."
      -- We can't use withDhallEnv here because remote import can't access env: or ~/ path.
      -- Instead we should look into using `Dhall.Import.loadWith` and a custom substituer to inject (env:PODENV)
      -- Until then, we can write the schemas to the cache
      Dhall.Import.writeExpressionToSemanticCache dhallPackage
      finalImport <- Dhall.freezeImport "." hubImport
      pure $
        unlines
          [ "-- | The default dhall hub. Replace it by a local copy, e.g. `~/src/github.com/podenv/hub/package.dhall`",
            "-- Or delete the file to update the reference.",
            Dhall.pretty (Dhall.Embed finalImport)
          ]
    defaultConfigContent =
      [ "-- | The initial podenv configuration.",
        "-- Add your own applications:",
        "-- let my-app = (env:PODENV).Application::{=} in ./Hub.dhall // { my-app }",
        "",
        "./Hub.dhall"
      ]

-- | The default app
defaultApp :: Application
defaultApp = case Dhall.extract Dhall.auto $(Dhall.TH.staticDhallExpression "(./package.dhall).Application.default // { runtime = (./package.dhall).Image \"\" }") of
  Success app -> app
  Failure v -> error $ "Invalid default application: " <> show v

defaultNixBuilder :: BuilderNix
defaultNixBuilder = case loadApp $(Dhall.TH.staticDhallExpression "./schemas/nix.dhall") of
  (Success (Lit a)) -> BuilderNix "default" a
  _ -> error "Can't load default nix builder."

-- | A type synonym to simplify function annotation.
type DhallParser a = Dhall.Extractor Dhall.Src.Src Void a

-- | A type synonym to simplify function annotation.
type DhallExpr = Dhall.Expr Dhall.Src.Src Void

-- | Parse and tag a DhallExpr with an Atom constructor
loadApp :: DhallExpr -> DhallParser Atom
loadApp expr = case expr of
  Dhall.Lam _ fb _
    | Dhall.functionBindingAnnotation fb == typeApp ->
      LamApp <$> Dhall.extract Dhall.auto expr
    | otherwise -> LamArg (ArgName $ Dhall.functionBindingVariable fb) <$> Dhall.extract Dhall.auto expr
  _ -> Lit <$> Dhall.extract Dhall.auto expr
  where
    -- The type of an application
    typeApp :: DhallExpr
    typeApp = $(Dhall.TH.staticDhallExpression "(./package.dhall).Application.Type")

loadSystem :: IO SystemConfig
loadSystem = do
  confDir <- getXdgDirectory XdgConfig "podenv"
  let fp = confDir </> "system.dhall"
  exist <- doesFileExist fp
  if exist
    then Dhall.input Dhall.auto (toText fp)
    else pure defaultSystemConfig

-- | The default system config
defaultSystemConfig :: SystemConfig
defaultSystemConfig =
  case Dhall.extract Dhall.auto $(Dhall.TH.staticDhallExpression "(./package.dhall).System.default") of
    Success x -> x
    Failure v -> error $ "Invalid default system config: " <> show v
