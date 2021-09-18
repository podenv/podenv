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
import qualified Data.Text.IO as Text (readFile)
import qualified Dhall
import qualified Dhall.Core as Dhall
import qualified Dhall.Import
import qualified Dhall.Map as DM
import Dhall.Marshal.Decode (extractError)
import qualified Dhall.Parser
import qualified Dhall.Src
import qualified Dhall.TH
import Podenv.Dhall (Application (name, runtime), ContainerBuild, Runtime (..), SystemConfig, appName, appRuntime)
import Podenv.Prelude
import System.Directory
import System.Environment (setEnv, unsetEnv)
import System.FilePath.Posix (dropExtension, isExtensionOf, splitPath)
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
load selectorM configTxt = case defaultSelector of
  Just c -> pure $ ConfigDefault c
  Nothing -> load' <$> withDhallEnv (loadExpr configTxt)
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
    (setEnv' "PODENV" dhallPackage >> setEnv' "HUB" hubPackage)
    (unsetEnv "PODENV" >> unsetEnv "HUB")
  where
    setEnv' env expr = setEnv env (toString $ Dhall.pretty expr)

loadExpr :: Text -> IO DhallExpr
loadExpr configTxt
  | configTxt /= defaultConfigPath = Dhall.inputExpr configTxt
  | otherwise = do
    baseDir <- getConfigDir
    let configPath = baseDir </> "config.dhall"
    config <-
      bool (pure "env:HUB") (Text.readFile configPath) =<< doesFileExist configPath
    case Dhall.Parser.exprFromText "~/.config/podenv/config.dhall" config of
      Right configExpr -> do
        -- lookup local.d configs
        let locald = baseDir </> "local.d"
        localFiles <- bool (pure []) (listDirectory locald) =<< doesPathExist locald
        let localConfig = createLocalRecord locald localFiles
        -- adds local.d to the main config using the `//` operator
        let expr' = Dhall.Prefer Nothing Dhall.PreferFromSource configExpr localConfig
        Dhall.normalize <$> Dhall.Import.loadRelativeTo baseDir Dhall.Import.UseSemanticCache expr'
      Left e -> error $ "Invalid config: " <> show e

-- | Create a record for local.d config
createLocalRecord :: FilePath -> [FilePath] -> Dhall.Expr Dhall.Src.Src Dhall.Import
createLocalRecord baseDir =
  Dhall.RecordLit . fromList . map toRecord . filter (isExtensionOf ".dhall")
  where
    toRecord :: FilePath -> (Text, Dhall.RecordField Dhall.Src.Src Dhall.Import)
    toRecord name = (toText $ dropExtension name, Dhall.makeRecordField (Dhall.Embed $ toImport name))
    toImport :: FilePath -> Dhall.Import
    toImport name =
      let file = Dhall.File (Dhall.Directory $ reverse $ map toText $ splitPath baseDir) (toText name)
       in Dhall.Import (Dhall.ImportHashed Nothing (Dhall.Local Dhall.Absolute file)) Dhall.Code

dhallPackage :: Dhall.Expr Void Void
dhallPackage = $(Dhall.TH.staticDhallExpression "./hub/schemas/package.dhall")

hubPackage :: Dhall.Expr Void Void
hubPackage = $(Dhall.TH.staticDhallExpression "./hub/package.dhall")

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
  _ -> extractError $ "Bad config: " <> Text.take 256 (show expr)

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
select :: Config -> [Text] -> Either Text ([Text], (Maybe Builder, Application))
select config args = case config of
  -- config default is always selected, drop the first args
  ConfigDefault app -> ensureBuilder [] app >>= \appB -> pure (tail (fromList args), appB)
  -- config has only one application, don't touch the args
  ConfigApplication atom -> selectApp [] args atom
  -- config has some applications, the first arg is a selector
  ConfigApplications atoms -> case args of
    (selector : xs) -> do
      atom <- lookup selector atoms `orDie` (selector <> ": not found")
      (args', app) <- selectApp atoms xs atom
      let name' = selector -- todo: mappend the extra selector arg
      pure (args', ensureName name' <$> app)
    [] -> Left "Multiple apps configured, provides a selector"
  where
    -- When an application define a builder, lookup its definition
    ensureBuilder :: [(Text, Atom)] -> Application -> Either Text (Maybe Builder, Application)
    ensureBuilder atoms app = do
      builderM <-
        case app ^. appRuntime of
          Image name
            | name == mempty -> Left "Empty image"
            | otherwise -> Right Nothing
          Nix {} -> case lookup "nix.setup" atoms of
            Just (Lit x) -> Right $ Just $ NixBuilder $ BuilderNix "nix.setup" x
            Just _ -> Left "Invalid nix.setup"
            Nothing -> Right $ Just $ NixBuilder defaultNixBuilder
          Container cb -> Right $ Just $ ContainerBuilder $ BuilderContainer "<inline>" cb
      pure (builderM, app)

    selectApp atoms args' atom = case atom of
      -- App is not a function, don't touch the arg
      Lit x -> ensureBuilder atoms x >>= \appB -> pure (args', appB)
      -- App needs an argument, the tail is the arg
      LamArg arg f -> case args' of
        (x : xs) -> ensureBuilder atoms (f x) >>= \appB -> pure (xs, appB)
        [] -> Left ("Missing argument: " <> show arg)
      LamApp f -> case args' of
        (x : xs) -> do
          -- Recursively select the app to eval arg `mod app arg` as `mod (app arg)`
          -- e.g. LamApp should be applied at the end.
          atom' <- lookup x atoms `orDie` (x <> ": unknown lam arg")
          (rest, (_, app)) <- selectApp atoms xs atom'
          appb <- ensureBuilder atoms (f app)
          pure (rest, appb)
        [] -> Left "Missing app argument"

defaultConfigPath :: Text
defaultConfigPath = "~/.config/podenv/config.dhall"

-- | The default app
defaultApp :: Application
defaultApp = case Dhall.extract
  Dhall.auto
  $( let package = "(./hub/schemas/package.dhall)"
      in Dhall.TH.staticDhallExpression (package <> ".Application.default // { runtime = " <> package <> ".Image \"\" }")
   ) of
  Success app -> app
  Failure v -> error $ "Invalid default application: " <> show v

defaultNixBuilder :: BuilderNix
defaultNixBuilder = case loadApp $(Dhall.TH.staticDhallExpression "./hub/Builders/nix.dhall") of
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
    typeApp = $(Dhall.TH.staticDhallExpression "(./hub/schemas/package.dhall).Application.Type")

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
  case Dhall.extract Dhall.auto $(Dhall.TH.staticDhallExpression "(./hub/schemas/package.dhall).System.default") of
    Success x -> x
    Failure v -> error $ "Invalid default system config: " <> show v
