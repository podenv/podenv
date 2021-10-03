{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | This module contains the logic to load the dhall configuration
module Podenv.Config
  ( load,
    select,
    Config (..),
    Atom (..),
    ApplicationRecord (..),
    defaultConfigPath,
    defaultApp,
    Builder (..),
    BuilderNix (..),
    BuilderContainer (..),
    loadSystem,
    defaultSystemConfig,
    podenvImportTxt,
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
import Dhall.Marshal.Decode (DhallErrors (..), extractError)
import qualified Dhall.Parser
import qualified Dhall.Src
import Podenv.Dhall hiding (name)
import Podenv.Prelude
import System.Directory
import System.Environment (setEnv, unsetEnv)
import System.FilePath.Posix (dropExtension, isExtensionOf, splitPath)
import qualified Text.Show

data Config
  = -- | A standalone application, e.g. defaultSelector
    ConfigDefault ApplicationRecord
  | -- | A single application
    ConfigApplication Atom
  | -- | A collection of applications
    ConfigApplications [(Text, Atom)]

data Atom
  = -- | A literal application
    Lit ApplicationRecord
  | -- | A paremeterized application
    LamArg ArgName (Text -> ApplicationRecord)
  | -- | A functional application
    LamApp (Application -> ApplicationRecord)

-- | A wrapper around the true Application type to manage weakly typed configuration
-- (e.g. so that `{ runtime.image = "ubi8" }` can be manually decoded)
newtype ApplicationRecord = ApplicationRecord {unRecord :: Application}

instance Dhall.FromDhall ApplicationRecord where
  autoWith = const appRecordDecoder

newtype ArgName = ArgName Text

instance Text.Show.Show ArgName where
  show (ArgName n) = toString n

-- | Config load entrypoint
load :: Maybe Text -> Maybe Text -> IO Config
load selectorM configTxt = case defaultSelector of
  Just c -> pure $ ConfigDefault (ApplicationRecord c)
  Nothing -> load' . Dhall.normalize <$> loadExpr configTxt
  where
    defaultSelector :: Maybe Application
    defaultSelector = case selectorM of
      Just s
        | "image:" `Text.isPrefixOf` s -> imageApp s
        | "nix:" `Text.isPrefixOf` s -> nixApp s
      _ -> Nothing
    imageApp x = mkApp ("image-" <> mkName x) (Image $ Text.drop (Text.length "image:") x)
    nixApp x = mkApp ("nix-" <> mkName x) (Nix $ Text.drop (Text.length "nix:") x)
    mkApp name runtime' = Just $ defaultApp & (appName .~ name) . (appRuntime .~ runtime')
    mkName = Text.take 6 . toText . SHA.showDigest . SHA.sha1 . encodeUtf8

-- | Inject the package.dhall into the environ so that config can use `env:PODENV`
loadWithEnv :: FilePath -> Dhall.Expr Dhall.Src.Src Dhall.Import -> IO DhallExpr
loadWithEnv baseDir expr = withEnv $ flip evalStateT initialState $ Dhall.Import.loadWith expr
  where
    -- Set the PODENV environment variable to the frozen url of the current hub
    withEnv = bracket_ (setEnv "PODENV" (toString podenvImportTxt)) (unsetEnv "PODENV")
    -- Then use a custom Dhall.Import.Status state that inject the static code
    -- The goal is to only pretty print (text encode) the package when the cache is cold
    initialState = baseStatus & Dhall.Import.remote .~ fetchUrl
    baseStatus = Dhall.Import.emptyStatus baseDir
    fetchUrl url
      | url == podenvUrl = pure (Dhall.pretty podenvPackage)
      | otherwise = (baseStatus ^. Dhall.Import.remote) url

-- | Helper function to parse the initial configuration
loadExpr :: Maybe Text -> IO DhallExpr
loadExpr configM = case configM of
  Just configTxt -> do
    cwd' <- getCurrentDirectory
    loadWithEnv cwd' $ exprFromText' configTxt
  Nothing -> do
    baseDir <- getConfigDir
    let configPath = baseDir </> "config.dhall"

    -- load main config
    configContent <-
      bool (pure "(env:PODENV).Hub") (Text.readFile configPath) =<< doesFileExist configPath
    let configExpr = exprFromText' configContent

    -- lookup local.d configs
    let locald = baseDir </> "local.d"
    localFiles <- bool (pure []) (listDirectory locald) =<< doesPathExist locald
    let localConfig = createLocalRecord locald localFiles

    -- adds local.d to the main config using the `//` operator
    let expr' = Dhall.Prefer Nothing Dhall.PreferFromSource configExpr localConfig
    loadWithEnv baseDir expr'
  where
    exprFromText' :: Text -> Dhall.Expr Dhall.Src.Src Dhall.Import
    exprFromText' configTxt = case Dhall.Parser.exprFromText "<config>" configTxt of
      Right expr -> expr
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

-- | The static hub package expression
podenvUrl :: Dhall.URL
podenvUrl =
  Dhall.URL Dhall.HTTPS "raw.githubusercontent.com" path Nothing Nothing
  where
    hubVersion = case Dhall.extract Dhall.auto (Dhall.renote hubCommit) of
      Success x -> Text.dropEnd 1 x
      Failure v -> error $ "Unknown hub commit: " <> show v
    path = Dhall.File (Dhall.Directory [hubVersion, "hub", "podenv"]) "package.dhall"

podenvImport :: Dhall.Import
podenvImport =
  Dhall.Import (Dhall.ImportHashed (Just hash) (Dhall.Remote podenvUrl)) Dhall.Code
  where
    hash = Dhall.Import.hashExpression (Dhall.alphaNormalize podenvPackage)

podenvImportTxt :: Text
podenvImportTxt = Text.replace "\n " "" $ Dhall.pretty podenvImport

-- | Pure config load
load' :: DhallExpr -> Config
load' expr = case loadConfig "" expr of
  Success [(selector, Lit app)] -> ConfigApplication $ Lit (ensureName selector app)
  Success [(_, x)] -> ConfigApplication x
  Success [] -> error "No application found"
  Success xs -> ConfigApplications xs
  Failure (DhallErrors (x :| _)) -> error $ show x

-- | When an application doesn't have a name, set it to the selector path
ensureName :: Text -> ApplicationRecord -> ApplicationRecord
ensureName x app = case unRecord app ^. appName of
  "" -> ApplicationRecord $ unRecord app & appName .~ x
  _ -> app

-- | The main config load function. It recursively descend the
-- tree by extending the selector name.
loadConfig :: Text -> DhallExpr -> DhallParser [(Text, Atom)]
loadConfig baseSelector expr = case expr of
  -- When the node is a function, assume it is an app.
  Dhall.Lam {} -> (\app -> [(baseSelector, app)]) <$> loadApp expr
  Dhall.RecordLit kv
    | -- When the node has a "runtime" attribute, assume it is an app.
      DM.member "runtime" kv ->
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
  _ -> extractError $ baseSelector <> ": expected a record literal, but got: " <> Text.take 256 (show expr)

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
select config args = (fmap . fmap) unRecord <$> select' config args

select' :: Config -> [Text] -> Either Text ([Text], (Maybe Builder, ApplicationRecord))
select' config args = case config of
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
    ensureBuilder :: [(Text, Atom)] -> ApplicationRecord -> Either Text (Maybe Builder, ApplicationRecord)
    ensureBuilder atoms app = do
      builderM <-
        case unRecord app ^. appRuntime of
          Image name
            | name == mempty -> Left "Empty image"
            | otherwise -> Right Nothing
          Nix {} -> case lookup "nix.setup" atoms of
            Just (Lit x) -> Right $ Just $ NixBuilder $ BuilderNix "nix.setup" (unRecord x)
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
          appb <- ensureBuilder atoms (f (unRecord app))
          pure (rest, appb)
        [] -> Left "Missing app argument"

defaultConfigPath :: Text
defaultConfigPath = "~/.config/podenv/config.dhall"

-- | The default app
defaultApp :: Application
defaultApp = case Dhall.extract Dhall.auto (Dhall.renote appDefault) of
  Success app -> app
  Failure v -> error $ "Invalid default application: " <> show v

defaultNixBuilder :: BuilderNix
defaultNixBuilder = case loadApp (Dhall.renote hubNixBuilder) of
  (Success (Lit a)) -> BuilderNix "default" (unRecord a)
  _ -> error "Can't load default nix builder."

-- | A type synonym to simplify function annotation.
type DhallParser a = Dhall.Extractor Dhall.Src.Src Void a

-- | A type synonym to simplify function annotation.
type DhallExpr = Dhall.Expr Dhall.Src.Src Void

type DhallExtractor a = Dhall.Extractor Dhall.Src.Src Void a

-- | The `//` dhall record update operation
pref :: Dhall.Expr s a -> Dhall.Expr s a -> Dhall.Expr s a
pref = Dhall.Prefer Nothing Dhall.PreferFromSource

mkRecord :: [(Text, Dhall.Expr s a)] -> Dhall.Expr s a
mkRecord kv = Dhall.RecordLit (Dhall.makeRecordField <$> DM.fromList kv)

recordItems :: DM.Map Text (Dhall.RecordField s a) -> [(Text, Dhall.Expr Void a)]
recordItems kv = fmap (Dhall.denote . Dhall.recordFieldValue) <$> DM.toList kv

-- | A custom Dhall Decoder that can convert a weakly type Application
-- This is done by modifying the 'base' Dhall.Expr with:
--   App.default // (base // { capabilities = Caps.default // base.capabilities })
-- so that the missing fields are automatically added.
--
-- For the runtime value, this convert may convert a tag record to an Union variant
appRecordDecoder :: Dhall.Decoder ApplicationRecord
appRecordDecoder = ApplicationRecord <$> Dhall.Decoder extract expected
  where
    extract :: Dhall.Expr Dhall.Src.Src Void -> DhallExtractor Application
    extract (Dhall.RecordLit kv) = case DM.lookup "runtime" kv of
      Just (Dhall.RecordField _ (Dhall.RecordLit kv') _ _) -> extract' kv (runtimeFromRecord kv')
      Just (Dhall.RecordField _ v _ _) -> extract' kv (Dhall.denote v)
      Nothing -> Dhall.extractError "Application does not have a runtime"
    extract _ = Dhall.extractError "Application is not a record"

    runtimeFromRecord :: DM.Map Text (Dhall.RecordField s Void) -> Dhall.Expr Void Void
    runtimeFromRecord kv = case recordItems kv of
      [("image", x)] -> mkRuntime "Image" x
      [("nix", x)] -> mkRuntime "Nix" x
      [("containerfile", x)] -> mkRuntime "Container" (pref containerBuildDefault (mkRecord [("containerfile", x)]))
      _ -> mkRuntime "Container" (pref containerBuildDefault (Dhall.denote (Dhall.RecordLit kv)))
      where
        mkRuntime field v =
          Dhall.App (Dhall.Field runtimeType (Dhall.FieldSelection Nothing field Nothing)) v

    extract' :: DM.Map Text (Dhall.RecordField s Void) -> Dhall.Expr Void Void -> DhallExtractor Application
    extract' kv runtimeExpr =
      let capsExpr = case DM.lookup "capabilities" kv of
            Just (Dhall.RecordField _ v _ _) -> pref capsDefault (Dhall.denote v)
            _ -> capsDefault

          -- capabilities and runtime are always added since they are nested schemas
          nestedSchemas = [("capabilities", capsExpr), ("runtime", runtimeExpr)]

          baseExpr = pref (Dhall.denote $ Dhall.RecordLit kv) (mkRecord nestedSchemas)
          expr = pref appDefault baseExpr

          -- The generic Application decoder
          Dhall.Decoder appDecoder _ = Dhall.genericAuto
       in appDecoder (Dhall.renote (Dhall.normalize expr))

    expected = Success (Dhall.renote appType)

-- | Parse and tag a DhallExpr with an Atom constructor
loadApp :: DhallExpr -> DhallParser Atom
loadApp expr = case expr of
  Dhall.Lam _ fb _
    | Dhall.denote (Dhall.functionBindingAnnotation fb) == appType ->
      LamApp <$> Dhall.extract Dhall.auto expr
    | otherwise -> LamArg (ArgName $ Dhall.functionBindingVariable fb) <$> Dhall.extract Dhall.auto expr
  _ -> Lit <$> Dhall.extract Dhall.auto expr

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
defaultSystemConfig = case Dhall.extract Dhall.auto (Dhall.renote systemConfigDefault) of
  Success x -> x
  Failure v -> error $ "Invalid default system config: " <> show v
