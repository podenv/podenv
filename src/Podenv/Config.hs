{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | This module contains the logic to load the dhall configuration
module Podenv.Config (
    loadConfig,
    defaultSelector,
    decodeExpr,
    select,
    setSelector,
    Config (..),
    Atom (..),
    ApplicationRecord (..),
    defaultAppRes,
    defaultApp,
    podenvImportTxt,
    getAppName,
) where

import Control.Exception (bracket_)
import Data.Either.Validation
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Text.IO qualified as Text (readFile)
import Dhall qualified
import Dhall.Core qualified as Dhall
import Dhall.Import qualified
import Dhall.Map qualified as DM
import Dhall.Marshal.Decode (DhallErrors (..), extractError)
import Dhall.Parser qualified
import Dhall.Src qualified
import Podenv.Dhall
import Podenv.Prelude
import Podenv.Version qualified
import System.Environment (setEnv, unsetEnv)
import System.FilePath.Posix (dropExtension, isExtensionOf, splitPath)
import Text.Show qualified

newtype Config = Config {unConfig :: [(Text, Atom)]}

data Atom
    = -- | A literal application
      LitApp ApplicationRecord
    | LitAppRes ApplicationResource
    | -- | A paremeterized application
      LamArg ArgName (Text -> ApplicationRecord)
    | LamArg2 ArgName ArgName (Text -> Text -> ApplicationRecord)
    | -- | A functional application
      LamApp (Application -> ApplicationRecord)

{- | A wrapper around the true Application type to manage weakly typed configuration
(e.g. so that `{ runtime.image = "ubi8" }` can be manually decoded)
-}
newtype ApplicationRecord = ApplicationRecord {unRecord :: Application}

instance Dhall.FromDhall ApplicationRecord where
    autoWith = const appRecordDecoder

newtype ArgName = ArgName Text

instance Text.Show.Show ArgName where
    show (ArgName n) = toString n

-- | Config load entrypoint
loadConfig :: Maybe Text -> IO Config
loadConfig configTxt = Config . decodeExpr . Dhall.normalize <$> loadExpr configTxt

defaultSelector :: Text -> Maybe (Text, Application)
defaultSelector s
    | "image:" `Text.isPrefixOf` s = imageApp s
    | "nix:" `Text.isPrefixOf` s = nixApp s
    | "nixpkgs#" `Text.isPrefixOf` s = nixApp' s
    | "rootfs:" `Text.isPrefixOf` s = rootfsApp s
    | otherwise = Nothing
  where
    imageApp x = mkApp (Image $ Text.drop (Text.length "image:") x)
    nixApp x = nixApp' (Text.drop (Text.length "nix:") x)
    rootfsApp x = mkApp (Rootfs $ Text.drop (Text.length "rootfs:") x)
    nixApp' x = mkApp (Nix x)
    mkApp r = Just (s, defaultApp r)

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
    podenvVersion = Text.pack Podenv.Version.version
    path = Dhall.File (Dhall.Directory ["hub", podenvVersion, "podenv", "podenv"]) "package.dhall"

podenvImport :: Dhall.Import
podenvImport =
    Dhall.Import (Dhall.ImportHashed (Just hash) (Dhall.Remote podenvUrl)) Dhall.Code
  where
    hash = Dhall.Import.hashExpression (Dhall.alphaNormalize podenvPackage)

podenvImportTxt :: Text
podenvImportTxt = Text.replace "\n " "" $ Dhall.pretty podenvImport

-- | Pure config load
decodeExpr :: DhallExpr -> [(Text, Atom)]
decodeExpr expr = case decodeExprAtom "" expr of
    Success [] -> error "No application found"
    Success xs -> xs
    Failure (DhallErrors (x :| _)) -> error $ show x

{- | The main config load function. It recursively descend the
tree by extending the selector name.
-}
decodeExprAtom :: Text -> DhallExpr -> DhallParser [(Text, Atom)]
decodeExprAtom baseSelector expr = case expr of
    -- When the node is a function, assume it is an app.
    Dhall.Lam{} -> (\app -> [(baseSelector, app)]) <$> loadApp expr
    Dhall.RecordLit kv
        | -- When the node has a "runtime" attribute, assume it is an app.
          DM.member "runtime" kv ->
            (\app -> [(baseSelector, app)]) <$> loadApp expr
        | -- When the node has a "kind" and "apiVersion" attribute, assume it is an appRes.
          DM.member "kind" kv && DM.member "apiVersion" kv ->
            (\ar -> [(baseSelector, LitAppRes ar)]) <$> Dhall.extract Dhall.auto expr
        | -- Otherwise, traverse each attributes
          otherwise ->
            concat <$> traverse (uncurry loadCollection) (DM.toList kv)
      where
        loadCollection n e
            -- Skip leaf starting with `use`, otherwise they can be used and likely fail with:
            -- FromDhall: You cannot decode a function if it does not have the correct type
            | "use" `Text.isPrefixOf` n = pure []
            | otherwise = decodeExprAtom (mkSelector n) (Dhall.recordFieldValue e)
        mkSelector name
            | baseSelector == mempty = name
            | name == "default" = baseSelector
            | otherwise = baseSelector <> "." <> name
    _ -> extractError $ baseSelector <> ": expected a record literal, but got: " <> Text.take 256 (show expr)

-- | Select the application, returning the unused cli args.
select :: Config -> [Text] -> Either Text ([Text], ApplicationResource)
select (Config config) args = case nonEmpty args of
    Just args' -> fmap (uncurry setSelector) <$> select' config args'
    -- When no argument are provided, if the config only has one application, then automatically select it
    Nothing -> case config of
        [(sel, atom)] ->
            fmap (setSelector sel) <$> case atom of
                LitApp app -> Right ([], defaultAppRes (unRecord app))
                LitAppRes ar -> Right ([], ar)
                _ -> Left "Can't select a non literal app without args"
        _ -> Left "Multiple apps configured, provides a selector"

setSelector :: Text -> ApplicationResource -> ApplicationResource
setSelector n
    | n == "" = id
    | otherwise = (arMetadata . metaLabels) %~ Map.insert "podenv.selector" n

getAppName :: ApplicationResource -> Maybe Text
getAppName ar = selector <|> ar ^. arMetadata . metaName
  where
    selector = Map.lookup "podenv.selector" (ar ^. arMetadata . metaLabels)

select' :: [(Text, Atom)] -> NonEmpty Text -> Either Text ([Text], (Text, ApplicationResource))
select' atoms baseArgs@(baseSelector :| baseRemainingArgs) = do
    (selector, args, atom) <- lookupAtom
    case atom of
        LitAppRes ar -> case deprecatedFields (ar ^. arApplication) of
            [] -> pure (args, (selector, ar))
            xs -> Left $ "ApplicationResource uses deprecated fields: " <> show xs
        _ -> do
            (args', app) <- selectApp args atom
            let selectorName
                    | -- If the selection didn't touch the args, then keep the selector name
                      args == args' =
                        selector
                    | -- Otherwise, pick the last element used
                      otherwise = case drop (length args - length args') args of
                        x : _ -> x
                        _ -> ""
            pure (args', (selectorName, defaultAppRes (unRecord app)))
  where
    deprecatedFields app = n <> ns
      where
        n = case app ^. appName of
            "" -> []
            _ -> ["move name to metadata.name" :: Text]
        ns = maybe [] (const ["move namespace to resources.network"]) (app ^. appNamespace)

    lookupAtom :: Either Text (Text, [Text], Atom)
    lookupAtom = case lookup baseSelector atoms of
        Just a -> Right (baseSelector, baseRemainingArgs, a)
        Nothing -> case atoms of
            [(sel, a)] ->
                let remainingArgs
                        -- If the selector match the atom name, then drop it from the args
                        | sel == baseSelector = baseRemainingArgs
                        | otherwise = toList baseArgs
                 in Right (sel, remainingArgs, a)
            _ -> Left $ baseSelector <> ": not found"

    selectApp :: [Text] -> Atom -> Either Text ([Text], ApplicationRecord)
    selectApp args' atom = case atom of
        LitAppRes _ -> Left "Can't select ApplicationResource"
        -- App is not a function, don't touch the arg
        LitApp app -> pure (args', app)
        -- App needs an argument, the tail is the arg
        LamArg arg f -> case args' of
            (x : xs) -> pure (xs, f x)
            [] -> Left ("Missing argument: " <> show arg)
        LamArg2 arg1 arg2 f -> case args' of
            (x : y : xs) -> pure (xs, f x y)
            (_ : _) -> Left ("Missing argument: " <> show arg2)
            _ -> Left ("Missing arguments: " <> show arg1 <> " " <> show arg2)
        LamApp f -> case args' of
            (x : xs) -> case defaultSelector x of
                Just (_, app) -> pure (xs, f app)
                Nothing -> do
                    -- Recursively select the app to eval arg `mod app arg` as `mod (app arg)`
                    -- e.g. LamApp should be applied at the end.
                    atom' <- lookup x atoms `orDie` (x <> ": unknown lam arg")
                    (rest, app) <- selectApp xs atom'
                    pure (rest, f (unRecord app))
            [] -> Left "Missing app argument"

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

{- | A custom Dhall Decoder that can convert a weakly type Application
This is done by modifying the 'base' Dhall.Expr with:
  App.default // (base // { capabilities = Caps.default // base.capabilities })
so that the missing fields are automatically added.

For the runtime value, this convert may convert a tag record to an Union variant
-}
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
        mkRuntime field =
            Dhall.App (Dhall.Field runtimeType (Dhall.FieldSelection Nothing field Nothing))

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
    Dhall.Lam _ fb1 (Dhall.Lam _ fb2 _) -> LamArg2 (getArgName fb1) (getArgName fb2) <$> Dhall.extract Dhall.auto expr
    Dhall.Lam _ fb _
        | Dhall.denote (Dhall.functionBindingAnnotation fb) == appType ->
            LamApp <$> Dhall.extract Dhall.auto expr
        | otherwise -> LamArg (getArgName fb) <$> Dhall.extract Dhall.auto expr
    _ -> LitApp <$> Dhall.extract Dhall.auto expr
  where
    getArgName = ArgName . Dhall.functionBindingVariable
