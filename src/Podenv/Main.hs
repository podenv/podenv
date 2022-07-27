{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | This module contains the podenv CLI entrypoint
-- The workflow is: Main -> Config -> Build -> Application -> Context
--
-- * Main: select the app and override with command line arguments
-- * Config: load the configuration and select the application
-- * Build: optional application build
-- * App: convert application and capability into a Context
-- * Runtime: execute with podman or kubernetes
module Podenv.Main
  ( main,

    -- * exports for tests
    usage,
    cliConfigLoad,
    cliInfo,
    cliPrepare,
    CLI (..),
  )
where

import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Version (showVersion)
import Options.Applicative hiding (command)
import Paths_podenv (version)
import Podenv.Capability (AppMode (..))
import Podenv.Capability qualified
import Podenv.Config
import Podenv.Dhall
import Podenv.Env
import Podenv.Prelude
import Podenv.Runtime (ExecMode (Foreground), GlobalEnv (..), Name (..), RunEnv)
import Podenv.Runtime qualified
import Podenv.Version qualified (version)

-- | podenv entrypoint
main :: IO ()
main = do
  cli@CLI {..} <- usage =<< getArgs
  when showManifest (printManifest configExpr >> exitSuccess)
  when showDhallEnv (putTextLn Podenv.Config.podenvImportTxt >> exitSuccess)
  when listCaps (printCaps >> exitSuccess)
  when listApps (printApps configExpr >> exitSuccess)
  when listProcs (Podenv.Runtime.listRunningApps >>= traverse_ putTextLn >> exitSuccess)

  (ar, mode, gl, run) <- cliLoad cli
  ctx <- Podenv.Runtime.appToContext run mode ar

  flip runReaderT gl $
    if showApplication
      then putTextLn . showApp ar run =<< Podenv.Runtime.showCmd run Foreground ctx
      else do
        when update $ Podenv.Runtime.updateRuntime run (ar ^. arApplication . appRuntime)
        Podenv.Runtime.execute run Foreground ctx

usage :: [String] -> IO CLI
usage args = do
  cli <- handleParseResult $ execParserPure defaultPrefs cliInfo cliArgs
  pure $ cli {cliExtraArgs = map toText appArgs}
  where
    cliArgs = takeCliArgs [] args
    appArgs = case drop (length cliArgs) args of
      -- Drop any `--` prefix
      ("--" : rest) -> rest
      xs -> xs

    -- Collect args until the selector, the rest should not be passed to optparse-applicative
    isPodenvArg arg
      | arg `elem` strOptions || "--bash-completion-" `isPrefixOf` arg = True
      | otherwise = False
    takeCliArgs acc args' = case args' of
      [] -> reverse acc
      -- Handle toggle such as `"--name" : "app-name" : _`
      (toggle : x : xs) | isPodenvArg toggle -> takeCliArgs (x : toggle : acc) xs
      (x : xs)
        -- `--` is a hard separator, stop now.
        | "--" == x -> takeCliArgs acc []
        -- this is switch, keep on taking
        | "--" `isPrefixOf` x -> takeCliArgs (x : acc) xs
        -- otherwise the selector is found, stop now
        | otherwise -> takeCliArgs (x : acc) []

data CLI = CLI
  { -- action modes:
    listApps :: Bool,
    listCaps :: Bool,
    listProcs :: Bool,
    showManifest :: Bool,
    showDhallEnv :: Bool,
    showApplication :: Bool,
    configExpr :: Maybe Text,
    headlessExpr :: Maybe Text,
    -- runtime env:
    update :: Bool,
    verbose :: Bool,
    -- app modifiers:
    capsOverride :: [Capabilities -> Capabilities],
    cliNetwork :: Maybe (Maybe Text),
    shell :: Bool,
    namespace :: Maybe Text,
    cliName :: Maybe Name,
    cliEnv :: [Text],
    volumes :: [Text],
    -- app selector and arguments:
    selector :: Maybe Text,
    cliExtraArgs :: [Text]
  }

-- WARNING: when adding strOption, update the 'strOptions' list.
-- This is necessary because the argument are pre-processed to detect application arguments.
-- See the 'usage' function where `--` is made optional.
cliParser :: Parser CLI
cliParser =
  CLI
    -- action modes:
    <$> switch (long "list" <> help "List available applications")
    <*> switch (long "list-caps" <> help "List available capabilities")
    <*> switch (long "ps" <> help "List running application")
    <*> switch (long "manifest" <> hidden)
    <*> switch (long "dhall-env" <> hidden)
    <*> switch (long "show" <> help "Show the environment without running it")
    <*> optional (strOption (long "config" <> help "A config expression"))
    <*> optional (strOption (long "headless" <> hidden))
    -- runtime env:
    <*> switch (long "update" <> help "Update the runtime")
    <*> switch (long "verbose" <> help "Increase verbosity")
    -- app modifiers:
    <*> capsParser
    <*> ( (bool Nothing (Just Nothing) <$> switch (long "no-network" <> hidden))
            <|> optional (Just <$> strOption (long "network" <> help "Network name"))
        )
    <*> switch (long "shell" <> help "Start a shell instead of the application command")
    <*> optional (strOption (long "namespace" <> help "The application namespace"))
    <*> (fmap Name <$> optional (strOption (long "name" <> metavar "NAME" <> help "The application name")))
    <*> many (strOption (long "env" <> metavar "ENV" <> help "Extra env 'KEY=VALUE'"))
    <*> many (strOption (long "volume" <> short 'v' <> metavar "VOLUME" <> help "Extra volumes 'volume|hostPath[:containerPath]'"))
    <*> optional (strArgument (metavar "APP" <> help "Application config name or default selector, e.g. image:name"))
    <*> many (strArgument (metavar "ARGS" <> help "Application args"))

-- | List of strOption that accept an argument (that is not the selector)
strOptions :: [String]
strOptions = ["--config", "--headless", "--namespace", "--name", "--env", "--volume", "-v", "--network"]

-- | Parse all capabilities toggles
capsParser :: Parser [Capabilities -> Capabilities]
capsParser = catMaybes <$> traverse mkCapParser (filter notNetwork Podenv.Capability.capsAll)
  where
    notNetwork Podenv.Capability.Cap {..} = capName /= "network"
    mkCapParser :: Podenv.Capability.Cap -> Parser (Maybe (Capabilities -> Capabilities))
    mkCapParser cap =
      toggleCapParser cap True <|> toggleCapParser cap False

-- | A helper function to parse CLI capability toggle
toggleCapParser :: Podenv.Capability.Cap -> Bool -> Parser (Maybe (Capabilities -> Capabilities))
toggleCapParser Podenv.Capability.Cap {..} isOn = setMaybeCap <$> flagParser
  where
    -- We parse a dummy flag from the command line
    flagParser :: Parser (Maybe ())
    flagParser = optional (flag' () (long flagName <> hidden))
    flagName = toString $ (if isOn then "" else "no-") <> capName

    -- That will be replaced by the setCap when Just ()
    setMaybeCap :: Maybe () -> Maybe (Capabilities -> Capabilities)
    setMaybeCap = fmap (const setCap)
    setCap :: Capabilities -> Capabilities
    setCap = capLens .~ isOn

cliInfo :: ParserInfo CLI
cliInfo =
  info
    (versionOption <*> cliParser <**> helper)
    (fullDesc <> header "podenv - a container wrapper")
  where
    versionOption =
      infoOption
        (concat [showVersion version, " ", Podenv.Version.version])
        (long "version" <> help "Show version")

cliLoad :: CLI -> IO (ApplicationResource, AppMode, GlobalEnv, RunEnv)
cliLoad cli = do
  volumesDir <- getDataDir >>= \fp -> pure $ fp </> "volumes"
  env <- Podenv.Env.createEnv
  config <- Podenv.Config.loadConfig (configExpr cli)
  cliConfigLoad volumesDir env config cli

-- | Load the config
cliConfigLoad :: FilePath -> AppEnv 'UnknownState -> Config -> CLI -> IO (ApplicationResource, AppMode, GlobalEnv, RunEnv)
cliConfigLoad volumesDir env config cli@CLI {..} = do
  (extraArgs, baseApp) <-
    case selector >>= Podenv.Config.defaultSelector of
      Just (sel, app) -> pure (cliExtraArgs, defaultAppRes app & setSelector sel)
      Nothing -> do
        (extraArgs, app) <- mayFail $ Podenv.Config.select config (maybeToList selector <> cliExtraArgs)
        pure (extraArgs, app)

  run <- case headlessExpr of
    Just expr -> do
      ecfg <- Podenv.Config.loadConfig (Just expr)
      pure $ Podenv.Runtime.createHeadlessRunEnv ecfg env
    Nothing -> pure $ Podenv.Runtime.createLocalhostRunEnv env

  let app = baseApp & cliPrepare cli
      re = GlobalEnv {verbose, volumesDir, config = Just config}
      mode = if shell then Shell else Regular extraArgs

  pure (app, mode, re, run)

-- | Apply the CLI argument to the application
cliPrepare :: CLI -> ApplicationResource -> ApplicationResource
cliPrepare CLI {..} = setMeta . setApp . setNetwork . setVolumes
  where
    setNetCap = arApplication . appCapabilities . capNetwork
    setNetwork = case cliNetwork of
      Nothing -> id
      Just Nothing -> setNetCap .~ False
      Just (Just netName) ->
        (setNetCap .~ True)
          . ( arNetwork .~ case netName of
                "host" -> Host
                "private" -> Private
                _
                  | "-" `Text.isPrefixOf` netName -> error "Invalid network name"
                  | otherwise -> Shared netName
            )
    setName = maybe id ((metaName ?~) . unName) cliName
    setNS = maybe id (metaNamespace ?~) namespace
    setMeta = arMetadata %~ (setName . setNS)

    setVolumes app' = foldr (\v -> arVolumes %~ (v :)) app' volumes

    setShell = bool id setShellCap shell
    setShellCap = appCapabilities %~ (capTerminal .~ True) . (capInteractive .~ True)
    setEnvs app' = foldr (\v -> appEnviron %~ (v :)) app' cliEnv
    setCaps app' = foldr (appCapabilities %~) app' capsOverride
    setApp = arApplication %~ setShell . setEnvs . setCaps

showApp :: ApplicationResource -> RunEnv -> Text -> Text
showApp ar run cmd = unlines infos
  where
    app = ar ^. arApplication
    rt = Podenv.Runtime.showBuildInfo run (app ^. appRuntime)
    infos =
      [ "[+] runtime: " <> rt,
        "[+] Capabilities",
        unwords (sort appCaps),
        ""
      ]
        <> ["[+] Command", cmd]
    appCaps = concatMap showCap (Podenv.Capability.capsAll <> [netCap])
    netCap = Podenv.Capability.Cap "network" "" capNetwork (pure id)
    showCap Podenv.Capability.Cap {..} =
      [capName | app ^. appCapabilities . capLens]

printCaps :: IO ()
printCaps = do
  putText $ unlines $ sort $ map showCap Podenv.Capability.capsAll
  where
    showCap Podenv.Capability.Cap {..} =
      let sep
            | Text.length capName < 4 = "\t\t\t"
            | Text.length capName < 8 = "\t\t"
            | otherwise = "\t"
       in capName <> sep <> capDescription

printApps :: Maybe Text -> IO ()
printApps configTxt = do
  atoms <- configToAtoms <$> Podenv.Config.loadConfig configTxt
  let showApp' (Podenv.Config.ApplicationRecord app) =
        "Application" <> maybe "" (\desc -> " (" <> desc <> ")") (app ^. appDescription)
      showFunc args app = "λ " <> args <> " → " <> showApp' app
      showArg a = "<" <> show a <> ">"
      showConfig = \case
        Podenv.Config.LitAppRes ar -> "Deployment: " <> showApp' (Podenv.Config.ApplicationRecord $ ar ^. arApplication)
        Podenv.Config.LitApp app -> showApp' app
        Podenv.Config.LamArg name f -> showFunc (show name) (f (showArg name))
        Podenv.Config.LamArg2 n1 n2 f -> showFunc (show n1 <> " " <> show n2) (f (showArg n1) (showArg n2))
        Podenv.Config.LamApp _ -> "λ app → app"
      showAtom (name, app) = name <> ": " <> showConfig app
  traverse_ (putTextLn . showAtom) atoms

configToAtoms :: Podenv.Config.Config -> [(Text, Podenv.Config.Atom)]
configToAtoms = sortOn fst . unConfig

printManifest :: Maybe Text -> IO ()
printManifest configTxt = do
  atoms <- configToAtoms <$> Podenv.Config.loadConfig configTxt
  env <- Podenv.Env.createEnv
  let run = Podenv.Runtime.createLocalhostRunEnv env
      gl = Podenv.Runtime.defaultGlobalEnv "/volume"
      addNL = Text.replace "--" "\\\n  --"
      doPrint name ar = do
        ctx <- Podenv.Runtime.appToContext run (Regular []) $ ar & (arMetadata . metaLabels) %~ Map.insert "podenv.selector" name
        flip runReaderT gl $ do
          putTextLn . addNL . showApp ar run =<< Podenv.Runtime.showCmd run Foreground ctx

      printAppContext :: (Text, Podenv.Config.Atom) -> IO ()
      printAppContext (name, atom) = do
        putTextLn name
        let mkAR = Podenv.Config.defaultAppRes . Podenv.Config.unRecord
        case atom of
          Podenv.Config.LitAppRes ar -> doPrint name ar
          Podenv.Config.LitApp app -> doPrint name (mkAR app)
          Podenv.Config.LamArg _ f -> doPrint name (mkAR $ f "a")
          Podenv.Config.LamArg2 _ _ f -> doPrint name (mkAR $ f "a" "b")
          Podenv.Config.LamApp _ -> putTextLn "lamapp"
  traverse_ printAppContext atoms
