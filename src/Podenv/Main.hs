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

import Data.Text qualified
import Data.Version (showVersion)
import Options.Applicative hiding (command)
import Paths_podenv (version)
import Podenv.Capability qualified
import Podenv.Config qualified
import Podenv.Dhall
import Podenv.Env
import Podenv.Prelude
import Podenv.Runtime (Name (..), RuntimeEnv (..), runtimeBackend)
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

  (app, mode, ctxName, re) <- cliConfigLoad cli
  env <- createLocalhostEnv (app ^. appRuntime)
  let run = Podenv.Runtime.createLocalhostRunEnv env app ctxName
  ctx <- Podenv.Runtime.appToContext run mode

  if showApplication
    then putTextLn $ showApp app (Podenv.Runtime.showRuntimeCmd re ctx (runtimeBackend run))
    else flip runReaderT re do
      Podenv.Runtime.buildRuntime run
      when update $ Podenv.Runtime.updateRuntime run
      Podenv.Runtime.execute run ctx

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
    showManifest :: Bool,
    showDhallEnv :: Bool,
    showApplication :: Bool,
    configExpr :: Maybe Text,
    -- runtime env:
    update :: Bool,
    verbose :: Bool,
    detach :: Bool,
    -- app modifiers:
    capsOverride :: [Capabilities -> Capabilities],
    shell :: Bool,
    namespace :: Maybe Text,
    name :: Maybe Text,
    cliEnv :: [Text],
    volumes :: [Text],
    -- app selector and arguments:
    selector :: Maybe Text,
    cliExtraArgs :: [Text]
  }

-- WARNING: when adding strOption, update the 'strOptions' list
cliParser :: Parser CLI
cliParser =
  CLI
    -- action modes:
    <$> switch (long "list" <> help "List available applications")
    <*> switch (long "list-caps" <> help "List available capabilities")
    <*> switch (long "manifest" <> hidden)
    <*> switch (long "dhall-env" <> hidden)
    <*> switch (long "show" <> help "Show the environment without running it")
    <*> optional (strOption (long "config" <> help "A config expression"))
    -- runtime env:
    <*> switch (long "update" <> help "Update the runtime")
    <*> switch (long "verbose" <> help "Increase verbosity")
    <*> switch (long "detach" <> hidden)
    -- app modifiers:
    <*> capsParser
    <*> switch (long "shell" <> help "Start a shell instead of the application command")
    <*> optional (strOption (long "namespace" <> help "Share a network ns"))
    <*> optional (strOption (long "name" <> metavar "NAME" <> help "Container name"))
    <*> many (strOption (long "env" <> metavar "ENV" <> help "Extra env 'KEY=VALUE'"))
    <*> many (strOption (long "volume" <> short 'v' <> metavar "VOLUME" <> help "Extra volumes 'volume|hostPath[:containerPath]'"))
    <*> optional (strArgument (metavar "APP" <> help "Application config name or image:name or nix:expr"))
    <*> many (strArgument (metavar "ARGS" <> help "Application args"))

-- | List of strOption that accept an argument (that is not the selector)
strOptions :: [String]
strOptions = ["--config", "--namespace", "--name", "--env", "--volume", "-v"]

-- | Parse all capabilities toggles
capsParser :: Parser [Capabilities -> Capabilities]
capsParser = catMaybes <$> traverse mkCapParser Podenv.Capability.capsAll
  where
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

-- | Load the config
cliConfigLoad :: CLI -> IO (Application, Podenv.Capability.Mode, Name, RuntimeEnv)
cliConfigLoad cli@CLI {..} = do
  system <- Podenv.Config.loadSystem
  -- The volumes dir may be provided by the system config, otherwise default to ~/.local/share/podenv/volumes
  volumesDir <- case data_volumes_dir system of
    Just fp -> pure $ toString fp
    Nothing -> getDataDir >>= \fp -> pure $ fp </> "volumes"

  config <- Podenv.Config.load selector configExpr
  (extraArgs, baseApp) <- mayFail $ Podenv.Config.select config (maybeToList selector <> cliExtraArgs)
  let app = cliPrepare cli baseApp
      name' = Name $ fromMaybe (app ^. appName) name
      re = RuntimeEnv {verbose, detach, system, volumesDir, config = Just config}
      mode = if shell then Podenv.Capability.Shell else Podenv.Capability.Regular extraArgs
  pure (app, mode, name', re)

-- | Apply the CLI argument to the application
cliPrepare :: CLI -> Application -> Application
cliPrepare CLI {..} = setShell . setEnvs . setVolumes . setCaps . setNS
  where
    setNS = maybe id (appNamespace ?~) namespace

    setShell = bool id setShellCap shell
    setShellCap = appCapabilities %~ (capTerminal .~ True) . (capInteractive .~ True)

    setEnvs app' = foldr (\v -> appEnviron %~ (v :)) app' cliEnv

    setVolumes app' = foldr (\v -> appVolumes %~ (v :)) app' volumes

    setCaps app' = foldr (appCapabilities %~) app' capsOverride

showApp :: Application -> Text -> Text
showApp Application {..} cmd = unlines infos
  where
    infos =
      ["[+] Capabilities", unwords appCaps, ""]
        <> ["[+] Command", cmd]
    appCaps = concatMap showCap Podenv.Capability.capsAll
    showCap Podenv.Capability.Cap {..} =
      [capName | capabilities ^. capLens]

printCaps :: IO ()
printCaps = do
  putText $ unlines $ sort $ map showCap Podenv.Capability.capsAll
  where
    showCap Podenv.Capability.Cap {..} =
      let sep = if Data.Text.length capName < 8 then "\t\t" else "\t"
       in capName <> sep <> capDescription

printApps :: Maybe Text -> IO ()
printApps configTxt = do
  atoms <- configToAtoms <$> Podenv.Config.load Nothing configTxt
  let showApp' (Podenv.Config.ApplicationRecord app) =
        "Application" <> maybe "" (\desc -> " (" <> desc <> ")") (app ^. appDescription)
      showFunc args app = "λ " <> args <> " → " <> showApp' app
      showArg a = "<" <> show a <> ">"
      showConfig = \case
        Podenv.Config.Lit app -> showApp' app
        Podenv.Config.LamArg name f -> showFunc (show name) (f (showArg name))
        Podenv.Config.LamArg2 n1 n2 f -> showFunc (show n1 <> " " <> show n2) (f (showArg n1) (showArg n2))
        Podenv.Config.LamApp _ -> "λ app → app"
      showAtom (name, app) = name <> ": " <> showConfig app
  traverse_ (putTextLn . showAtom) atoms

configToAtoms :: Podenv.Config.Config -> [(Text, Podenv.Config.Atom)]
configToAtoms = \case
  Podenv.Config.ConfigDefault ar -> [("default", Podenv.Config.Lit ar)]
  Podenv.Config.ConfigApplication atom -> [("default", atom)]
  Podenv.Config.ConfigApplications xs -> sortOn fst xs

printManifest :: Maybe Text -> IO ()
printManifest configTxt = do
  atoms <- configToAtoms <$> Podenv.Config.load Nothing configTxt
  let re = Podenv.Runtime.defaultGlobalEnv "/volume"
      addNL = Data.Text.replace "--" "\\\n  --"
      doPrint name app = do
        putTextLn name
        env <- createLocalhostEnv (app ^. appRuntime)
        let run = Podenv.Runtime.createLocalhostRunEnv env app (Name name)
        ctx <- Podenv.Runtime.appToContext run (Regular [])
        putTextLn . addNL $ showApp app (Podenv.Runtime.showRuntimeCmd re Foreground ctx (runtimeBackend run))

      printAppContext :: (Text, Podenv.Config.Atom) -> IO ()
      printAppContext (name, atom) = case atom of
        Podenv.Config.Lit app -> doPrint name (Podenv.Config.unRecord app)
        Podenv.Config.LamArg _ f -> doPrint name (Podenv.Config.unRecord $ f "a")
        Podenv.Config.LamArg2 _ _ f -> doPrint name (Podenv.Config.unRecord $ f "a" "b")
        Podenv.Config.LamApp _ -> putTextLn $ name <> ": lamapp"
  traverse_ (printAppContext) atoms
