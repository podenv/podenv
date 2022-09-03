{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Podenv.Notifications
  ( Notifier (..),
    Severity (..),
    Output (..),
    pickNotifier,

    -- * helpers
    defaultNotifier,
    consoleNotifier,
    zenityNotifier,
  )
where

import Podenv.Prelude
import System.Exit (ExitCode (..))
import System.Posix.IO qualified
import System.Posix.Terminal qualified
import System.Process.Typed qualified as P
import System.Timeout (timeout)

data Severity = Info | Error deriving (Show)

data Notifier = Notifier
  { sendMessage :: Severity -> Text -> IO (),
    askConfirm :: Text -> IO Bool
  }

defaultNotifier :: Notifier
defaultNotifier = Notifier (\s m -> putStrLn (show s <> " " <> toString m)) (const (pure False))

consoleNotifier :: Notifier
consoleNotifier = Notifier consoleMessage consoleRead
  where
    consoleMessage sev msg = hPutStrLn stderr (pre <> " " <> toString msg)
      where
        pre = case sev of
          Info -> "[+]"
          Error -> "[ERROR]"
    consoleRead prompt = do
      putText $ prompt <> " [Yn]? "
      hFlush stdout
      resp <- getLine
      pure $ resp `elem` ["", "Y", "YES", "y", "yes"]

zenityNotifier :: Notifier
zenityNotifier = Notifier zenityMessage zenityRead
  where
    zenityMessage sev msg = do
      sendMessage consoleNotifier sev msg
      void $ timeout 10_000_000 do
        P.runProcess_ $ P.proc "zenity" ["--title", "podenv", arg, "--text", toString msg]
      where
        arg = case sev of
          Info -> "--info"
          Error -> "--error"
    zenityRead prompt =
      fromMaybe False <$> timeout 20_000_000 ((== ExitSuccess) <$> P.runProcess cmd)
      where
        cmd = P.proc "zenity" ["--title", "podenv", "--question", "--text", toString prompt <> "?"]

hasZenity :: IO Bool
hasZenity = not . null <$> findExecutable "zenity"

isInteractive :: IO Bool
isInteractive = System.Posix.Terminal.queryTerminal System.Posix.IO.stdInput

data Output = Display | Console deriving (Eq, Show)

pickNotifier :: Output -> IO Notifier
pickNotifier output = do
  interactive <- isInteractive
  if interactive || output == Console
    then pure consoleNotifier
    else do
      zenity <- hasZenity
      pure $
        if zenity
          then zenityNotifier
          else consoleNotifier
