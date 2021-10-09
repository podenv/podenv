{-# LANGUAGE OverloadedStrings #-}

-- | Static test
module Main (main) where

import qualified Data.Set (fromList)
import qualified Language.Haskell.HLint as HLint
import System.Exit (exitFailure)
import qualified Weeder.Config
import qualified Weeder.Main (mainWithConfig)

main :: IO ()
main = runWeeder >> runHLint

runHLint :: IO ()
runHLint = do
  ideas <- HLint.hlint ["./src/", "./app/", "./test/"]
  case ideas of
    [] -> pure ()
    _ -> exitFailure

runWeeder :: IO ()
runWeeder = Weeder.Main.mainWithConfig ".hie" ["./."] True config
  where
    config =
      Weeder.Config.Config
        { Weeder.Config.rootPatterns =
            Data.Set.fromList (map (\c -> "^" <> c <> "$") roots),
          Weeder.Config.typeClassRoots = True
        }
    roots =
      [ "Main.main",
        "Podenv.Dhall.\\$sel.*",
        -- lenses
        "Podenv.Dhall.Capabilities",
        "Podenv.Dhall.ContainerBuild",
        "Podenv.Dhall.Provider",
        "Podenv.Dhall.Runtime",
        "Podenv.Dhall.SystemConfig",
        "Podenv.Context.hostname",
        "Podenv.Context.image",
        "Podenv.Env.hostCwd",
        "Podenv.Env.hostHomeDir",
        "Podenv.Env.hostUid",
        "Podenv.Env.hostXdgRunDir",
        -- TBD
        "Podenv.Dhall.sysData_volumes_dir",
        "Podenv.Dhall.capKerberos",
        "Podenv.Dhall.capGitroot"
      ]
