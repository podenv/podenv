{-# LANGUAGE TemplateHaskell #-}

-- | Podenv version
module Podenv.Version (version) where

import Development.GitRev (gitDirty, gitHash)
import Language.Haskell.TH.Env (envQ)

version :: String
version = case fromEnv of
  Just v -> v
  Nothing -> $(gitHash) <> dirty
  where
    fromEnv :: Maybe String
    fromEnv = $$(envQ "PODENV_COMMIT")
    dirty
      | $(gitDirty) = "-dirty"
      | otherwise = ""
