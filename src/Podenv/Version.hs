{-# LANGUAGE TemplateHaskell #-}

-- | Podenv version
module Podenv.Version (version) where

import Development.GitRev (gitDirty, gitHash)

version :: String
version = $(gitHash) <> dirty
  where
    dirty
      | $(gitDirty) = "-dirty"
      | otherwise = ""
