{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

-- | The platform environment
module Podenv.Env where

import Lens.Family.TH (makeLenses)
import Podenv.Prelude

data AppEnv = AppEnv
  { _hostXdgRunDir :: Maybe FilePath,
    _hostHomeDir :: Maybe FilePath,
    _hostCwd :: FilePath,
    _hostUid :: UserID,
    _appHomeDir :: Maybe FilePath
  }
  deriving (Show)

$(makeLenses ''AppEnv)

type AppEnvT a = ReaderT AppEnv IO a

new :: IO AppEnv
new =
  AppEnv
    <$> lookupEnv "XDG_RUNTIME_DIR"
    <*> lookupEnv "HOME"
    <*> getCurrentDirectory
    <*> getRealUserID
    <*> pure Nothing
