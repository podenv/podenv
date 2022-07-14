{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

-- | The platform environment
module Podenv.Env where

import Data.List.NonEmpty qualified
import Data.Maybe qualified
import Data.Text qualified
import Lens.Family.TH (makeLenses)
import Podenv.Prelude

data AppEnv = AppEnv
  { _hostXdgRunDir :: Maybe FilePath,
    _hostHomeDir :: Maybe FilePath,
    _hostCwd :: FilePath,
    _hostUid :: UserID,
    _appHomeDir :: Maybe FilePath,
    _rootfsHome :: FilePath -> IO (Maybe FilePath)
  }

$(makeLenses ''AppEnv)

type AppEnvT a = ReaderT AppEnv IO a

-- | Get the current uid home path in the rootfs
getRootfsHome :: UserID -> Maybe FilePath -> FilePath -> IO (Maybe FilePath)
getRootfsHome _ (Just hostHome) "/" = pure $ Just hostHome
getRootfsHome uid _ fp = do
  passwd <- readFileM (fp </> "etc/passwd")
  pure $
    toString . Data.List.NonEmpty.head
      <$> Data.List.NonEmpty.nonEmpty (Data.Maybe.mapMaybe isUser $ Podenv.Prelude.lines passwd)
  where
    isUser l = case Data.Text.splitOn ":" l of
      (_ : _ : uid' : _ : _ : home : _) | readMaybe (toString uid') == Just uid -> Just home
      _ -> Nothing

new :: IO AppEnv
new = do
  uid <- getRealUserID
  home <- lookupEnv "HOME"
  AppEnv
    <$> lookupEnv "XDG_RUNTIME_DIR"
    <*> pure home
    <*> getCurrentDirectory
    <*> pure uid
    <*> pure Nothing
    <*> pure (getRootfsHome uid home)
