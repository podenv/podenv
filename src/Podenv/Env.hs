{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

-- | The platform environment
module Podenv.Env
  ( AppEnv (..),
    SocketName (..),
    AppEnvT,
    createLocalhostEnv,
    runAppEnv,
    isNVIDIAEnabled,
    ensureResolvConf,
    getVideoDevices,
    getCertLocation,
  )
where

import Control.Monad (msum)
import Data.List.NonEmpty qualified
import Data.Maybe qualified
import Data.Text qualified
import Data.Text qualified as Text
import Podenv.Context hiding (Container, uid)
import Podenv.Context qualified as Ctx
import Podenv.Dhall hiding (runtime)
import Podenv.Prelude
import System.Posix.Files qualified

newtype SocketName = SocketName FilePath

data AppEnv = AppEnv
  { _hostXdgRunDir :: Maybe FilePath,
    _hostWaylandSocket :: Maybe SocketName,
    _hostDisplay :: String,
    _hostSSHAgent :: Maybe FilePath,
    _hostHomeDir :: Maybe FilePath,
    _hostCwd :: FilePath,
    _hostUid :: UserID,
    _appHomeDir :: Maybe FilePath,
    -- environment query
    _isNVIDIAEnabled :: IO Bool,
    _ensureResolvConf :: FilePath -> IO (Context -> Context),
    _getVideoDevices :: IO [FilePath],
    _getCertLocation :: IO (Maybe FilePath)
  }

-- | This newtype hide the inner IO so that only the ones defined below are available.
newtype AppEnvT a = AppEnvT (ReaderT AppEnv IO a)
  deriving newtype (Functor, Applicative, Monad)
  deriving newtype (MonadReader AppEnv)

isNVIDIAEnabled :: AppEnvT Bool
isNVIDIAEnabled = AppEnvT $ lift =<< asks _isNVIDIAEnabled

getVideoDevices :: AppEnvT [FilePath]
getVideoDevices = AppEnvT $ lift =<< asks _getVideoDevices

ensureResolvConf :: FilePath -> AppEnvT (Context -> Context)
ensureResolvConf fp = AppEnvT $ do
  ensure <- asks _ensureResolvConf
  lift $ ensure fp

getCertLocation :: AppEnvT (Maybe FilePath)
getCertLocation = AppEnvT $ lift =<< asks _getCertLocation

runAppEnv :: AppEnv -> AppEnvT a -> IO a
runAppEnv env (AppEnvT r) = runReaderT r env

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

doEnsureResolvConf :: FilePath -> IO (Ctx.Context -> Ctx.Context)
doEnsureResolvConf fp
  -- When using host rootfs, then we need to mount /etc/resolv.conf target when it is a symlink
  | fp == "/" = do
    symlink <- System.Posix.Files.isSymbolicLink <$> System.Posix.Files.getSymbolicLinkStatus "/etc/resolv.conf"
    if symlink
      then do
        realResolvConf <- getSymlinkPath
        pure $ Ctx.addMount realResolvConf $ Ctx.roHostPath realResolvConf
      else pure id
  -- Otherwise we can just mount it directly
  | otherwise = pure $ Ctx.addMount "/etc/resolv.conf" (Ctx.roHostPath "/etc/resolv.conf")
  where
    getSymlinkPath = do
      realResolvConf <- System.Posix.Files.readSymbolicLink "/etc/resolv.conf"
      pure $
        if "../" `isPrefixOf` realResolvConf
          then drop 2 realResolvConf
          else realResolvConf

doGetVideoDevices :: IO [FilePath]
doGetVideoDevices = map toString . filter ("video" `Text.isPrefixOf`) . map toText <$> listDirectory "/dev"

doGetCertLocation :: IO (Maybe FilePath)
doGetCertLocation = runMaybeT $ Control.Monad.msum $ [checkEnv] <> (checkPath <$> paths)
  where
    checkEnv :: MaybeT IO FilePath
    checkEnv = do
      env <- lift $ lookupEnv "NIX_SSL_CERT_FILE"
      case env of
        Just fp -> checkPath fp
        Nothing -> mzero
    checkPath :: FilePath -> MaybeT IO FilePath
    checkPath fp = do
      exist <- lift $ doesPathExist fp
      unless exist mzero
      pure fp
    -- Copied from profile.d/nix.sh
    paths =
      [ "/etc/pki/tls/certs/ca-bundle.crt",
        "/etc/ssl/certs/ca-certificates.crt",
        "/etc/ssl/ca-bundle.pem",
        "/etc/ssl/certs/ca-bundle.crt"
      ]

createLocalhostEnv :: Runtime -> IO AppEnv
createLocalhostEnv r = do
  _hostUid <- getRealUserID
  _hostHomeDir <- lookupEnv "HOME"
  _hostXdgRunDir <- lookupEnv "XDG_RUNTIME_DIR"
  _hostWaylandSocket <- fmap SocketName <$> lookupEnv "WAYLAND_DISPLAY"
  _hostDisplay <- fromMaybe ":0" <$> lookupEnv "DISPLAY"
  _hostSSHAgent <- lookupEnv "SSH_AUTH_SOCK"
  _hostCwd <- getCurrentDirectory
  _appHomeDir <- case r of
    Container cb -> pure $ toString <$> cb ^. cbImage_home
    Rootfs fp -> getRootfsHome _hostUid _hostHomeDir (toString fp)
    Nix _ -> pure _hostHomeDir
    Image _ -> pure Nothing
  let _isNVIDIAEnabled = doesPathExist "/dev/nvidiactl"
      _ensureResolvConf = doEnsureResolvConf
      _getVideoDevices = doGetVideoDevices
      _getCertLocation = doGetCertLocation
  pure $ AppEnv {..}
