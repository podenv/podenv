{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

-- | The platform environment
module Podenv.Env (
    SocketName (..),
    AppEnvState (UnknownState),
    AppEnvT,
    runAppEnv,
    isNVIDIAEnabled,
    getVideoDevices,
    getCertLocation,
    createEnv,

    -- * Lenses
    envHostXdgRunDir,
    envHostWaylandSocket,
    envHostDisplay,
    envHostSSHAgent,
    envHostHomeDir,
    envHostCwd,
    envHostUid,
    envAppHomeDir,

    -- * For testing
    AppEnv (..),
) where

import Control.Monad (msum)
import Data.List.NonEmpty qualified
import Data.Maybe qualified
import Data.Text qualified
import Data.Text qualified as Text
import Lens.Family.TH (makeLenses)
import Podenv.Dhall
import Podenv.Prelude

newtype SocketName = SocketName FilePath

{- | A data kind to keep track of the application environment such as its user uid and home directory.
  These values depends on the runtime, and it can be costly to resolve them.
  Thus we only query the information once before switching the state from Unknown to Resolved.
  See the 'resolveAppEnv' function below
-}
data AppEnvState = Resolved | UnknownState

data AppEnv (s :: AppEnvState) = AppEnv
    { _envHostXdgRunDir :: Maybe FilePath
    , _envHostWaylandSocket :: Maybe SocketName
    , _envHostDisplay :: String
    , _envHostSSHAgent :: Maybe FilePath
    , _envHostHomeDir :: Maybe FilePath
    , _envHostCwd :: FilePath
    , _envHostUid :: UserID
    , _envAppHomeDir :: Maybe FilePath
    , -- environment query
      _envGetAppHomeDir :: Application -> IO (Maybe FilePath)
    , _envGetVideoDevices :: IO [FilePath]
    , _envGetCertLocation :: IO (Maybe FilePath)
    , _envIsNVIDIAEnabled :: IO Bool
    }

$(makeLenses ''AppEnv)

resolveAppEnv :: AppEnv 'UnknownState -> Application -> IO (AppEnv 'Resolved)
resolveAppEnv env app = do
    appHomeDir <- resolveHomeDir
    pure $ env & envAppHomeDir .~ appHomeDir
  where
    resolveHomeDir
        | app ^. appCapabilities . capRoot = pure $ Just "/root"
        | otherwise = (env ^. envGetAppHomeDir) app

-- | This newtype hide the inner IO so that only the ones defined below are available.
newtype AppEnvT a = AppEnvT (ReaderT (AppEnv 'Resolved) IO a)
    deriving newtype (Functor, Applicative, Monad)
    deriving newtype (MonadReader (AppEnv 'Resolved))

isNVIDIAEnabled :: AppEnvT Bool
isNVIDIAEnabled = AppEnvT $ lift =<< askL envIsNVIDIAEnabled

getVideoDevices :: AppEnvT [FilePath]
getVideoDevices = AppEnvT $ lift =<< askL envGetVideoDevices

getCertLocation :: AppEnvT (Maybe FilePath)
getCertLocation = AppEnvT $ lift =<< askL envGetCertLocation

-- | 'runAppEnv' is the only available runner to perform a 'AppEnvT' action.
runAppEnv :: AppEnv 'UnknownState -> ApplicationResource -> (ApplicationResource -> AppEnvT a) -> IO a
runAppEnv env ar action = do
    appEnv <- resolveAppEnv env (ar ^. arApplication)
    let AppEnvT r = action ar
    runReaderT r appEnv

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

doGetVideoDevices :: IO [FilePath]
doGetVideoDevices = map toString . filter ("video" `Text.isPrefixOf`) . map toText <$> listDirectory "/dev"

doGetCertLocation :: IO (Maybe FilePath)
doGetCertLocation = runMaybeT $ Control.Monad.msum $ [checkEnv] <> (checkPath <$> paths)
  where
    checkEnv :: MaybeT IO FilePath
    checkEnv = do
        env <- lift $ lookupEnv "NIX_SSL_CERT_FILE"
        maybe mzero checkPath env
    checkPath :: FilePath -> MaybeT IO FilePath
    checkPath fp = do
        exist <- lift $ doesPathExist fp
        unless exist mzero
        pure fp
    -- Copied from profile.d/nix.sh
    paths =
        [ "/etc/pki/tls/certs/ca-bundle.crt"
        , "/etc/ssl/certs/ca-certificates.crt"
        , "/etc/ssl/ca-bundle.pem"
        , "/etc/ssl/certs/ca-bundle.crt"
        ]

createEnv :: IO (AppEnv 'UnknownState)
createEnv = do
    _envHostUid <- getRealUserID
    _envHostHomeDir <- lookupEnv "HOME"
    _envHostXdgRunDir <- lookupEnv "XDG_RUNTIME_DIR"
    _envHostWaylandSocket <- fmap SocketName <$> lookupEnv "WAYLAND_DISPLAY"
    _envHostDisplay <- fromMaybe ":0" <$> lookupEnv "DISPLAY"
    _envHostSSHAgent <- lookupEnv "SSH_AUTH_SOCK"
    _envHostCwd <- getCurrentDirectory
    let _envAppHomeDir = Nothing
        _envGetAppHomeDir app = case app ^. appRuntime of
            Container cb -> pure $ toString <$> cb ^. cbImage_home
            Rootfs fp -> getRootfsHome _envHostUid _envHostHomeDir (toString fp)
            Nix _ -> pure _envHostHomeDir
            Image _ -> pure Nothing
        _envGetVideoDevices = doGetVideoDevices
        _envGetCertLocation = doGetCertLocation
        _envIsNVIDIAEnabled = doesPathExist "/dev/nvidiactl"
    pure $ AppEnv{..}
