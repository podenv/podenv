{-# LANGUAGE CPP #-}
-- | Common functions
module Podenv.Prelude
  ( module Relude,
    foldM,
    lookup,
    getEnv,
    orDie,
    mayFail,

    -- * base env
#if !MIN_VERSION_relude(1,0,0)
    getArgs,
    lookupEnv,
#endif

    -- * xdg
    getCacheDir,
    getConfigDir,
    getDataDir,

    -- * directory
    createDirectoryIfMissing,
    getCurrentDirectory,
    doesFileExist,
    doesPathExist,
    (</>),
    takeFileName,
    takeDirectory,
    hasTrailingPathSeparator,
    listDirectory,

    -- * posix
    UserID,
    getRealUserID,

    -- * lens
    Lens',
    (^.),
    (.~),
    (?~),
    (%~),
    setWhenNothing,
  )
where

import Control.Monad (foldM)
import Lens.Family (ASetter, set, (%~), (.~), (^.))
import Relude
import Relude.Extra.Lens (Lens')
import System.Directory
import System.Environment
import System.FilePath.Posix (hasTrailingPathSeparator, takeDirectory, takeFileName, (</>))
import System.IO (hPutStrLn)
import System.Posix.Types (UserID)
import System.Posix.User (getRealUserID)

orDie :: Maybe a -> Text -> Either Text a
orDie (Just a) _ = Right a
orDie Nothing e = Left e

mayFail :: Either Text a -> IO a
mayFail (Right a) = pure a
mayFail (Left msg) = hPutStrLn stderr ("Error: " <> toString msg) >> exitFailure >> fail "over"

(?~) :: ASetter s t a (Maybe b) -> b -> s -> t
l ?~ b = set l (Just b)

setWhenNothing :: ASetter s t (Maybe b) (Maybe b) -> b -> s -> t
l `setWhenNothing` b = l %~ maybe (Just b) Just

getCacheDir :: IO FilePath
getCacheDir = getXdgDirectory XdgCache "podenv"

getDataDir :: IO FilePath
getDataDir = getXdgDirectory XdgData "podenv"

getConfigDir :: IO FilePath
getConfigDir = getXdgDirectory XdgConfig "podenv"
