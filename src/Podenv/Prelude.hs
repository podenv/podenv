-- | Common functions
module Podenv.Prelude
  ( module Relude,
    foldM,
    lookup,
    getEnv,
    orDie,
    mayFail,

    -- * xdg
    getCacheDir,
    getConfigDir,

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
import System.Environment (getEnv)
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

getConfigDir :: IO FilePath
getConfigDir = getXdgDirectory XdgConfig "podenv"
