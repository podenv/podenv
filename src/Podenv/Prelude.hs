{-# LANGUAGE CPP #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | Common functions
module Podenv.Prelude (
    module Relude,
    foldM,
    lookup,
    getEnv,
    orDie,
    mayFail,

    -- * io
    readFileM,
    deleteFileM,
    hPutStrLn,

    -- * base env
    getExecutablePath,

    -- * xdg
    getCacheDir,
    getConfigDir,
    getDataDir,

    -- * directory
    createDirectoryIfMissing,
    getCurrentDirectory,
    doesFileExist,
    doesPathExist,
    doesSymlinkExist,
    pathIsSymbolicLink,
    findExecutable,
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
    askL,
    lensName,
) where

import Control.Exception qualified
import Control.Monad (foldM)
import Data.Char (toUpper)
import Data.Text.IO qualified
import Lens.Family (ASetter, set, (%~), (.~), (^.))
import Relude
import Relude.Extra.Lens (Lens')
import System.Directory
import System.Environment
import System.FilePath.Posix (hasTrailingPathSeparator, takeDirectory, takeFileName, (</>))
import System.IO
import System.Posix.Files qualified
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

askL :: (MonadReader s m) => Lens' s a -> m a
askL l = do
    e <- ask
    pure $ e ^. l

getCacheDir :: IO FilePath
getCacheDir = getXdgDirectory XdgCache "podenv"

getDataDir :: IO FilePath
getDataDir = getXdgDirectory XdgData "podenv"

getConfigDir :: IO FilePath
getConfigDir = getXdgDirectory XdgConfig "podenv"

readFileM :: FilePath -> IO Text
readFileM fp' = do
    exist <- liftIO $ doesFileExist fp'
    if exist
        then liftIO $ Data.Text.IO.readFile fp'
        else pure ""

deleteFileM :: FilePath -> IO ()
deleteFileM fp' = do
    exist <- liftIO $ doesFileExist fp'
    when exist $ removeFile fp'

doesSymlinkExist :: FilePath -> IO Bool
doesSymlinkExist fp =
    either (const False) (const True) <$> checkFp
  where
    checkFp :: IO (Either Control.Exception.SomeException FilePath)
    checkFp = Control.Exception.try $ System.Posix.Files.readSymbolicLink fp

{- | Create a lens name

 >>> lensName "name" "name"
 Nothing
-}
lensName :: String -> String -> Maybe String
lensName prefix field
    | field `Prelude.elem` manualFields = Nothing
    | otherwise = Just $ prefix <> [toUpper $ Prelude.head field] <> Prelude.tail field
  where
    -- these fields have conflicts and their lens need to be implemented manually
    manualFields = ["name", "namespace", "volumes", "network", "labels", "apiVersion", "kind"]
