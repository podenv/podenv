{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | This modules contains logic to perform application runtime build
module Podenv.Build
  ( initBuildEnv,
    prepare,
    execute,
    BuildEnv (..),
  )
where

import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Podenv.Application (Mode (Regular), prepare)
import Podenv.Config (Builder (..), BuilderContainer (..), BuilderNix (..))
import Podenv.Context (Name (..))
import Podenv.Dhall
import Podenv.Prelude
import qualified Podenv.Runtime
import System.Directory (renameFile)
import System.Exit (ExitCode (ExitSuccess))
import qualified System.Process.Typed as P

-- | A build env contains action to be performed before preparation and execution
data BuildEnv = BuildEnv
  { beName :: Text,
    beInfos :: Text,
    -- | Creates the modified application, e.g. with a new runtime image name
    bePrepare :: IO (Bool, Application),
    -- | Builds the runtime
    beBuild :: IO (),
    -- | Updates the runtime
    beUpdate :: IO ()
  }

-- | Create the build env
initBuildEnv :: Podenv.Runtime.RuntimeEnv -> Application -> Builder -> BuildEnv
initBuildEnv re app builder' = case (runtime app, builder') of
  (Nix expr, NixBuilder nb) -> initNix re app expr nb
  (Container _, ContainerBuilder cb) -> initContainer app cb
  (_, _) -> error "Application runtime does not match builder"

-- | Prepare the application when necessary
prepare :: Maybe BuildEnv -> Application -> IO (Bool, Application)
prepare Nothing app = pure (True, app)
prepare (Just BuildEnv {..}) _ = bePrepare

-- | Build the runtime when necessary
execute :: Maybe BuildEnv -> IO ()
execute Nothing = pure ()
execute (Just BuildEnv {..}) = do
  putTextLn $ "Building app with: " <> beName
  beBuild

-- | Container build env:
initContainer :: Application -> BuilderContainer -> BuildEnv
initContainer baseApp BuilderContainer {..} = BuildEnv {..}
  where
    -- buildenv basic info:
    beName = bcName
    beInfos = "# Containerfile " <> imageName <> "\n" <> fileContent
    containerBuild = bcBuild
    fileContent = containerBuild ^. cbContainerfile
    imageHash = toText . SHA.showDigest . SHA.sha256 . encodeUtf8 $ fileContent

    -- The image name can be set by the container build,
    -- otherwise it default to the Containerfile hash
    imageName =
      "localhost/" <> fromMaybe imageHash (containerBuild ^. cbImage_name)
    fileName = imageNameToFilePath imageName

    setHome = case containerBuild ^. cbImage_home of
      Just home -> appHome ?~ home
      Nothing -> id
    setImage = appRuntime .~ Image imageName

    bePrepare = do
      imageReady <- checkImageExist imageName
      pure (imageReady, baseApp & setHome . setImage)

    beBuild = do
      buildImage imageName fileName fileContent (containerBuild ^. cbImage_volumes)

    beUpdate = case containerBuild ^. cbImage_update of
      Nothing -> error "The container is missing the `image_update` attribute"
      Just cmd -> do
        buildImage
          imageName
          (fileName <> "-update")
          (unlines ["FROM " <> imageName, "RUN " <> cmd])
          (containerBuild ^. cbImage_volumes)

-- | Nix build env:
initNix :: Podenv.Runtime.RuntimeEnv -> Application -> Text -> BuilderNix -> BuildEnv
initNix re baseApp expr BuilderNix {..} = BuildEnv {..}
  where
    -- buildenv basic info:
    beName = bnName
    beInfos = "# Nix expr:\n" <> expr
    name = baseApp ^. appName
    fileName = toString $ "nix_" <> name

    -- container image info to use for nix runtime
    containerBuild = case buildNixApp ^. appRuntime of
      Container cb -> cb
      _ -> error "Nix build must have a Container runtime"
    containerfile = containerBuild ^. cbContainerfile
    imageName =
      fromMaybe (error "Nix build must have a image_name attribute") $
        containerBuild ^. cbImage_name

    -- update the application with nix requirements
    setImage = appRuntime .~ Image imageName
    addVolumes = appVolumes %~ (\xs -> (containerBuild ^. cbImage_volumes) <> xs)
    profileDir = toText $ "/nix/var/nix/profiles/podenv" </> toString name
    addEnv = appEnviron %~ ("PATH=" <> toText profileDir <> "/bin:/bin" :)

    -- the app to run to setup the nix-store volume and instantiate the profile
    builderApp = buildNixApp & setImage . addVolumes

    setHome = case containerBuild ^. cbImage_home of
      Just home -> appHome ?~ home
      Nothing -> id

    bePrepare = do
      built <- checkIfBuilt fileName expr
      pure (built, baseApp & setHome . addVolumes . addEnv . setImage)

    beBuild = do
      cacheDir <- getCacheDir

      let setupMark = cacheDir </> toString ("nix_" <> buildNixApp ^. appName) <> ".ready"
      setupDone <- doesFileExist setupMark
      unless setupDone $ do
        buildImage imageName (imageNameToFilePath imageName) containerfile []
        runApp re builderApp
        Text.writeFile setupMark ""

      -- prepare and cleanup leftover
      runApp re $
        builderApp & appCommand
          .~ ["sh", "-c", "mkdir -p /nix/var/nix/profiles/podenv; rm -f " <> profileDir <> "-[0-9]-link"]

      let installCommand =
            ["nix-env", "--profile", profileDir, "--install"]
              <> ["--remove-all", "-E", "_:" <> expr]

      runApp re $ builderApp & appCommand .~ installCommand

      -- save that the build succeeded
      Text.writeFile (cacheDir </> fileName) expr

    beUpdate = error "Nix update is not implemented (yet)"

-- | Build a container image
buildImage :: Text -> FilePath -> Text -> [Text] -> IO ()
buildImage imageName fileName containerfile volumes = do
  uid <- getRealUserID
  cacheDir <- getCacheDir
  createDirectoryIfMissing True cacheDir
  let want = fileName <> ".want"
      wantfp = cacheDir </> want
  Text.writeFile wantfp containerfile
  -- podman build does not support regular volume, lets ensure absolute path
  volumesArgs <- traverse (mkVolumeArg cacheDir) volumes
  let buildArgs =
        ["build"]
          <> ["-t", toString imageName]
          <> ["--build-arg", "USER_UID=" <> show uid]
          <> map toString volumesArgs
          <> ["-f", want, cacheDir]
      cmd = Podenv.Runtime.podman buildArgs
  -- putTextLn $ "Building " <> imageName <> " with " <> toText want <> ": " <> show cmd
  P.runProcess_ cmd

  -- save that the build succeeded
  renameFile wantfp (cacheDir </> fileName)
  where
    mkVolumeArg :: FilePath -> Text -> IO Text
    mkVolumeArg cacheDir volume = do
      createDirectoryIfMissing True hostPath
      pure $ "-v=" <> toText hostPath <> ":" <> containerPath <> ":Z"
      where
        (p1, p2) = Text.break (== ':') volume
        hostPath = cacheDir </> toString p1
        containerPath = Text.drop 1 p2

checkImageExist :: Text -> IO Bool
checkImageExist imageName = do
  res <- P.runProcess (Podenv.Runtime.podman ["image", "exists", Text.unpack imageName])
  pure $ res == ExitSuccess

imageNameToFilePath :: Text -> FilePath
imageNameToFilePath imageName = "Containerfile_" <> toString (imageNameToFP imageName)
  where
    imageNameToFP = Text.replace "/" "_" . Text.replace ":" "-"

runApp :: Podenv.Runtime.RuntimeEnv -> Application -> IO ()
runApp re app = do
  ctx <- Podenv.Application.prepare app Podenv.Application.Regular (Name $ app ^. appName)
  Podenv.Runtime.execute re ctx

checkIfBuilt :: FilePath -> Text -> IO Bool
checkIfBuilt filename expected = do
  cacheDir <- getCacheDir
  current <- readFileM (cacheDir </> filename)
  pure $ current == expected
