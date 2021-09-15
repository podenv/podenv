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

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Podenv.Application (prepare)
import Podenv.Config (Builder (..), BuilderContainer (..), BuilderNix (..))
import Podenv.Dhall
import Podenv.Prelude
import qualified Podenv.Runtime (defaultRuntimeEnv, execute, podman)
import System.Directory (renameFile)
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
initBuildEnv :: Application -> Builder -> BuildEnv
initBuildEnv app builder' = case (runtime app, builder') of
  (Nix expr, NixBuilder nb) -> initNix app expr nb
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
    fileContent = containerfile containerBuild

    -- The image name can be set by the container build,
    -- otherwise it default to the app name (e.g. when using distro template)
    imageName =
      "localhost/" <> fromMaybe (baseApp ^. appName) (containerBuild ^. cbImage_name)
    fileName = imageNameToFilePath imageName

    setHome = case containerBuild ^. cbImage_home of
      Just home -> appHome ?~ home
      Nothing -> id
    setImage = appRuntime .~ Image imageName

    bePrepare = do
      built <- checkIfBuilt fileName fileContent
      pure (built, baseApp & setHome . setImage)

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
initNix :: Application -> Text -> BuilderNix -> BuildEnv
initNix baseApp expr BuilderNix {..} = BuildEnv {..}
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
    addVolumes = appVolumes %~ (\xs -> ("nix-profiles" <> ":/profile") : (containerBuild ^. cbImage_volumes) <> xs)
    profileBase = toText $ "/profile" </> toString name
    addEnv = appEnviron %~ ("PATH=" <> toText profileBase <> "/bin:/bin" :)

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
        runApp builderApp
        Text.writeFile setupMark ""

      runApp $
        builderApp
          & (appCommand .~ ["nix-env", "-i", "-p", profileBase, "-E", "_: " <> expr])

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

imageNameToFilePath :: Text -> FilePath
imageNameToFilePath imageName = "Containerfile_" <> toString (imageNameToFP imageName)
  where
    imageNameToFP = Text.replace "/" "_" . Text.replace ":" "-"

runApp :: Application -> IO ()
runApp app = do
  ctx <- Podenv.Application.prepare app
  Podenv.Runtime.execute Podenv.Runtime.defaultRuntimeEnv ctx

checkIfBuilt :: FilePath -> Text -> IO Bool
checkIfBuilt filename expected = do
  cacheDir <- getCacheDir
  current <- readFileM (cacheDir </> filename)
  pure $ current == expected

readFileM :: FilePath -> IO Text
readFileM fp' = do
  exist <- liftIO $ doesFileExist fp'
  if exist
    then liftIO $ Text.readFile fp'
    else pure ""
