{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | This modules contains logic to perform application runtime build
module Podenv.Build
  ( prepare,
    BuildEnv (..),
  )
where

import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Podenv.Config qualified
import Podenv.Dhall
import Podenv.Image
import Podenv.Prelude
import Podenv.Runtime qualified
import System.Directory (doesDirectoryExist, renameFile)
import System.Exit (ExitCode (ExitSuccess))
import System.Process.Typed qualified as P

-- | Helper function to run a standalone app, usefull for local build
type AppRunner = Application -> IO ()

-- | A build env contains action to be performed before preparation and execution
data BuildEnv = BuildEnv
  { beInfos :: Text,
    -- | Builds the runtime
    beEnsure :: AppRunner -> IO (),
    -- | Updates the runtime
    beUpdate :: AppRunner -> IO ()
  }

defaultBuildEnv :: Text -> BuildEnv
defaultBuildEnv beInfos = BuildEnv {..}
  where
    beEnsure = const $ pure ()
    beUpdate = const $ pure ()

-- | Create the build env
prepare :: Podenv.Runtime.RuntimeEnv -> Application -> IO BuildEnv
prepare re app = case runtime app of
  Image name -> pure (defaultBuildEnv name)
  Container cb -> pure (prepareContainer cb)
  Rootfs fp -> pure (defaultBuildEnv fp)
  Nix expr -> prepareNix re app expr

-- | Container build env
prepareContainer :: ContainerBuild -> BuildEnv
prepareContainer containerBuild = BuildEnv {..}
  where
    -- buildenv basic info:
    beInfos = "# Containerfile " <> imageName <> "\n" <> fileContent
    fileContent = containerBuild ^. cbContainerfile

    ImageName imageName = mkImageName containerBuild
    fileName = "Containerfile_" <> toString (imageNameToFP imageName)
      where
        imageNameToFP = Text.replace "/" "_" . Text.replace ":" "-"

    beEnsure = const $ do
      imageReady <- checkImageExist imageName
      unless imageReady $
        buildImage imageName fileName fileContent (containerBuild ^. cbImage_volumes)

    beUpdate = const $ case containerBuild ^. cbImage_update of
      Nothing -> error "The container is missing the `image_update` attribute"
      Just cmd -> do
        buildImage
          imageName
          (fileName <> "-update")
          (unlines ["FROM " <> imageName, "RUN " <> cmd])
          (containerBuild ^. cbImage_volumes)

-- | Nix build env
prepareNix :: Podenv.Runtime.RuntimeEnv -> Application -> Flakes -> IO BuildEnv
prepareNix re app flakes = do
  -- TODO: check howto re-use the host /nix
  let certs = undefined
  pure $
    BuildEnv
      { beInfos = "# Nix expr:\n" <> Text.unwords (nixArgs flakes),
        beEnsure = beEnsure certs,
        beUpdate = const $ error "Nix update is not implemented"
      }
  where
    name = app ^. appName
    fileName = toString $ "nix_" <> name

    -- The location where we expect to find the `nix` command
    nixStore = Podenv.Runtime.volumesDir re </> "nix-store"

    beEnsure certs runApp = do
      built <- checkIfBuilt fileName (show $ nixArgs flakes)
      unless built $ do
        ensureNixInstalled
        _ <- runApp (buildApp certs)

        -- save that the build succeeded
        cacheDir <- getCacheDir
        Text.writeFile (cacheDir </> fileName) (show $ nixArgs flakes)

    debug = when (Podenv.Runtime.verbose re) . hPutStrLn stderr . mappend "[+] "

    ensureNixInstalled = do
      debug $ "Checking if " <> nixStore </> nixCommandProfile <> " exists"
      nixInstalled <- doesSymlinkExist $ nixStore </> nixCommandProfile
      unless nixInstalled $ do
        debug $ "Checking if " <> nixStore </> "store" <> " exists"
        storeExist <- doesDirectoryExist $ nixStore </> "store"
        when storeExist $ error $ "existing nix-store is invalid, try removing " <> toText nixStore

        podenv <- getExecutablePath
        debug $ "[+] Installing nix-store with " <> podenv <> " nix.setup"
        P.runProcess_ $ P.setDelegateCtlc True $ P.proc podenv ["nix.setup"]

    -- The Application to build the expression, it is executed in advance to separate build and execution
    buildApp certs =
      Podenv.Config.defaultApp
        { runtime = Rootfs "/",
          name = "build-" <> name,
          volumes = [toText nixStore <> ":/nix", "nix-setup-home:~/", "nix-cache:~/.cache/nix"],
          environ = ["NIX_SSL_CERT_FILE=" <> certs, "LC_ALL=C.UTF-8", "TERM=xterm"],
          command =
            [toText nixCommandPath, "--verbose"]
              <> nixFlags
              <> ["build", "--no-link"]
              <> nixArgs flakes
        }
        & (appCapabilities . capNetwork .~ True)

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

checkIfBuilt :: FilePath -> Text -> IO Bool
checkIfBuilt filename expected = do
  cacheDir <- getCacheDir
  current <- readFileM (cacheDir </> filename)
  pure $ current == expected
