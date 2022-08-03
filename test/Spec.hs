{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Podenv hiding (loadConfig)
import Podenv.Capability (AppMode (..))
import Podenv.Capability qualified
import Podenv.Config
import Podenv.Context
import Podenv.Dhall
import Podenv.Env
import Podenv.Image
import Podenv.Main hiding (main)
import Podenv.Prelude
import Podenv.Runtime (ExecMode (..))
import Podenv.Runtime qualified
import System.Environment (setEnv)
import Test.Hspec

main :: IO ()
main = mockEnv >> doLoadConfig >>= hspec . spec
  where
    doLoadConfig = do
      testConfig <- Podenv.Config.loadConfig (Just "./test/config.dhall")
      goldenConfig <- Podenv.Config.loadConfig (Just "./test/golden.dhall")
      pure (testConfig, goldenConfig)

    -- Fix env values while keeping the host cache for dhall
    mockEnv = do
      curHome <- getEnv "HOME"
      setEnv "XDG_CACHE_HOME" (curHome <> "/.cache")
      setEnv "NIX_SSL_CERT_FILE" "/etc/hosts"

spec :: (Config, Config) -> Spec
spec (config, goldenConfig) = describe "unit tests" $ do
  describe "config" $ do
    let loadTest code expected = do
          config' <- Podenv.Config.unConfig <$> loadConfig' code
          map fst config' `shouldBe` expected

    it "load simple" $ loadTest "env" [""]
    it "load collection" $ loadTest "{ a = env, b = env}" ["a", "b"]
    it "load nested" $ loadTest "{ a = { b = env, c = env}, d = env}" ["a.b", "a.c", "d"]
    it "load weak" $ loadTest "{ image = { runtime.image = \"ubi\" }, nix = { runtime.nix = \"n\" } }" ["image", "nix"]

  describe "golden" $ do
    let mkGoldenConfig :: Maybe Config -> [String] -> IO Text
        mkGoldenConfig configM args = do
          cli <- usage args
          cfg <- maybe (loadConfig (configExpr cli)) pure configM
          (ar, mode, gl, run) <- cliConfigLoad "/volumes" testEnv cfg cli
          ctx <- Podenv.Runtime.appToContext run mode ar
          mappend ("==== " <> show args <> "\n") . Text.replace " --" "\n  --"
            <$> runReaderT (Podenv.Runtime.showCmd run Foreground ctx) gl
        mkGolden = mkGoldenConfig (Just goldenConfig)
        writeGolden :: [[String]] -> [[String]] -> IO ()
        writeGolden xs ys = do
          content <- Text.unlines <$> traverse mkGolden xs
          content2 <- Text.unlines <$> traverse (mkGoldenConfig Nothing) ys
          current <- Text.readFile "test/golden.txt"
          let new =
                -- work around typed-process display bug
                Text.replace "--detach-keys  -t" "--detach-keys \"\" -t"
                  . Text.unlines
                  $ [ show $ map fst (unConfig goldenConfig),
                      content,
                      content2
                    ]
          when (current /= new) $ do
            Text.writeFile "test/golden.txt" new
            putTextLn "Checkout `git diff`"
            exitFailure

    it "update golden.txt" $ do
      writeGolden
        [ ["legacy.vpn"],
          ["legacy.web"],
          ["corp.vpn"],
          ["corp.bridge"],
          ["--root", "--name", "ubi", "ubi"],
          ["--root", "--name", "ubi", "--namespace", "testns", "ubi"],
          ["--headless", "./test/headless.dhall", "firefox"],
          ["--network", "container:sway.vnc", "vnc-viewer", "localhost"],
          ["podenv"]
        ]
        [ -- selector is removed from arg because it match the single app selector
          ["--config", "{env = {runtime.image = \"ubi8\" }}", "env", "id"],
          -- the single app is automatically selected
          ["--config", "{env = {runtime.image = \"ubi8\" }}", "id"],
          -- image selector
          ["image:ubi8"],
          -- nix selector
          ["nix:test"],
          -- run with name
          ["--name", "test", "image:ubi8"],
          -- run net default
          ["--config", "{ runtime.image = \"ubi8\", capabilities.network = True }"],
          -- run no-network
          ["--no-network", "--config", "{ runtime.image = \"ubi8\", capabilities.network = True }"],
          -- run net private
          ["--network", "private", "image:ubi8"],
          -- run net host
          ["--network", "host", "image:ubi8"],
          -- run shared net
          ["--network", "vpn", "image:ubi8"],
          -- wayland disable selinux
          ["--wayland", "image:ubi8"],
          -- hostfile are substituted
          ["--hostfile", "image:ubi8", "cat", "/etc/hosts", "/proc/cmdline"],
          -- shell override hostfile
          ["--shell", "--hostfile", "--terminal", "image:ubi8", "vi", "/etc/hosts"],
          ["--hostfile", "--terminal", "image:ubi8", "vi", "/etc/hosts"],
          -- many volumes
          ["--volume", "/home/data:/tmp/data", "--volume", "/tmp", "--volume", "/old:/tmp/data", "image:ubi8"],
          -- name override keep image
          ["--name", "tmp", "--config", "{ name = \"firefox\", runtime.image = \"localhost/firefox\" }"],
          -- bwrap test
          ["--shell", "rootfs:/srv"]
        ]

  describe "builder config" $ do
    it "load firefox" $ do
      (_, baseApp) <- mayFail $ Podenv.Config.select config ["firefox"]
      let be = Podenv.Runtime.createLocalhostRunEnv testEnv
      Text.take 34 (Podenv.Runtime.showBuildInfo be (baseApp ^. arApplication . appRuntime)) `shouldBe` "# Containerfile localhost/3c922bca"
    it "load nixify" $ do
      (_, baseApp) <- mayFail $ Podenv.Config.select config ["nixify", "firefox", "about:blank"]
      let be = Podenv.Runtime.createLocalhostRunEnv testEnv
      Text.take 34 (Podenv.Runtime.showBuildInfo be (baseApp ^. arApplication . appRuntime)) `shouldBe` "# Containerfile localhost/3c922bca"
    it "override nixpkgs when necessary" $ do
      let mkApp installables' pin =
            Podenv.Config.defaultApp (Podenv.Dhall.Nix (Podenv.Dhall.Flakes Nothing installables' pin))
              & (appName .~ "test")

          checkCommand test app expected = do
            ctx <- runPrepare (Regular []) testEnv (defaultAppRes app)
            (ctx ^. ctxCommand) `test` expected
          commandShouldContain = checkCommand shouldContain
          commandShouldNotContain = checkCommand shouldNotContain

      mkApp ["nixpkgs#hello"] (Just "nixpkgs/42") `commandShouldContain` ["--override-input", "nixpkgs"]

      mkApp ["nixpkgs/42#hello"] (Just "nixpkgs/42") `commandShouldNotContain` ["--override-input", "nixpkgs"]

      mkApp ["nixpkgs/42#hello", "nixGL"] (Just "nixpkgs/42") `commandShouldContain` ["--override-input", "nixpkgs"]

  describe "cli parser" $ do
    it "pass command args" $ do
      cli <- Podenv.Main.usage ["--name", "test", "image:ubi8", "ls", "-la"]
      Podenv.Main.cliExtraArgs cli `shouldBe` ["ls", "-la"]
    it "handle separator" $ do
      cli <- Podenv.Main.usage ["--name", "test", "image:ubi8", "--", "ls", "-la"]
      Podenv.Main.cliExtraArgs cli `shouldBe` ["ls", "-la"]

  describe "cli with single config" $ do
    it "select app" $ cliTest "env" [] "env"
    it "add args" $ cliTest "env" ["ls"] "env // { command = [\"ls\"]}"
    it "add --arg" $ cliTest "env" ["--", "--arg"] "env // { command = [\"--arg\"]}"
    it "set cap" $ cliTest "env" ["--wayland"] (addCap "env" "wayland = True")
    it "unset cap" $ cliTest (addCap "env" "wayland = True") ["--no-wayland"] "env"
    it "set volume" $ cliTest "env" ["--volume", "/tmp/test"] "env // { volumes = [\"/tmp/test\"]}"
    it "one args" $ cliTest "\\(a : Text) -> env // { description = Some a }" ["a"] "env // { description = Some \"a\"}"
    it "two args" $
      cliTest
        "\\(a : Text) -> \\(b : Text) -> env // { description = Some (a ++ b) }"
        ["a", "b"]
        "env // { description = Some \"ab\"}"

  describe "nix test" $ do
    it "nix run without args" $ nixTest "{ runtime.nix = \"test\"}" [] ["run", "test"]
    it "nix run with args" $
      nixTest
        "{env, test = { runtime.nix = \"test\"}}"
        ["test", "--help"]
        ["run", "test", "--", "--help"]
    it "nix run with shell" $
      nixTest
        "{env, test = { runtime.nix = \"test\", command = [\"cmd\"]}}"
        ["test", "--help"]
        ["shell", "test", "--command", "cmd", "--help"]

  describe "podman ctx" $ do
    let defRun xs = ["run", "--rm"] <> xs <> ["--label", "podenv.selector=unknown", defImg]
    let podmanTest code expected = do
          ar <- loadOne (addCap code "network = True, rw = True")
          ctx <- runPrepare (Regular []) testEnv ar
          Podenv.Runtime.podmanRunArgs defRe fg ctx (getImg ar) `shouldBe` expected
    it "run simple" $ podmanTest "env" (defRun [])
    it "run simple root" $
      podmanTest
        "env // { capabilities.root = True }"
        (defRun ["--user", "0", "--workdir", "/root", "--env", "HOME=/root", "--volume", "/data/podenv-home:/root"])
    it "run syscaps" $
      podmanTest
        "env // { syscaps = [\"NET_ADMIN\"] }"
        (defRun ["--cap-add", "CAP_NET_ADMIN"])
    it "run hostdir" $
      podmanTest
        "env // { volumes = [\"/tmp/test\"]}"
        (defRun ["--security-opt", "label=disable", "--volume", "/tmp/test:/tmp/test"])
    it "run volumes" $
      podmanTest
        "env // { volumes = [\"nix-store:/nix\"]}"
        (defRun ["--volume", "/data/nix-store:/nix"])
    it "run home volumes" $
      podmanTest
        "env // { volumes = [\"~/src:/data\"]}"
        (defRun ["--security-opt", "label=disable", "--volume", "/home/user/src:/data"])
    it "run many volumes" $
      podmanTest
        "env // { volumes = [\"/home/data:/tmp/data\", \"/tmp\", \"/home/old-data:/tmp/data\"]}"
        (defRun ["--security-opt", "label=disable", "--volume", "/tmp:/tmp", "--volume", "/home/data:/tmp/data"])
  where
    defImg = "ubi8"
    defRe = Podenv.Runtime.defaultGlobalEnv "/data"

    runPrepare mode env app = runAppEnv env app $ Podenv.Capability.prepare mode

    testEnv =
      AppEnv
        { _envHostXdgRunDir = Just "/run/user/1000",
          _envHostWaylandSocket = Just (SocketName "wayland-0"),
          _envHostHomeDir = Just "/home/user",
          _envHostCwd = "/usr/src/podenv",
          _envHostUid = 1000,
          _envAppHomeDir = Nothing,
          _envHostDisplay = ":0",
          _envHostSSHAgent = Nothing,
          _envIsNVIDIAEnabled = pure False,
          _envGetAppHomeDir = \app -> pure $ case app ^. Podenv.Dhall.appRuntime of
            Podenv.Dhall.Nix _ -> Just "/home/user"
            Podenv.Dhall.Container cb -> toString <$> cb ^. cbImage_home
            _ -> Nothing,
          _envGetVideoDevices = pure [],
          _envGetCertLocation = pure $ Just "/etc/ca"
        }

    getImg app = case app ^. Podenv.Dhall.arApplication . Podenv.Dhall.appRuntime of
      Podenv.Dhall.Image image -> ImageName image
      _ -> error "Not podman"

    fg = Foreground

    getApp code args = do
      cli <- Podenv.Main.usage args
      cfg <- loadConfig' code
      (app, mode, _, _) <- Podenv.Main.cliConfigLoad "/volumes" testEnv cfg cli
      runPrepare mode (testEnv & envAppHomeDir ?~ "/home") app

    cliTest :: Text -> [String] -> Text -> IO ()
    cliTest gotCode args expectedCode = do
      got <- getApp gotCode args
      expected <- getApp expectedCode []
      let removeSelector = ctxLabels .~ mempty
      (got & removeSelector) `shouldBe` (expected & removeSelector)

    nixTest code args expectedCommand = do
      ctx <- getApp code args
      drop 3 (ctx ^. ctxCommand) `shouldBe` expectedCommand

    addCap code capCode =
      "( " <> code <> " // { capabilities = (" <> code <> ").capabilities // {" <> capCode <> "}})"

    mkConfig code =
      unlines
        [ "let Podenv = env:PODENV",
          "let Nix = Podenv.Nix",
          "let def = { capabilities = {=}, runtime = Podenv.Image \"ubi8\" }",
          "let env = def in ",
          code
        ]
    loadConfig' code = Podenv.Config.loadConfig (Just . mkConfig $ code)

    loadOne code = do
      config' <- loadConfig' code
      pure $ case Podenv.Config.unConfig config' of
        [(_, Podenv.Config.LitApp x)] -> defaultAppRes (Podenv.Config.unRecord x)
        _ -> error "Expected a single app"
