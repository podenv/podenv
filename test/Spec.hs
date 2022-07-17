{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Text (pack)
import Data.Text qualified as Text
import Podenv hiding (command, loadConfig)
import Podenv.Capability qualified
import Podenv.Config qualified
import Podenv.Context (command)
import Podenv.Context qualified
import Podenv.Dhall qualified
import Podenv.Env
import Podenv.Main qualified
import Podenv.Prelude (mayFail, (^.))
import Podenv.Runtime (RunMode (..))
import Podenv.Runtime qualified
import System.Environment (getEnv, setEnv)
import Test.Hspec

main :: IO ()
main = mockEnv >> doLoadConfig >>= hspec . spec
  where
    doLoadConfig = Podenv.Config.load Nothing (Just "./test/config.dhall")

    -- Fix env values while keeping the host cache for dhall
    mockEnv = do
      curHome <- getEnv "HOME"
      setEnv "XDG_CACHE_HOME" (curHome <> "/.cache")
      setEnv "NIX_SSL_CERT_FILE" "/etc/hosts"

spec :: Podenv.Config.Config -> Spec
spec config = describe "unit tests" $ do
  describe "config" $ do
    it "load simple" $ loadTest "env" []
    it "load collection" $ loadTest "{ a = env, b = env}" ["a", "b"]
    it "load nested" $ loadTest "{ a = { b = env, c = env}, d = env}" ["a.b", "a.c", "d"]
    it "load weak" $ loadTest "{ image = { runtime.image = \"ubi\" }, nix = { runtime.nix = \"n\" } }" ["image", "nix"]
  describe "builder config" $ do
    it "load firefox" $ do
      (_, baseApp) <- mayFail $ Podenv.Config.select config ["firefox"]
      let be = Podenv.Runtime.createLocalhostRunEnv testEnv baseApp (Name "firefox")
      Text.take 34 (Podenv.Runtime.buildInfo be) `shouldBe` "# Containerfile localhost/3c922bca"
    it "load nixify" $ do
      (_, baseApp) <- mayFail $ Podenv.Config.select config ["nixify", "firefox", "about:blank"]
      let be = Podenv.Runtime.createLocalhostRunEnv testEnv baseApp (Name "firefox")
      Text.take 34 (Podenv.Runtime.buildInfo be) `shouldBe` "# Containerfile localhost/3c922bca"
    it "override nixpkgs when necessary" $ do
      let mkApp installables pin =
            Podenv.Config.defaultApp
              { Podenv.Dhall.runtime = Podenv.Dhall.Nix (Podenv.Dhall.Flakes installables pin)
              }
          checkCommand test app expected = do
            ctx <- runPrepare (Regular []) (testEnv {_appHomeDir = Just "/tmp"}) app (Name "test")
            (ctx ^. command) `test` expected
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
    it "one args" $ cliTest "\\(name : Text) -> env // { name }" ["a"] "env // { name = \"a\"}"
    it "two args" $ cliTest "\\(name : Text) -> \\(desc : Text) -> env // { name, description = Some desc }" ["a", "b"] "env // { name = \"a\", description = Some \"b\"}"
  describe "cli default" $ do
    it "image:name" $ cliTest "env" ["image:testy"] "def // { runtime = Podenv.Image \"testy\", name = \"image-b2effd\" }"
    it "nix:expr" $ cliTest "env" ["nix:test"] "def // { runtime.nix = \"test\", name = \"nix-a94a8f\" }"
  describe "bwrap ctx" $ do
    it "run simple" $ bwrapTest ["--shell"] (defBwrap <> ["/bin/sh"])
  describe "nix test" $ do
    it "nix run without args" $ nixTest "{ runtime.nix = \"test\"}" [] ["run", "test"]
    it "nix run with args" $ nixTest "{env, test = { runtime.nix = \"test\"}}" ["test", "--help"] ["run", "test", "--", "--help"]
    it "nix run with shell" $ nixTest "{env, test = { runtime.nix = \"test\", command = [\"test\"]}}" ["test", "--help"] ["shell", "test", "--command", "test", "--help"]
  describe "podman ctx" $ do
    it "run simple" $ podmanTest "env" ["run", "--rm", "--hostname", "env", "--name", "env", defImg]
    it "run syscaps" $ podmanTest "env // { syscaps = [\"NET_ADMIN\"] }" (defRun ["--hostname", "env", "--cap-add", "CAP_NET_ADMIN"])
    it "run hostdir" $ podmanTest "env // { volumes = [\"/tmp/test\"]}" (defRun ["--security-opt", "label=disable", "--hostname", "env", "--volume", "/tmp/test:/tmp/test"])
    it "run volumes" $ podmanTest "env // { volumes = [\"nix-store:/nix\"]}" (defRun ["--hostname", "env", "--volume", "/data/nix-store:/nix"])
    it "run home volumes" $
      podmanTest "env // { volumes = [\"~/src:/data\"]}" (defRun ["--security-opt", "label=disable", "--hostname", "env", "--volume", "/home/user/src:/data"])
    it "run many volumes" $
      podmanTest
        "env // { volumes = [\"/home/data:/tmp/data\", \"/tmp\", \"/home/old-data:/tmp/data\"]}"
        (defRun ["--security-opt", "label=disable", "--hostname", "env", "--volume", "/tmp:/tmp", "--volume", "/home/data:/tmp/data"])
  describe "podman cli" $ do
    it "run minimal" $ podmanCliTest ["image:ubi8"] ["run", "--rm", "--network", "none", "--name", "image-8bfbaa", "ubi8"]
    it "wayland disable selinux" $
      let cmd = ["run", "--rm", "--security-opt", "label=disable", "--network", "none", "--env", "GDK_BACKEND=wayland", "--env", "QT_QPA_PLATFORM=wayland", "--env", "WAYLAND_DISPLAY=wayland-0", "--env", "XDG_RUNTIME_DIR=/run/user/1000", "--env", "XDG_SESSION_TYPE=wayland", "--mount", "type=tmpfs,destination=/dev/shm", "--volume", "/etc/machine-id:/etc/machine-id", "--mount", "type=tmpfs,destination=/run/user", "--volume", "/run/user/1000/wayland-0:/run/user/1000/wayland-0", "--name", "image-8bfbaa", "ubi8"]
       in podmanCliTest ["--wayland", "image:ubi8"] cmd
    it "hostfile are substituted" $
      let cmd = ["run", "--rm", "--security-opt", "label=disable", "--network", "none", "--volume", "/proc/cmdline:/data/cmdline", "--volume", "/etc/hosts:/data/hosts", "--name", "image-8bfbaa", "ubi8", "cat", "/data/hosts", "/data/cmdline"]
       in podmanCliTest ["--hostfile", "image:ubi8", "cat", "/etc/hosts", "/proc/cmdline"] cmd
    it "run with name" $
      podmanCliTest
        ["--name", "test", "image:ubi8"]
        ["run", "--rm", "--network", "none", "--name", "test", "ubi8"]
    it "run simple" $
      podmanCliTest
        ["--network", "--shell", "image:ubi8"]
        ["run", "-i", "--detach-keys", "", "-t", "--rm", "--hostname", "image-8bfbaa", "--env", "TERM=xterm-256color", "--name", "image-8bfbaa", "ubi8", "/bin/sh"]
    it "shell override hostfile" $ do
      let expected seArgs extra = ["run", "-i", "--detach-keys", "", "-t", "--rm"] <> seArgs <> ["--network", "none", "--env", "TERM=xterm-256color"] <> extra <> ["--name", "image-8bfbaa", "ubi8"]
          cmd = ["--hostfile", "--terminal", "image:ubi8", "vi", "/etc/hosts"]
      podmanCliTest
        (["--shell"] <> cmd)
        (expected [] [] <> ["/bin/sh"])
      podmanCliTest
        cmd
        (expected ["--security-opt", "label=disable"] ["--volume", "/etc/hosts:/data/hosts"] <> ["vi", "/data/hosts"])
    it "run many volumes" $
      podmanCliTest
        ["--volume", "/home/data:/tmp/data", "--volume", "/tmp", "--volume", "/old:/tmp/data", "image:ubi8"]
        ["run", "--rm", "--security-opt", "label=disable", "--network", "none", "--volume", "/tmp:/tmp", "--volume", "/home/data:/tmp/data", "--name", "image-8bfbaa", "ubi8"]
    it "name override keep image" $ do
      cli <- Podenv.Main.usage ["--name", "tmp", "--config", "{ name = \"firefox\", runtime.image = \"localhost/firefox\" }"]
      (app, mode, ctxName, gl) <- Podenv.Main.cliConfigLoad cli
      ctx <- runPrepare mode testEnv app ctxName
      let re = Podenv.Runtime.createLocalhostRunEnv testEnv app (Name "test")
      Podenv.Runtime.podmanRunArgs gl fg ctx (getImg re) `shouldBe` ["run", "--rm", "--read-only=true", "--network", "none", "--name", "tmp", "localhost/firefox"]
  where
    defRun xs = ["run", "--rm"] <> xs <> ["--name", "env", defImg]
    defImg = "ubi8"
    defRe = Podenv.Runtime.defaultGlobalEnv "/data"

    runPrepare mode env app ctxName = runAppEnv env $ Podenv.Capability.prepare mode app ctxName

    testEnv =
      AppEnv
        { _hostXdgRunDir = Just "/run/user/1000",
          _hostWaylandSocket = Just (SocketName "wayland-0"),
          _hostHomeDir = Just "/home/user",
          _hostCwd = "/usr/src/podenv",
          _hostUid = 1000,
          _appHomeDir = Nothing,
          _hostDisplay = ":0",
          _hostSSHAgent = Nothing,
          _isNVIDIAEnabled = pure False,
          _getVideoDevices = pure [],
          _getCertLocation = pure $ Just "/etc/ca"
        }

    getFP re = case Podenv.Runtime.runtimeBackend re of
      Podenv.Runtime.Bubblewrap fp -> fp
      _ -> error "Not bwrap"

    defBwrap =
      ["--die-with-parent", "--unshare-pid", "--unshare-ipc", "--unshare-uts", "--unshare-net"]
        <> ["--ro-bind", "/srv", "/", "--proc", "/proc", "--dev", "/dev", "--perms", "01777", "--tmpfs", "/tmp"]
        <> ["--clearenv", "--setenv", "TERM", "xterm-256color"]

    bwrapTest args expected = do
      cli <- Podenv.Main.usage (args <> ["rootfs:/srv"])
      (app, mode, ctxName, gl') <- Podenv.Main.cliConfigLoad cli
      let gl = gl' {Podenv.Runtime.system = Podenv.Config.defaultSystemConfig}
      ctx <- runPrepare mode testEnv app ctxName
      let re = Podenv.Runtime.createLocalhostRunEnv testEnv app (Name "test")
      Podenv.Runtime.bwrapRunArgs gl ctx (getFP re) `shouldBe` expected

    getImg re = case Podenv.Runtime.runtimeBackend re of
      Podenv.Runtime.Podman image -> image
      _ -> error "Not podman"

    fg = Foreground

    podmanCliTest args expected = do
      cli <- Podenv.Main.usage (["--rw"] <> args)
      (app, mode, ctxName, gl') <- Podenv.Main.cliConfigLoad cli
      ctx <- runPrepare mode testEnv app ctxName
      let gl = gl' {Podenv.Runtime.system = Podenv.Config.defaultSystemConfig}
      let re = Podenv.Runtime.createLocalhostRunEnv testEnv app (Name "test")
      Podenv.Runtime.podmanRunArgs gl fg ctx (getImg re) `shouldBe` expected

    podmanTest code expected = do
      app <- loadOne (addCap code "network = True, rw = True")
      ctx <- runPrepare (Regular []) testEnv app (Podenv.Context.Name "env")
      let re = Podenv.Runtime.createLocalhostRunEnv testEnv app (Name "test")
      Podenv.Runtime.podmanRunArgs defRe fg ctx (getImg re) `shouldBe` expected

    getApp code args = do
      cli <- Podenv.Main.usage args
      (app, mode, name, _) <- Podenv.Main.cliConfigLoad (cli {Podenv.Main.configExpr = Just . mkConfig $ code})
      ctx <- runPrepare mode (testEnv {_appHomeDir = Just "/home"}) app name
      pure (app, ctx)

    cliTest gotCode args expectedCode = do
      (_, got) <- getApp gotCode args
      (_, expected) <- getApp expectedCode []
      got `shouldBe` expected

    nixTest code args expectedCommand = do
      (_, ctx) <- getApp code args
      drop 3 (ctx ^. command) `shouldBe` expectedCommand

    loadTest code expected = do
      config' <- loadConfig Nothing code
      let got = case config' of
            Podenv.Config.ConfigApplication _ -> []
            Podenv.Config.ConfigApplications xs -> map fst xs
            _ -> error "Bad test config"
      got `shouldBe` expected

    addCap code capCode =
      "( " <> code <> " // { capabilities = (" <> code <> ").capabilities // {" <> capCode <> "}})"

    mkConfig code =
      pack $
        unlines
          [ "let Podenv = env:PODENV",
            "let Nix = Podenv.Nix",
            "let def = { capabilities = {=}, runtime = Podenv.Image \"ubi8\" }",
            "let env = def // { name = \"env\" }",
            "let env2 = env // { name = \"beta\" } in",
            code
          ]
    loadConfig s code =
      Podenv.Config.load s (Just . mkConfig $ code)

    loadOne code = do
      config' <- loadConfig Nothing code
      case config' of
        Podenv.Config.ConfigApplication (Podenv.Config.Lit (Podenv.Config.ApplicationRecord x)) -> pure x
        _ -> error "Expected a single app"
