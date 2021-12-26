{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Maybe (maybeToList)
import Data.Text (pack)
import qualified Data.Text as Text
import qualified Podenv.Application
import qualified Podenv.Build
import qualified Podenv.Config
import Podenv.Env
import qualified Podenv.Main
import Podenv.Prelude (mayFail)
import qualified Podenv.Runtime
import System.Environment (getEnv, setEnv)
import Test.Hspec

main :: IO ()
main = mockEnv >> loadConfig >>= hspec . spec
  where
    loadConfig = Podenv.Config.load Nothing (Just "./test/config.dhall")

    -- Fix env values while keeping the host cache for dhall
    mockEnv = do
      curHome <- getEnv "HOME"
      setEnv "XDG_CACHE_HOME" (curHome <> "/.cache")
      setEnv "HOME" "/home/user"
      setEnv "WAYLAND_DISPLAY" "wayland-0"

spec :: Podenv.Config.Config -> Spec
spec config = describe "unit tests" $ do
  describe "config" $ do
    it "load simple" $ loadTest "env" []
    it "load collection" $ loadTest "{ a = env, b = env}" ["a", "b"]
    it "load nested" $ loadTest "{ a = { b = env, c = env}, d = env}" ["a.b", "a.c", "d"]
    it "load weak" $ loadTest "{ image = { runtime.image = \"ubi\" }, nix = { runtime.nix = \"n\" } }" ["image", "nix"]
  describe "builder config" $ do
    it "load firefox" $ do
      (_, (builderM, baseApp)) <- mayFail $ Podenv.Config.select config ["firefox"]
      let be = Podenv.Build.initBuildEnv baseApp <$> builderM
      Text.take 33 . Podenv.Build.beInfos <$> be `shouldBe` Just "# Containerfile localhost/firefox"
  describe "cli parser" $ do
    it "pass command args" $ do
      cli <- Podenv.Main.usage ["--name", "test", "image:ubi8", "ls", "-la"]
      Podenv.Main.extraArgs cli `shouldBe` ["ls", "-la"]
    it "handle separator" $ do
      cli <- Podenv.Main.usage ["--name", "test", "image:ubi8", "--", "ls", "-la"]
      Podenv.Main.extraArgs cli `shouldBe` ["ls", "-la"]
  describe "cli with single config" $ do
    it "select app" $ cliTest "env" [] "env"
    it "add args" $ cliTest "env" ["ls"] "env // { command = [\"ls\"]}"
    it "add --arg" $ cliTest "env" ["--", "--arg"] "env // { command = [\"--arg\"]}"
    it "set cap" $ cliTest "env" ["--wayland"] (addCap "env" "wayland = True")
    it "unset cap" $ cliTest (addCap "env" "wayland = True") ["--no-wayland"] "env"
    it "set volume" $ cliTest "env" ["--volume", "/tmp/test"] "env // { volumes = [\"/tmp/test\"]}"
  describe "cli default" $ do
    it "image:name" $ cliTest "env" ["image:testy"] "def // { runtime = Podenv.Image \"testy\", name = \"image-b2effd\" }"
    it "nix:expr" $ cliTest "env" ["nix:test"] "def // { runtime.nix = \"test\", name = \"nix-1c3a91\" }"
  describe "podman ctx" $ do
    it "run simple" $ podmanTest "env" ["run", "--rm", "--hostname", "env", "--name", "env", defImg]
    it "run keep" $ podmanTest (addCap "env" "keep = True") ["run", "--hostname", "env", "--name", "env", defImg]
    it "run syscaps" $ podmanTest "env // { syscaps = [\"NET_ADMIN\"] }" (defRun ["--hostname", "env", "--cap-add", "NET_ADMIN"])
    it "run volumes" $ podmanTest "env // { volumes = [\"/tmp/test\"]}" (defRun ["--security-opt", "label=disable", "--hostname", "env", "--volume", "/tmp/test:/tmp/test"])
    it "run home volumes" $
      podmanTest "env // { volumes = [\"~/src:/data\"]}" (defRun ["--security-opt", "label=disable", "--hostname", "env", "--volume", "/home/user/src:/data"])
    it "run many volumes" $
      podmanTest
        "env // { volumes = [\"/home/data:/tmp/data\", \"/tmp\", \"/home/old-data:/tmp/data\"]}"
        (defRun ["--security-opt", "label=disable", "--hostname", "env", "--volume", "/tmp:/tmp", "--volume", "/home/data:/tmp/data"])
  describe "podman cli" $ do
    it "run minimal" $ podmanCliTest ["image:ubi8"] ["run", "--rm", "--network", "none", "--name", "image-8bfbaa", "ubi8"]
    it "wayland disable selinux" $
      let cmd = ["run", "--rm", "--security-opt", "label=disable", "--network", "none", "--env", "GDK_BACKEND=wayland", "--env", "QT_QPA_PLATFORM=wayland", "--env", "WAYLAND_DISPLAY=wayland-0", "--env", "XDG_RUNTIME_DIR=/run/user", "--env", "XDG_SESSION_TYPE=wayland", "--mount", "type=tmpfs,destination=/dev/shm", "--volume", "/etc/machine-id:/etc/machine-id", "--mount", "type=tmpfs,destination=/run/user", "--volume", "/run/user/1000/wayland-0:/run/user/wayland-0", "--name", "image-8bfbaa", "ubi8"]
       in podmanCliTest ["--wayland", "image:ubi8"] cmd
    it "hostfile are substituted" $
      let cmd = ["run", "--rm", "--network", "none", "--volume", "/proc/cmdline:/data/cmdline", "--volume", "/etc/hosts:/data/hosts", "--name", "image-8bfbaa", "ubi8", "cat", "/data/hosts", "/data/cmdline"]
       in podmanCliTest ["--hostfile", "image:ubi8", "cat", "/etc/hosts", "/proc/cmdline"] cmd
    it "run with name" $
      podmanCliTest
        ["--name", "test", "image:ubi8"]
        ["run", "--rm", "--network", "none", "--name", "test", "ubi8"]
    it "run simple" $
      podmanCliTest
        ["--network", "--shell", "image:ubi8"]
        ["run", "-i", "--detach-keys", "", "-t", "--rm", "--hostname", "image-8bfbaa", "--name", "image-8bfbaa", "ubi8", "/bin/sh"]
    it "shell override hostfile" $ do
      let expected extra = ["run", "-i", "--detach-keys", "", "-t", "--rm", "--network", "none"] <> extra <> ["--name", "image-8bfbaa", "ubi8"]
          cmd = ["--hostfile", "--terminal", "image:ubi8", "vi", "/etc/hosts"]
      podmanCliTest
        (["--shell"] <> cmd)
        (expected [] <> ["/bin/sh"])
      podmanCliTest
        cmd
        (expected ["--volume", "/etc/hosts:/data/hosts"] <> ["vi", "/data/hosts"])
    it "run many volumes" $
      podmanCliTest
        ["--volume", "/home/data:/tmp/data", "--volume", "/tmp", "--volume", "/old:/tmp/data", "image:ubi8"]
        ["run", "--rm", "--security-opt", "label=disable", "--network", "none", "--volume", "/tmp:/tmp", "--volume", "/home/data:/tmp/data", "--name", "image-8bfbaa", "ubi8"]
  where
    defRun xs = ["run", "--rm"] <> xs <> ["--name", "env", defImg]
    defImg = "ubi8"
    defRe = Podenv.Runtime.defaultRuntimeEnv

    testEnv =
      AppEnv
        { _hostXdgRunDir = Just "/run/user/1000",
          _hostHomeDir = Just "/home/user",
          _hostCwd = "/usr/src/podenv",
          _hostUid = 1000,
          _appHomeDir = Just "/home/fedora"
        }

    podmanCliTest args expected = do
      cli <- Podenv.Main.usage args
      (app, mode, _, _) <- Podenv.Main.cliConfigLoad cli
      ctx <- Podenv.Application.preparePure testEnv app mode
      Podenv.Runtime.podmanRunArgs defRe ctx `shouldBe` expected

    podmanTest code expected = do
      app <- loadOne (addCap code "network = True")
      ctx <- Podenv.Application.prepare app Podenv.Application.Regular
      Podenv.Runtime.podmanRunArgs defRe ctx `shouldBe` expected

    cliTest code args expectedCode = do
      cli@Podenv.Main.CLI {..} <- Podenv.Main.usage args
      config' <- loadConfig (Podenv.Main.selector cli) code
      (args', (_, baseApp)) <- mayFail $ Podenv.Config.select config' (maybeToList selector <> extraArgs)
      expected <- loadOne expectedCode

      let got = Podenv.Main.cliPrepare cli args' baseApp
      got `shouldBe` expected

    loadTest code expected = do
      config' <- loadConfig Nothing code
      let got = case config' of
            Podenv.Config.ConfigApplication _ -> []
            Podenv.Config.ConfigApplications xs -> map fst xs
            _ -> error "Bad test config"
      got `shouldBe` expected

    addCap code capCode =
      "( " <> code <> " // { capabilities = (" <> code <> ").capabilities // {" <> capCode <> "}})"

    loadConfig s code =
      Podenv.Config.load
        s
        ( Just
            . pack
            $ unlines
              [ "let Podenv = env:PODENV",
                "let Nix = Podenv.Nix",
                "let def = { capabilities = {=}, runtime = Podenv.Image \"ubi8\" }",
                "let env = def // { name = \"env\" }",
                "let env2 = env // { name = \"beta\" } in",
                code
              ]
        )

    loadOne code = do
      config' <- loadConfig Nothing code
      case config' of
        Podenv.Config.ConfigApplication (Podenv.Config.Lit (Podenv.Config.ApplicationRecord x)) -> pure x
        _ -> error "Expected a single app"
