{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Maybe (maybeToList)
import Data.Text (pack)
import qualified Options.Applicative as Opts
import qualified Podenv.Application
import qualified Podenv.Build
import qualified Podenv.Config
import Podenv.Dhall (Application (..))
import qualified Podenv.Dhall
import qualified Podenv.Main
import qualified Podenv.Runtime
import System.Environment (getEnv, setEnv)
import Test.Hspec

main :: IO ()
main = mockEnv >> loadConfig >>= hspec . spec
  where
    loadConfig = Podenv.Config.load Nothing "./test/config.dhall"

    -- Fix env values while keeping the host cache for dhall
    mockEnv = do
      curHome <- getEnv "HOME"
      setEnv "XDG_CACHE_HOME" (curHome <> "/.cache")
      setEnv "HOME" "/home/user"

spec :: Podenv.Config.Config -> Spec
spec config = describe "unit tests" $ do
  describe "config" $ do
    it "load simple" $ loadTest "env" []
    it "load collection" $ loadTest "{ a = env, b = env}" ["a", "b"]
    it "load nested" $ loadTest "{ a = { b = env, c = env}, d = env}" ["a.b", "a.c", "d"]
  describe "builder config" $ do
    it "load firefox" $ do
      let (_, (builderM, baseApp)) = Podenv.Config.select config ["firefox"]
          be = Podenv.Build.initBuildEnv baseApp <$> builderM
      (_, app) <- Podenv.Build.prepare be baseApp
      runtime app `shouldBe` Podenv.Dhall.Image "localhost/firefox"
  -- ctx <- Podenv.Application.prepare app

  describe "cli with single config" $ do
    it "select app" $ cliTest "env" [] "env"
    it "add args" $ cliTest "env" ["ls"] "env // { command = [\"ls\"]}"
    it "add --arg" $ cliTest "env" ["--", "--arg"] "env // { command = [\"--arg\"]}"
    it "set cap" $ cliTest "env" ["--wayland"] (addCap "env" "wayland = True")
    it "unset cap" $ cliTest (addCap "env" "wayland = True") ["--no-wayland"] "env"
    it "set volume" $ cliTest "env" ["--volume", "/tmp/test"] "env // { volumes = [\"/tmp/test\"]}"
    it "set home" $ cliTest "env" ["--home", "/var/app"] "env // { volumes = [\"/var/app:~/\"] }"
  describe "cli default" $ do
    it "image:name" $ cliTest "env" ["image:testy"] "def // { runtime = Podenv.Image \"testy\", name = \"image-b2effd\" }"
    it "nix:expr" $ cliTest "env" ["nix:test"] "def // { runtime = (Nix \"test\"), name = \"nix-1c3a91\", builder = Some \"nix\" }"
  describe "podman ctx" $ do
    it "run simple" $ podmanTest "env" ["run", "--rm", "--hostname", "localhost", "--name", "env", defImg]
    it "run keep" $ podmanTest (addCap "env" "keep = True") ["run", "--hostname", "localhost", "--name", "env", defImg]
    it "run syscaps" $ podmanTest "env // { syscaps = [\"NET_ADMIN\"] }" (defRun ["--hostname", "localhost", "--cap-add", "NET_ADMIN"])
    it "run volumes" $ podmanTest "env // { volumes = [\"/tmp/test\"]}" (defRun ["--security-opt", "label=disable", "--hostname", "localhost", "--volume", "/tmp/test:/tmp/test"])
    it "run home volumes" $
      podmanTest "env // { volumes = [\"~/src:/data\"]}" (defRun ["--security-opt", "label=disable", "--hostname", "localhost", "--volume", "/home/user/src:/data"])
    it "run many volumes" $
      podmanTest
        "env // { volumes = [\"/home/data:/tmp/data\", \"/tmp\", \"/home/old-data:/tmp/data\"]}"
        (defRun ["--security-opt", "label=disable", "--hostname", "localhost", "--volume", "/tmp:/tmp", "--volume", "/home/data:/tmp/data"])
  describe "podman cli" $ do
    it "run minimal" $ podmanCliTest ["image:ubi8"] ["run", "--rm", "--network", "none", "--name", "image-8bfbaa", "ubi8"]
    it "wayland disable selinux" $
      let cmd = ["run", "--rm", "--security-opt", "label=disable", "--network", "none", "--env", "GDK_BACKEND=wayland", "--env", "QT_QPA_PLATFORM=wayland", "--env", "WAYLAND_DISPLAY=wayland-0", "--env", "XDG_RUNTIME_DIR=/run/user", "--mount", "type=tmpfs,destination=/dev/shm", "--volume", "/etc/machine-id:/etc/machine-id", "--mount", "type=tmpfs,destination=/run/user", "--volume", "/run/user/1000/wayland-0:/run/user/wayland-0", "--name", "image-8bfbaa", "ubi8"]
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
        ["run", "-i", "--detach-keys", "", "-t", "--rm", "--hostname", "localhost", "--name", "image-8bfbaa", "ubi8", "/bin/bash"]
    it "run many volumes" $
      podmanCliTest
        ["--volume", "/home/data:/tmp/data", "--volume", "/tmp", "--volume", "/old:/tmp/data", "image:ubi8"]
        ["run", "--rm", "--security-opt", "label=disable", "--network", "none", "--volume", "/tmp:/tmp", "--volume", "/home/data:/tmp/data", "--name", "image-8bfbaa", "ubi8"]
  where
    defRun xs = ["run", "--rm"] <> xs <> ["--name", "env", defImg]
    defImg = ""
    defRe = Podenv.Runtime.defaultRuntimeEnv

    podmanCliTest args expected = do
      (app, _, _) <- Podenv.Main.cliConfigLoad (parseCli args)
      ctx <- Podenv.Application.prepare app
      Podenv.Runtime.podmanRunArgs defRe ctx `shouldBe` expected

    podmanTest code expected = do
      app <- loadOne (addCap code "network = True")
      ctx <- Podenv.Application.prepare app
      Podenv.Runtime.podmanRunArgs defRe ctx `shouldBe` expected

    cliTest code args expectedCode = do
      let cli@Podenv.Main.CLI {..} = parseCli args
      config' <- loadConfig (Podenv.Main.selector cli) code
      let (args', (_, baseApp)) = Podenv.Config.select config' (maybeToList selector <> extraArgs)
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

    parseCli args = case Opts.execParserPure Opts.defaultPrefs Podenv.Main.cliInfo args of
      Opts.Success cli -> cli
      Opts.Failure x -> error $ "Fail to parse cli: " <> show x
      Opts.CompletionInvoked _ -> error "Unexpected completion"

    addCap code capCode =
      "( " <> code <> " // { capabilities = (" <> code <> ").capabilities // {" <> capCode <> "}})"

    loadConfig s code =
      Podenv.Config.load
        s
        ( pack $
            unlines $
              [ "let Podenv = env:PODENV",
                "let Nix = Podenv.Nix",
                "let def = Podenv.Application.default // { runtime = Podenv.Image \"\" }",
                "let env = def // { name = \"env\" }",
                "let env2 = env // { name = \"beta\" } in",
                code
              ]
        )

    loadOne code = do
      config' <- loadConfig Nothing code
      case config' of
        Podenv.Config.ConfigApplication (Podenv.Config.Lit x) -> pure x
        _ -> error "Expected a single app"
