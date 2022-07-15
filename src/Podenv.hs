-- | The podenv library entry point
module Podenv
  ( -- * Config
    Application (..),
    Capabilities (..),

    -- * Import
    loadConfig,
    decodeExpr,
    select,

    -- * Context
    appToContext,

    -- * Runtime
    RuntimeEnv (..),
    defaultRuntimeEnv,
    execute,
    getPodmanPodStatus,
    deletePodmanPod,
  )
where

import Podenv.Application
import Podenv.Config
import Podenv.Context
import Podenv.Dhall
import Podenv.Env
import Podenv.Prelude
import Podenv.Runtime

appToContext :: AppEnv -> Application -> Name -> IO Context
appToContext = preparePure Regular

loadConfig :: Text -> IO Config
loadConfig = Podenv.Config.load Nothing . Just
