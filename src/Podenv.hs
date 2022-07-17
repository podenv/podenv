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
    AppEnv (..),
    Context (..),
    Name (..),
    prepare,

    -- * Runtime
    RuntimeEnv (..),
    defaultRuntimeEnv,
    execute,
    getPodmanPodStatus,
    deletePodmanPod,
  )
where

import Podenv.Capability
import Podenv.Config
import Podenv.Context
import Podenv.Dhall
import Podenv.Env
import Podenv.Prelude
import Podenv.Runtime

loadConfig :: Text -> IO Config
loadConfig = Podenv.Config.load Nothing . Just
