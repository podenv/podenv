{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module defines Haskell data types and lenses for the podenv dhall schemas.
module Podenv.Dhall (
    -- * Application
    Application,
    appDescription,
    appRuntime,
    appCommand,
    appCapabilities,
    appSyscaps,
    appEnviron,
    -- deprecated fields
    appName,
    appNamespace,
    appVolumes,

    -- * ApplicationResource
    ApplicationResource,
    arApplication,
    arMetadata,
    arVolumes,
    arNetwork,
    Network (..),

    -- * Metadata
    Metadata,
    metaName,
    metaNamespace,
    metaLabels,

    -- * Capability Lenses
    Capabilities,
    capTerminal,
    capInteractive,
    capRoot,
    capRw,
    capPrivileged,
    capWayland,
    capPipewire,
    capDbus,
    capPulseaudio,
    capX11,
    capSsh,
    capGpg,
    capVideo,
    capDri,
    capKvm,
    capAlsa,
    capNetwork,
    capTun,
    capCwd,
    capHostfile,

    -- * Runtime
    ContainerBuild,
    Runtime (..),
    cbImage_home,
    cbImage_name,
    cbImage_volumes,
    cbImage_update,
    cbContainerfile,

    -- * Defaults
    defaultApp,
    defaultAppRes,
    appDefault,
    capsDefault,
    containerBuildDefault,

    -- * Embeded
    podenvPackage,
    runtimeType,
    appType,
) where

import Data.Map qualified as Map
import Podenv.Dhall.TH

-- See: https://github.com/adamgundry/ghc-proposals/blob/no-ambiguous-selectors/proposals/0000-no-ambiguous-field-access.rst#migration-strategy
import Podenv.Dhall.TH as Application (Application (..))
import Podenv.Dhall.TH as ApplicationResource (ApplicationResource (..))
import Podenv.Dhall.TH as Capabilities (Capabilities (..))
import Podenv.Dhall.TH as Metadata (Metadata (..))
import Podenv.Prelude hiding (get)
import Relude.Extra.Lens (lens)

defaultApp :: Runtime -> Application
defaultApp runtime =
    Application
        { capabilities = defaultCaps
        , command = []
        , description = Nothing
        , namespace = Nothing
        , name = ""
        , syscaps = []
        , volumes = []
        , environ = []
        , runtime
        }

defaultMetadata :: Metadata
defaultMetadata = Metadata [] Nothing Nothing

-- | Adapt legacy application to the new ApplicationResource definition
defaultAppRes :: Application -> ApplicationResource
defaultAppRes application =
    ApplicationResource
        { metadata =
            defaultMetadata
                & (metaName .~ name)
                . (metaNamespace .~ (application ^. appNamespace))
        , kind = "Application"
        , apiVersion = "podenv/0.2"
        , network = maybe Private netFromNamespace (application ^. appNamespace)
        , volumes = []
        , application = application & (appName .~ "") . (appNamespace .~ Nothing)
        }
  where
    name = case application ^. appName of
        "" -> Nothing
        n -> Just n
    netFromNamespace = \case
        "host" -> Host
        n -> Shared n

defaultCaps :: Capabilities
defaultCaps = extractDhallDefault capsDefault

appName :: Lens' Application Text
appName = lens get set
  where
    get Application{..} = name
    set r v = r{Application.name = v}

appNamespace :: Lens' Application (Maybe Text)
appNamespace = lens get set
  where
    get Application{..} = namespace
    set r v = r{Application.namespace = v}

appVolumes :: Lens' Application [Text]
appVolumes = lens get set
  where
    get Application{..} = volumes
    set r v = r{Application.volumes = v}

arVolumes :: Lens' ApplicationResource [Text]
arVolumes = lens get set
  where
    get ApplicationResource{..} = volumes
    set r v = r{ApplicationResource.volumes = v}

arNetwork :: Lens' ApplicationResource Network
arNetwork = lens get set
  where
    get ApplicationResource{..} = network
    set r v = r{ApplicationResource.network = v}

capNetwork :: Lens' Capabilities Bool
capNetwork = lens get set
  where
    get Capabilities{..} = network
    set caps network = caps{Capabilities.network = network}

metaName :: Lens' Metadata (Maybe Text)
metaName = lens get set
  where
    get Metadata{..} = name
    set r v = r{Metadata.name = v}

metaNamespace :: Lens' Metadata (Maybe Text)
metaNamespace = lens get set
  where
    get Metadata{..} = namespace
    set r v = r{Metadata.namespace = v}

-- This lens takes care of converting the dhall [KVLabel {mapKey, mapValue}] into haskell Map
metaLabels :: Lens' Metadata (Map Text Text)
metaLabels = lens get set
  where
    get Metadata{..} = Map.fromList $ map (\(LabelKV k v) -> (k, v)) labels
    set r m = r{Metadata.labels = map (uncurry LabelKV) $ Map.toList m}
