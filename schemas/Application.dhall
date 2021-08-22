{ Type =
    { -- | The application name, default to the parent key.
      name : Text
    , description : Optional Text
    , -- | The application runtime.
      runtime : ./Runtime.dhall
    , -- | An optional namespace name.
      namespace : Optional Text
    , -- | Optional runtime mode.
      provide : Optional ./Provider.dhall
    , -- | The application capabilities.
      capabilities : (./Capabilities.dhall).Type
    , -- | List of system capabilities(7).
      syscaps : List Text
    , -- | Process environment.
      environ : List Text
    , -- | Extra volumes.
      volumes : List Text
    , -- | Optional command and its argument.
      command : List Text
    , -- | The home/data directory path in the container, used to resolve `~/`.
      -- This may be overriden by ContainerBuild, and ideally this attribute should be moved to the Image Runtime,
      -- but to keep things simple (e.g. Runtime.Image is just a Text), this is currently an Application attribute.
      home : Optional Text
    }
, default =
  { -- The name defaults to the cli selector
    name = ""
  , description = None Text
  , namespace = None Text
  , provide = None ./Provider.dhall
  , capabilities = (./Capabilities.dhall).default
  , syscaps = [] : List Text
  , environ = [] : List Text
  , volumes = [] : List Text
  , command = [] : List Text
  , home = None Text
  }
}
