{ Type =
    { description : Optional Text
    , -- | The application runtime.
      runtime : ./Runtime.dhall
    , -- | The application capabilities.
      capabilities : (./Capabilities.dhall).Type
    , -- | List of system capabilities(7).
      syscaps : List Text
    , -- | Process environment.
      environ : List Text
    , -- | Optional command and its argument.
      command : List Text
    , -- deprecated values
      name : Text
    , namespace : Optional Text
    , volumes : List Text
    }
, default =
  { description = None Text
  , capabilities = (./Capabilities.dhall).default
  , syscaps = [] : List Text
  , environ = [] : List Text
  , command = [] : List Text
  , -- deprecated defaults
    volumes = [] : List Text
  , namespace = None Text
  , name = ""
  }
}
