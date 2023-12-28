{ Type =
    { metadata : (./Metadata.dhall).Type
    , kind : Text
    , apiVersion : Text
    , network : ./Network.dhall
    , volumes : List Text
    , application : (./Application.dhall).Type
    }
, default =
  { metadata = (./Metadata.dhall).default
  , kind = "Application"
  , apiVersion = "podenv/0.2"
  , network = (./Network.dhall).Private
  , volumes = [] : List Text
  , application = (./Application.dhall).default
  }
}
