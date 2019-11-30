{- A podenv definition -}
{ name : Text
, image : Text
, command : List Text
, capabilities : ./Capabilities.dhall
, environ : Optional ./Environ.dhall
}
