{- A podenv definition -}
{ name : Text
, description : Optional Text
, url : Optional Text
, image : Text
, packages : Optional (List Text)
, command : List Text
, capabilities : ./Capabilities.dhall
, environ : Optional ./Environ.dhall
, ports : Optional (List Text)
}
