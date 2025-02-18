(../Podenv.dhall).Application::{
, description = Some "Client for the KGS Go Server"
, runtime = (./nix.dhall).use "cgoban"
, capabilities = (../Podenv.dhall).Capabilities::{ x11 = True, network = True }
}
