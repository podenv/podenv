(../Podenv.dhall).Application::{
, description = Some "The Programmers Solid 3D CAD Modeller"
, runtime = (../Podenv.dhall).Shell [ ./nixGL.dhall, "nixpkgs#openscad" ]
, command = [ "nixGL", "openscad" ]
, capabilities = (../Podenv.dhall).Capabilities::{ x11 = True, dri = True }
}
