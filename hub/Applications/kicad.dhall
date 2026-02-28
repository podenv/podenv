(../Podenv.dhall).Application::{
, description = Some "Schematic Capture & PCB Design Software"
, runtime = (../Podenv.dhall).Shell [ ./nixGL.dhall, "nixpkgs#kicad" ]
, command = [ "nixGL", "kicad" ]
, capabilities = (../Podenv.dhall).Capabilities::{ x11 = True, dri = True }
}
