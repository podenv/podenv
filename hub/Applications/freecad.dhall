(../Podenv.dhall).Application::{
, description = Some
    "General purpose Open Source 3D CAD/MCAD/CAx/CAE/PLM modeler"
, runtime = (../Podenv.dhall).Shell [ ./nixGL.dhall, "nixpkgs#freecad-wayland" ]
, command = [ "nixGL", "freecad" ]
, capabilities = (../Podenv.dhall).Capabilities::{ wayland = True, dri = True }
}
