(../Podenv.dhall).Application::{
, description = Some "Test graphical setup"
, runtime = (./fedora.dhall).useGraphicRuntime "34" "" [ "xeyes" ] ""
, command = [ "xeyes" ]
, capabilities = (../Podenv.dhall).Capabilities::{ x11 = True }
}
