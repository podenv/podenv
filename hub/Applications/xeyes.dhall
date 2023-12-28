(../Podenv.dhall).Application::{
, description = Some "Test graphical setup"
, runtime = (./fedora.dhall).`34`.use [ "xeyes" ]
, command = [ "xeyes" ]
, capabilities = (../Podenv.dhall).Capabilities::{ x11 = True }
}
