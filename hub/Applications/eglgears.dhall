(../Podenv.dhall).Application::{
, description = Some "Test wayland graphical setup"
, runtime = (./fedora.dhall).latest.useGraphic [ "mesa-demos" ]
, command = [ "/usr/lib64/mesa/eglgears_wayland" ]
, capabilities = (../Podenv.dhall).Capabilities::{ wayland = True, dri = True }
}
