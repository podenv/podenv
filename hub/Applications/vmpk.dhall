let Podenv = ../Podenv.dhall

in  Podenv.Application::{
    , description = Some "Virtual MIDI Piano Keyboard"
    , runtime = (./fedora.dhall).latest.useGraphic [ "vmpk" ]
    , command = [ "vmpk" ]
    , capabilities = Podenv.Capabilities::{ pipewire = True, wayland = True }
    }
