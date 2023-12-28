let Podenv = ../Podenv.dhall

in  Podenv.Application::{
    , description = Some "Go Board game"
    , runtime = (./fedora.dhall).latest.useGraphic [ "kigo", "dbus-x11" ]
    , command = [ "kigo" ]
    , capabilities = Podenv.Capabilities::{ pulseaudio = True, x11 = True }
    }
