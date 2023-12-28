let Podenv = ../Podenv.dhall

in  Podenv.Application::{
    , description = Some "Electrum is an easy to use Bitcoin client"
    , runtime = (./fedora.dhall).latest.useGraphic [ "electrum" ]
    , command = [ "electrum" ]
    , capabilities = Podenv.Capabilities::{ network = True, wayland = True }
    }
