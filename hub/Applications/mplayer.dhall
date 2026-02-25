let Podenv = ../Podenv.dhall

in  Podenv.Application::{
    , description = Some "Media player software"
    , runtime = (./fedora.dhall).latest.useGraphic [ "mplayer" ]
    , command = [ "mplayer" ]
    , capabilities = Podenv.Capabilities::{
      , dri = True
      , pulseaudio = True
      , wayland = True
      , hostfile = True
      }
    }
