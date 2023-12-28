let Podenv = ../Podenv.dhall

in  Podenv.Application::{
    , description = Some "Media player software"
    , runtime = (./fedora.dhall).latest.useGraphicCodec [ "mplayer" ]
    , command = [ "mplayer" ]
    , capabilities = Podenv.Capabilities::{
      , dri = True
      , pulseaudio = True
      , wayland = True
      }
    }
