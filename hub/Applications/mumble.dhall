let Podenv = ../Podenv.dhall

in  Podenv.Application::{
    , description = Some "VoIP solution"
    , runtime = (./fedora.dhall).latest.useGraphic [ "mumble", "opus" ]
    , command = [ "mumble" ]
    , capabilities = Podenv.Capabilities::{
      , network = True
      , pulseaudio = True
      , wayland = True
      }
    }
