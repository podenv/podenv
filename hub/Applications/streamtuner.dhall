let Podenv = ../Podenv.dhall

in  Podenv.Application::{
    , description = Some "An internet radio browser"
    , runtime = (./fedora.dhall).latest.useGraphic [ "streamtuner" ]
    , command = [ "streamtuner2" ]
    , capabilities = Podenv.Capabilities::{
      , x11 = True
      , network = True
      , pulseaudio = True
      }
    }
