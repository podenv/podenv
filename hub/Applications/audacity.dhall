let Podenv = ../Podenv.dhall

in  Podenv.Application::{
    , description = Some "Sound editor with graphical UI"
    , runtime =
        (./fedora.dhall).latest.useGraphic
          [ "audacity", "alsa-plugins-pulseaudio" ]
    , command = [ "audacity" ]
    , capabilities = Podenv.Capabilities::{
      , pipewire = True
      , pulseaudio = True
      , wayland = True
      }
    }
