let Podenv = ../Podenv.dhall

in  Podenv.Application::{
    , description = Some
        "Programming environment for real-time audio and video processing"
    , runtime =
        (./fedora.dhall).latest.useGraphicCodec
          [ "supercollider", "pipewire-jack-audio-connection-kit" ]
    , command = [ "scide" ]
    , capabilities = Podenv.Capabilities::{
      , dri = True
      , x11 = True
      , pipewire = True
      }
    }
