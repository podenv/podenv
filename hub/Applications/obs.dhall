let Podenv = ../Podenv.dhall

in  Podenv.Application::{
    , description = Some "Open Broadcaster Software"
    , runtime = (./fedora.dhall).latest.useGraphicCodec [ "obs-studio" ]
    , command = [ "obs" ]
    , capabilities = Podenv.Capabilities::{
      , wayland = True
      , dri = True
      , dbus = True
      , video = True
      , pipewire = True
      }
    }
