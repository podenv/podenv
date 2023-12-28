let Podenv = ../Podenv.dhall

let packages =
      [ "https://dl.google.com/linux/direct/google-chrome-stable_current_x86_64.rpm"
      ]

in  Podenv.Application::{
    , description = Some "Cross-platform web browser developed by Google"
    , runtime = (./fedora.dhall).latest.useGraphicCodec packages
    , command = [ "google-chrome" ]
    , capabilities = Podenv.Capabilities::{
      , network = True
      , wayland = True
      , -- | chrome somehow still needs x11
        x11 = True
      }
    }
