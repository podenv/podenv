let Podenv = ../Podenv.dhall

let browser =
      Podenv.Application::{
      , description = Some "Tor browser"
      , runtime =
          (./fedora.dhall).latest.use
            [ "torbrowser-launcher"
            , "dbus-glib"
            , "qt5-qtwayland"
            , "libXt"
            , "gtk3"
            ]
      , command =
        [ "sh", "-c", "torbrowser-launcher; echo Press enter to exit; read" ]
      , capabilities = Podenv.Capabilities::{
        , network = True
        , x11 = True
        , interactive = True
        }
      }

in  { browser }
