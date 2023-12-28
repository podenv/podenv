let Podenv = ../Podenv.dhall

in  Podenv.Application::{
    , description = Some "Qt based JACK control application"
    , runtime =
        (./nix.dhall).useInstallables [ "github:podenv/modularix#qjackctl" ]
    , capabilities = Podenv.Capabilities::{
      , pipewire = True
      , x11 = True
      , alsa = True
      }
    }
