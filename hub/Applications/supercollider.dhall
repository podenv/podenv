let Podenv = ../Podenv.dhall

let default =
      Podenv.Application::{
      , description = Some
          "Programming environment for real-time audio and video processing"
      , runtime =
          (../Podenv.dhall).Nix
            "git+https://codeberg.org/podenv/modularix#supercollider-nixGL"
      , capabilities = Podenv.Capabilities::{
        , dri = True
        , x11 = True
        , pipewire = True
        }
      }

let lang =
      Podenv.Application::{
      , description = Some
          "Programming environment for real-time audio and video processing"
      , runtime =
          (../Podenv.dhall).Nix
            "git+https://codeberg.org/podenv/modularix#sclang-nixGL"
      , volumes = [ "supercollider-home:~" ]
      , capabilities = Podenv.Capabilities::{
        , dri = True
        , x11 = True
        , pipewire = True
        , network = True
        , alsa = True
        }
      }

in  { default, lang }
