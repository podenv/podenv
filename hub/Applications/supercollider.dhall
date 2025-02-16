let Podenv = ../Podenv.dhall

let default =
      Podenv.Application::{
      , description = Some
          "Programming environment for real-time audio and video processing"
      , runtime =
          (../Podenv.dhall).Nix
            "github:podenv/modularix/c7a19b1a56034440dc787b854daf15a870d09f89#supercollider-nixGL"
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
            "github:podenv/modularix/c7a19b1a56034440dc787b854daf15a870d09f89#sclang-nixGL"
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
