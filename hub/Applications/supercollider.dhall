let Podenv = ../Podenv.dhall

let default =
      Podenv.Application::{
      , description = Some
          "Programming environment for real-time audio and video processing"
      , runtime =
          (./nix.dhall).useInstallables
            [ ./nixGL.dhall
            , "github:podenv/modularix/e3d85b9fc5f9a66ae4abce8817f9df51b1304b81#supercollider"
            ]
      , command = [ "nixGL", "scide" ]
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
          (./nix.dhall).useInstallables
            [ ./nixGL.dhall
            , "github:podenv/modularix/aeeca74b03d36a8a866e1f4f76aa3f925d24cf98#sclang"
            ]
      , command = [ "nixGL", "sclang" ]
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
