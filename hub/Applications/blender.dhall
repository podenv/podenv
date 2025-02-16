let default =
    -- This is using nix because the fedora package doesn't come with ffmpeg
      (../Podenv.dhall).Application::{
      , description = Some "3D creation suite"
      , runtime =
          (../Podenv.dhall).Nix
            "github:podenv/modularix/895e38a077190ae94c65aa2fa5b60733fa5c79f7#blender"
      , capabilities = (../Podenv.dhall).Capabilities::{
        , dri = True
        , wayland = True
        , pulseaudio = True
        }
      }

let fedora =
          default
      //  { runtime = (./fedora.dhall).latest.useGraphic [ "blender" ]
          , command = [ "blender" ]
          }

in  { default, fedora }
