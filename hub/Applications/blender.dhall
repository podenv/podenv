let default =
    -- This is using nix because the fedora package doesn't come with ffmpeg
      (../Podenv.dhall).Application::{
      , description = Some "3D creation suite"
      , runtime =
          (../Podenv.dhall).Nix
            "git+https://codeberg.org/podenv/modularix#blender"
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
