let Podenv = ../Podenv.dhall

let default =
      Podenv.Application::{
      , description = Some
          "A unix commandline program that manages encrypted password databases"
      , runtime = (./fedora.dhall).latest.use [ "pwsafe", "wl-clipboard" ]
      , capabilities = Podenv.Capabilities::{
        , terminal = True
        , interactive = True
        }
      }

let copy =
      \(name : Text) ->
            default
        //  { description = Some "Copy a password to the wayland clipboard"
            , capabilities = default.capabilities // { wayland = True }
            , command =
              [ "sh", "-c", "pwsafe -E -p ${name} | wl-copy -f -o -p" ]
            }

in  { default, copy }
