\(name : Text) ->
\(desc : Text) ->
  let Podenv = ../Podenv.dhall

  in  Podenv.Application::{
      , description = Some desc
      , command = [ name ]
      , runtime = (./fedora.dhall).latest.useGraphic [ name ]
      , capabilities = Podenv.Capabilities::{
        , hostfile = True
        , wayland = True
        , x11 = True
        }
      }
