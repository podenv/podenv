let Podenv = ../Podenv.dhall

in  \(description : Text) ->
    \(packages : List Text) ->
    \(command : Text) ->
          (./fedora.dhall).useGraphic
            description
            ((./fedora.dhall).useCopr "ycollet/audinux")
            packages
      //  { command = [ command ]
          , capabilities = Podenv.Capabilities::{
            , pipewire = True
            , x11 = True
            , alsa = True
            }
          }
