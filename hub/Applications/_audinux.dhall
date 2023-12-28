let Podenv = ../Podenv.dhall

in  \(description : Text) ->
    \(packages : List Text) ->
    \(command : Text) ->
      Podenv.Application::{
      , description = Some description
      , runtime =
          Podenv.Container
            ( (./fedora.dhall).useGraphicImage
                "latest"
                ((./fedora.dhall).useCopr "ycollet/audinux")
                packages
            )
      , command = [ command ]
      , capabilities = Podenv.Capabilities::{
        , pipewire = True
        , x11 = True
        , alsa = True
        }
      }
