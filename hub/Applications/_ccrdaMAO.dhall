let Podenv = ../Podenv.dhall

let url =
      let base =
            "https://ccrma.stanford.edu/planetccrma/mirror/fedora/linux/planetccrma"

      let release =
            "\$(rpm -E %fedora)/x86_64/planetccrma-repo-1.1-3.fc\$(rpm -E %fedora).ccrma.noarch.rpm"

      in  "${base}/${release}"

in  \(description : Text) ->
    \(packages : List Text) ->
    \(command : Text) ->
    \(alsa : Bool) ->
      Podenv.Application::{
      , description = Some description
      , runtime =
          Podenv.Container
            ( (./fedora.dhall).useGraphicImage
                "latest"
                "RUN rpm -Uvh ${url}"
                packages
            )
      , command = [ command ]
      , capabilities = Podenv.Capabilities::{
        , pipewire = True
        , x11 = True
        , alsa
        }
      }
