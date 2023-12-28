let Podenv = ../Podenv.dhall

let container =
      let repo =
            ''
            [brave-browser]
            name=created by dnf config-manager from https://brave-browser-rpm-release.s3.brave.com/x86_64/
            baseurl=https://brave-browser-rpm-release.s3.brave.com/x86_64/
            enabled=1
            ''

      let key = "https://brave-browser-rpm-release.s3.brave.com/brave-core.asc"

      let extra =
            ''
            RUN rpm --import ${key}
            RUN echo -e ${Text/show repo} > /etc/yum.repos.d/brave-browser.repo
            ''

      in  \(packages : List Text) ->
            Podenv.Container
              ((./fedora.dhall).useGraphicCodecImage "latest" extra packages)

let default =
      Podenv.Application::{
      , description = Some "Brave Browser"
      , runtime = container [ "brave-browser" ]
      , command = [ "brave-browser" ]
      , capabilities = Podenv.Capabilities::{
        , dbus = True
        , network = True
        , x11 = True
        , dri = True
        , video = True
        , pipewire = True
        , pulseaudio = True
        }
      }

in  { default }
