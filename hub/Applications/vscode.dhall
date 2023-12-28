-- | This application includes a custom runtime to install the upstream repository key.
let Podenv = ../Podenv.dhall

let container =
      let repo =
            ''
            [code]
            name=Visual Studio Code
            baseurl=https://packages.microsoft.com/yumrepos/vscode
            enabled=1
            gpgcheck=1
            gpgkey=https://packages.microsoft.com/keys/microsoft.asc
            ''

      let key = "https://packages.microsoft.com/keys/microsoft.asc"

      let extra =
            ''
            RUN rpm --import ${key}
            RUN echo -e ${Text/show repo} > /etc/yum.repos.d/vscode.repo
            ''

      in  \(packages : List Text) ->
            Podenv.Container
              ((./fedora.dhall).useGraphicCodecImage "latest" extra packages)

let default =
      Podenv.Application::{
      , description = Some "Code editing"
      , runtime = container [ "code", "iproute" ]
      , command = [ "code", "--wait" ]
      , capabilities = Podenv.Capabilities::{ wayland = True, x11 = True }
      }

in  { default }
