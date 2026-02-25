-- | This application includes a custom runtime to install the upstream repository key.
let Podenv = ../Podenv.dhall

let pre-task =
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

      in  ''
          RUN rpm --import ${key} && echo -e ${Text/show
                                                 repo} > /etc/yum.repos.d/vscode.repo
          ''

in      (./fedora.dhall).useGraphic
          "Code editing"
          pre-task
          [ "code", "iproute" ]
    //  { command = [ "code", "--wait" ]
        , capabilities = Podenv.Capabilities::{ wayland = True, x11 = True }
        }
