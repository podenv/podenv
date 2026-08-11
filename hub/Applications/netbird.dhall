let Podenv = ../Podenv.dhall

let pre-task =
      let repo =
            ''
            [NetBird]
            name=NetBird
            baseurl=https://pkgs.netbird.io/yum/
            enabled=1
            gpgcheck=0
            gpgkey=https://pkgs.netbird.io/yum/repodata/repomd.xml.key
            repo_gpgcheck=1
            ''

      in  ''
          RUN echo -e ${Text/show repo} > /etc/yum.repos.d/netbird.repo
          ''

in      (./fedora.dhall).useGraphic
          "VPN solution"
          pre-task
          [ "--setopt=tsflags=noscripts"
          , "netbird"
          , "iproute"
          , "firefox"
          , "xdg-open"
          , "gdouros-symbola-fonts"
          , "nftables"
          ]
    //  { command = [ "netbird", "up", "--foreground-mode" ]
        , syscaps =
          [ "NET_ADMIN", "NET_RAW", "SYS_RESOURCE", "SETUID", "SETGID" ]
        , capabilities = (../Podenv.dhall).Capabilities::{
          , network = True
          , tun = True
          , dri = True
          , root = True
          , privileged = True
          , interactive = True
          , terminal = True
          , wayland = True
          }
        }
