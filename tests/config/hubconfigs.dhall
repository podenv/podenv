{- Import of podenv/hub envs -}
let Podenv = ./podenv/package.dhall

let Cap = Podenv.Schemas.Capabilities

let Task = Podenv.Schemas.Task

let Fedora =
      Podenv.Schemas.Env::{
      , url = Some "https://fedoraproject.org/"
      , image = "registry.fedoraproject.org/fedora:30"
      , name = "fedora"
      , command = [ "/bin/bash" ]
      }

let {- When the env, package and command is a single name -} mkSimple =
          \(name : Text)
      ->  \(description : Text)
      ->  { name = name
          , description = Some description
          , packages = Some [ name ]
          , command = [ name ]
          }

let X11 = { x11 = Some True }

let Pulseaudio = { pulseaudio = Some True }

let Network = { network = Some True }

in  Podenv.Schemas.Config::{
    , system = { dns = None Text }
    , environments =
        [     Fedora
          //  mkSimple "pavucontrol" "Adjust audio volumes"
          //  { capabilities = Cap::X11 // Pulseaudio }
        ,     Fedora
          //  mkSimple "xeyes" "Test graphical setup"
          //  { packages = Some [ "xorg-x11-apps" ], capabilities = Cap::X11 }
        ,     Fedora
          //  mkSimple "mupdf" "Display a pdf file"
          //  { capabilities = Cap::X11 }
        ,     Fedora
          //  { name = "python-http-server"
              , description = Some "Expose current directory over HTTP"
              , capabilities = Cap::Network
              , environ = Some (toMap { PORT = "8000", PYTHONUNBUFFERED = "1" })
              , packages = Some [ "python3" ]
              , command = [ "python3", "-m", "http.server" ]
              , ports = Some [ "{PORT}:8000" ]
              }
        ,     Fedora
          //  mkSimple "mumble" "VoIP solution"
          //  { capabilities = Cap::Network // X11 // Pulseaudio }
        ,     Fedora
          //  { name = "sshvpn"
              , description = Some "Create a point-to-point ssh tunel"
              , capabilities =
                  Cap::Network // { tun = Some True, terminal = Some True }
              , pre-tasks =
                  Some
                    [ Task::{
                      , name = Some "Creating local tun"
                      , shell =
                          Some
                            ''
                            ip tuntap add dev tun1 mode tun user user;
                            ip link set tun1 mtu 9000 up
                            ''
                      , unless = Some "ip link show tun1"
                      }
                    , Task::{
                      , name = Some "Creating remote tun"
                      , shell =
                          Some
                            ''
                            ip tuntap add dev tun{REMOTE_TUN}
                            ''
                      }
                    ]
              }
        ]
    }
