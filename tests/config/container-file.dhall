{- A container-image config -}
let Podenv = env:PODENV_PRELUDE

let mkBuildEnv =
          \(mounts : List Podenv.Types.Mount)
      ->  Podenv.Schemas.BuildEnv::{ mounts = mounts }

let mkMount =
          \(container : Text)
      ->  \(host : Text)
      ->  { HostPath = host, ContainerPath = container }

let fedoraImage =
          \(packages : List Text)
      ->  let concat = (env:PODENV_HUB).Prelude.Text.concatSep

          in  { container-file =
                  Some
                    [ Podenv.Schemas.Task::{
                      , name = Some "TODO: expand this..."
                      , command =
                          Some
                            ''
                            FROM registry.fedoraproject.org/fedora:30
                            RUN dnf install -y https://dowload1.rpmfusion.org/free/...
                            RUN dnf install -y ${concat " " packages}
                            ''
                      }
                    ]
              , container-update =
                  Some
                    [ Podenv.Schemas.Task::{
                      , name = Some "Update package"
                      , command = Some "dnf update -y"
                      }
                    ]
              , build-env =
                  mkBuildEnv [ mkMount "/var/cache/dnf" "~/.cache/podenv/dnf" ]
              }

in  [     fedoraImage [ "firefox", "ffmpeg", "mesa-dri-drivers" ]
      //  { name = "firefox", capabilities = { x11 = True, dri = True } }
    ]
