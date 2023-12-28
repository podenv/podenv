let Prelude = ../Prelude.dhall

let Podenv = ../Podenv.dhall

let image-ref = \(ver : Text) -> "docker.io/library/debian:${ver}"

let mkVolumes = \(ver : Text) -> [ "cache-apt-${ver}:/var/cache/apt" ]

let image =
      \(ver : Text) ->
      \(pkgs : List Text) ->
        Podenv.ContainerBuild::{
        , containerfile =
            ''
            FROM ${image-ref ver}
            ARG USER_UID
            RUN ${./mkUser.dhall "debian"}
            RUN apt-get update
            RUN apt-get install -y ${Prelude.Text.concatSep " " pkgs}
            ENV USER=debian
            ''
        , image_volumes = mkVolumes ver
        , image_home = Some "/home/debian"
        , image_update = Some "apt-get update"
        }

in  { image-ref, mkVolumes, image }
