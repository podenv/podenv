let Prelude = ../Prelude.dhall

let Podenv = ../Podenv.dhall

let image-ref = \(ver : Text) -> "registry.fedoraproject.org/fedora${ver}"

let mkVolumes = \(ver : Text) -> [ "cache-dnf-${ver}:/var/cache/dnf" ]

let base-image =
      \(from : Text) ->
      \(ver : Text) ->
      \(pre-task : Text) ->
      \(pkgs : List Text) ->
        Podenv.ContainerBuild::{
        , containerfile =
            ''
            FROM ${from}
            ARG USER_UID
            RUN ${./mkUser.dhall "fedora"}
            ${pre-task}
            RUN dnf update -y
            RUN dnf install -y ${Prelude.Text.concatSep " " pkgs}
            ENV USER=fedora
            ''
        , image_volumes = mkVolumes ver
        , image_home = Some "/home/fedora"
        , image_update = Some "dnf update -y"
        }

let image = \(ver : Text) -> base-image (image-ref (":" ++ ver)) ver

in  { image-ref, mkVolumes, base-image, image }
