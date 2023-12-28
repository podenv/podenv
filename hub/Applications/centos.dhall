let Podenv = ../Podenv.dhall

let image-ref = \(ver : Text) -> "registry.centos.org/centos:${ver}"

let default =
      Podenv.Application::{
      , name = "centos-7"
      , description = Some "CentOS 7 shell"
      , runtime = Podenv.Image (image-ref "7")
      , volumes = [ "cache-yum-c7:/var/cache/yum" ]
      }

in  { default }
