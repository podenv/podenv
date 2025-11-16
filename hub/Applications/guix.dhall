let Podenv = ../Podenv.dhall

let setup = ../Builders/guix.dhall

let default =
      Podenv.Application::{
      , description = Some "Run the guix command"
      , capabilities = Podenv.Capabilities::{
        , terminal = True
        , interactive = True
        , network = True
        }
      , volumes =
        [ "guix-store:/gnu", "guix-etc:/etc/guix", "guix-var:/var/guix" ]
      , runtime = Podenv.Rootfs "/"
      , environ =
        [ "PATH=/bin:/var/guix/profiles/per-user/root/current-guix/bin" ]
      }

in  { setup, default }
