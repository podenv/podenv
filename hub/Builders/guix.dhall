-- | The default guix builder
-- The purpose of this application is to setup the guix store
--
-- Note that the extracted tarball file can't be deleted from within this environment,
-- that's why this is using 'rsync' to move them to the volumes
let Prelude = ../Prelude.dhall

let Podenv = ../Podenv.dhall

in  Podenv.Application::{
    , description = Some "Setup the guix store"
    , capabilities = Podenv.Capabilities::{ network = True }
    , command =
        let version = "3143"

        let hash =
              "43131b6ff30f08c48a4b37e95cd16f22a3eb548d6f2c1a65f173e785496cde70"

        let args =
              [ "set -x;"
              , "test -d /gnu/store || ("
              , Prelude.Text.concatSep
                  " && "
                  [ "cd /tmp"
                  , "curl -OL https://ci.guix.gnu.org/download/${version}"
                  , "echo '${hash}  ${version}' | sha256sum -c"
                  , "tar --warning=no-timestamp -xf ${version}"
                  , "rsync -a gnu/ /gnu/"
                  , "rsync -a var/guix/ /var/guix/"
                  ]
              , ");"
              , "export PATH=\$PATH:/var/guix/profiles/per-user/root/current-guix/bin/;"
              , "guix archive --authorize < /var/guix/profiles/per-user/root/current-guix/share/guix/bordeaux.guix.gnu.org.pub;"
              , "guix archive --authorize < /var/guix/profiles/per-user/root/current-guix/share/guix/ci.guix.gnu.org.pub;"
              , "guix --version"
              ]

        in  [ "bash", "-c", Prelude.Text.concatSep " " args ]
    , volumes =
      [ "guix-store:/gnu", "guix-etc:/etc/guix", "guix-var:/var/guix" ]
    , runtime = Podenv.Rootfs "/"
    }
