-- | The default nix builder
-- The purpose of this application is to setup the nix store and provide a known location for
-- the nix command line interface (at /nix/var/nix/profiles/nix-install)
--
-- When podenv runs a nix installable, if the nix-store volume is empty, then this
-- application is automatically executed (using the `nix.setup` app selector).
let Prelude = ../Prelude.dhall

let Podenv = ../Podenv.dhall

in  Podenv.Application::{
    , description = Some "Setup the nix store"
    , capabilities = Podenv.Capabilities::{ network = True }
    , command =
        let version = "2.24.12"

        let hash =
              "abbc5cbd390e4fe3b38bc3f15a5eda2750cfd1299db0abc6915a08391d9441dc"

        let default-config =
              ''
              sandbox = false
              build-users-group =
              experimental-features = nix-command flakes
              ''

        let args =
              [ "set -x;"
              , "test -d /nix/var || ("
              , Prelude.Text.concatSep
                  " && "
                  [ "cd /tmp"
                  , "curl -OL https://nixos.org/releases/nix/nix-${version}/nix-${version}-x86_64-linux.tar.xz"
                  , "echo '${hash}  nix-${version}-x86_64-linux.tar.xz' | sha256sum -c"
                  , "tar xf nix-${version}-x86_64-linux.tar.xz"
                  , "/tmp/nix-${version}-x86_64-linux/install --no-daemon --no-channel-add --no-modify-profile "
                  , "rm -r /tmp/nix-*-x86_64-linux*"
                  , "cp -P ~/.local/state/nix/profiles/profile-1-link /nix/var/nix/profiles/nix-install"
                  , "/nix/var/nix/profiles/nix-install/bin/nix-collect-garbage --delete-old"
                  , "/nix/var/nix/profiles/nix-install/bin/nix-store --optimise"
                  , "/nix/var/nix/profiles/nix-install/bin/nix-store --verify --check-contents"
                  ]
              , "); test -f ~/.config/nix/nix.conf || ("
              , "echo -en ${Text/show default-config} > ~/.config/nix/nix.conf"
              , "); /nix/var/nix/profiles/nix-install/bin/nix --version"
              ]

        in  [ "bash", "-c", Prelude.Text.concatSep " " args ]
    , volumes =
      [ "nix-store:/nix", "nix-config:~/.config/nix", "nix-setup-home:~/" ]
    , runtime = Podenv.Rootfs "/"
    }
