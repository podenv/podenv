let Podenv = ../Podenv.dhall

let setup = ../Builders/nix.dhall

let -- | Add nix to the application environment
    nixify =
      \(app : Podenv.Application.Type) ->
            app
        //  { environ =
                  app.environ
                # [ "NIX_SSL_CERT_FILE=/etc/pki/tls/certs/ca-bundle.crt"
                  , "PATH=/nix/var/nix/profiles/nix-install/bin:/bin:/sbin"
                  ]
            , volumes =
                  app.volumes
                # [ "nix-store:/nix"
                  , "nix-config:~/.config/nix"
                  , "nix-cache:~/.cache/nix"
                  ]
            }

in  nixify
