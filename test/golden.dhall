let Podenv = env:PODENV

let Hub = Podenv.Hub

let legacy =
      let ns = { namespace = Some "corp-vpn" }

      in  { vpn = Hub.openvpn // { volumes = [ "~/.config/openvpn:~/" ] } // ns
          , web =
                  Hub.firefox.default
              //  { volumes = [ "~/.config/firefox-vpn:~" ] }
              //  ns
          }

let corp =
      let ns =
            \(name : Text) ->
            \(application : Podenv.Application.Type) ->
              Podenv.ApplicationResource::{
              , application
              , volumes = [ "~/.config/${name}:~/" ]
              , network = Podenv.Network.Shared "corp-vpn"
              }

      in  { vpn =
              (ns "openvpn" Podenv.Hub.openvpn)
            with metadata.name = Some "vpn"
          , web = ns "firefox-vpn" Podenv.Hub.firefox.default
          , bridge = ns "bridge" (Podenv.Hub.ssh.client "user@internal-host")
          }

let podenv =
      { runtime =
          Podenv.Nix
            Podenv.Flakes::{
            , installables = [ "github:podenv/podenv" ]
            , cache = Some
                "podenv.cachix.org-1:FA80Dv5XSHxzMYOg1wEANhDN7h43nN8twBJiYNzcDGY="
            }
      }

in  Podenv.Hub // { legacy, corp, ubi.runtime.image = "ubi8", podenv }
