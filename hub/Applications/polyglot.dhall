(../Podenv.dhall).Application::{
, description = Some "Tool to count lines of source code."
, runtime = (./nix.dhall).useInstallables [ "github:podenv/polyglot.nix" ]
, capabilities = (../Podenv.dhall).Capabilities::{ cwd = True }
}
