(../Podenv.dhall).Application::{
, runtime =
    (./nix.dhall).useInstallables
      [ "nixpkgs/d3429a135af936205b7af11a70780cb03260586c#ollama" ]
, command = [ "ollama" ]
, capabilities = (../Podenv.dhall).Capabilities::{ dri = True }
}
