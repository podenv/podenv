(../Podenv.dhall).Application::{
, runtime =
    (./nix.dhall).useInstallables
      [ "nixpkgs/3a228057f5b619feb3186e986dbe76278d707b6e#ollama" ]
, command = [ "ollama" ]
, capabilities = (../Podenv.dhall).Capabilities::{ dri = True }
}
