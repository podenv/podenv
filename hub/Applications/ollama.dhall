(../Podenv.dhall).Application::{
, runtime =
    (../Podenv.dhall).Shell
      [ "nixpkgs/3a228057f5b619feb3186e986dbe76278d707b6e#ollama" ]
, capabilities = (../Podenv.dhall).Capabilities::{ dri = True, terminal = True }
}
