(../Podenv.dhall).Application::{
, runtime = (../Podenv.dhall).Nix "nixpkgs#signal-desktop"
, capabilities = (../Podenv.dhall).Capabilities::{
  , x11 = True
  , pulseaudio = True
  , network = True
  }
}
