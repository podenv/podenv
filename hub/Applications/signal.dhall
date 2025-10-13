(../Podenv.dhall).Application::{
, runtime =
    (../Podenv.dhall).Nix "github:NixOS/nixpkgs/nixos-25.05#signal-desktop"
, capabilities = (../Podenv.dhall).Capabilities::{
  , x11 = True
  , pulseaudio = True
  , network = True
  }
}
