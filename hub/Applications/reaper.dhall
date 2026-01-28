(../Podenv.dhall).Application::{
, description = Some "Complete digital audio production application"
, runtime =
    (../Podenv.dhall).Nix "git+https://codeberg.org/podenv/modularix#reaper"
, capabilities = (../Podenv.dhall).Capabilities::{
  , dri = True
  , x11 = True
  , pipewire = True
  }
}
