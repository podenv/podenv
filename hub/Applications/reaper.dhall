(../Podenv.dhall).Application::{
, description = Some "Complete digital audio production application"
, runtime =
    (../Podenv.dhall).Nix
      "github:podenv/modularix/31d3399671a2e85c434ab9a507f508c4de4f3e9b#reaper"
, capabilities = (../Podenv.dhall).Capabilities::{
  , dri = True
  , x11 = True
  , pipewire = True
  }
}
