(../Podenv.dhall).Application::{
, description = Some "Complete digital audio production application"
, runtime = (./nix.dhall).useInstallables [ "github:podenv/modularix#reaper" ]
, capabilities = (../Podenv.dhall).Capabilities::{
  , dri = True
  , x11 = True
  , pipewire = True
  }
}
