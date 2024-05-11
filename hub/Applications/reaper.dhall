(../Podenv.dhall).Application::{
, description = Some "Complete digital audio production application"
, runtime =
    (./nix.dhall).useInstallables
      [ "github:podenv/modularix/c3077ce442d1d6ca9cfaeb110cfb22c69826440d#reaper"
      ]
, capabilities = (../Podenv.dhall).Capabilities::{
  , dri = True
  , x11 = True
  , pipewire = True
  }
}
