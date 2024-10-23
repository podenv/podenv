(../Podenv.dhall).Application::{
, description = Some "Complete digital audio production application"
, runtime =
    (./nix.dhall).useInstallables
      [ "github:podenv/modularix/78ffb5608dec9a512a52b8c00bd4c95b7ed5e862#reaper"
      ]
, capabilities = (../Podenv.dhall).Capabilities::{
  , dri = True
  , x11 = True
  , pipewire = True
  }
}
