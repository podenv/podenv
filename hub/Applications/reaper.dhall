(../Podenv.dhall).Application::{
, description = Some "Complete digital audio production application"
, runtime =
    (./nix.dhall).useInstallables
      [ "github:podenv/modularix/e079cd811ddafa9d931a2185829970ee12866a02#reaper"
      ]
, capabilities = (../Podenv.dhall).Capabilities::{
  , dri = True
  , x11 = True
  , pipewire = True
  }
}
