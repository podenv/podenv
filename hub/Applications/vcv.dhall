(../Podenv.dhall).Application::{
, description = Some "The EuroRack simulator"
, runtime =
    (./nix.dhall).useInstallables
      [ "github:podenv/modularix/e079cd811ddafa9d931a2185829970ee12866a02#vcv" ]
, capabilities = (../Podenv.dhall).Capabilities::{
  , dri = True
  , x11 = True
  , pipewire = True
  , alsa = True
  }
}
