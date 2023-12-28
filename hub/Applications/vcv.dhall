(../Podenv.dhall).Application::{
, description = Some "The EuroRack simulator"
, runtime = (./nix.dhall).useInstallables [ "github:podenv/modularix#vcv" ]
, capabilities = (../Podenv.dhall).Capabilities::{
  , dri = True
  , x11 = True
  , pipewire = True
  , alsa = True
  }
}
