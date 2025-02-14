(../Podenv.dhall).Application::{
, description = Some "The EuroRack simulator"
, runtime =
    (../Podenv.dhall).Nix
      "github:podenv/modularix/b0b6a962a27c46379008e29db47f18e67f63088c#vcv"
, capabilities = (../Podenv.dhall).Capabilities::{
  , dri = True
  , x11 = True
  , pipewire = True
  , alsa = True
  }
}
