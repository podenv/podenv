(../Podenv.dhall).Application::{
, description = Some "Media Player"
, runtime = (./nix.dhall).use [ "vlc" ]
, capabilities = (../Podenv.dhall).Capabilities::{
  , dri = True
  , x11 = True
  , pulseaudio = True
  }
}
