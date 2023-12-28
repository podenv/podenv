let Podenv = ../Podenv.dhall

in  Podenv.Application::{
    , description = Some "Steam"
    , command = [ "steam" ]
    , runtime = (./fedora.dhall).fusion.nonfree.use [ "steam", "pciutils" ]
    , capabilities = Podenv.Capabilities::{
      , dri = True
      , x11 = True
      , pulseaudio = True
      , network = True
      , privileged = True
      }
    }
