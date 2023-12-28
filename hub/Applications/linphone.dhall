    (./fedora.dhall).useGraphicSimple "linphone" "Open Source VOIP project"
//  { capabilities = (../Podenv.dhall).Capabilities::{
      , network = True
      , pulseaudio = True
      , x11 = True
      }
    }
