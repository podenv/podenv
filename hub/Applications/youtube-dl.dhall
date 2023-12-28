    (./fedora.dhall).useSimple
      "youtube-dl"
      "command-line program to download videos"
//  { capabilities = (../Podenv.dhall).Capabilities::{
      , network = True
      , cwd = True
      , terminal = True
      }
    }
