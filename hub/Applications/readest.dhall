(../Podenv.dhall).Application::{
, description = Some "Modern, feature-rich ebook reader"
, runtime = (../Podenv.dhall).Shell [ ./nixGL.dhall, "nixpkgs#readest" ]
, command = [ "nixGL", "readest" ]
, capabilities = (../Podenv.dhall).Capabilities::{ x11 = True, dri = True }
}
