(../Podenv.dhall).Application::{
, description = Some "Flutter SDK"
, runtime =
    (./nix.dhall).uses
      [ ./nixGL.dhall ]
      [ "flutter"
      , "clang"
      , "pkg-config"
      , "gtk3"
      , "pcre"
      , "epoxy"
      , "glib.dev"
      , "mount"
      ]
, command = [ "nixGL", "bash" ]
, capabilities = (../Podenv.dhall).Capabilities::{
  , terminal = True
  , interactive = True
  , dri = True
  , x11 = True
  , pulseaudio = True
  }
}
