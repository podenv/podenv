(../Podenv.dhall).Application::{
, description = Some "command-line program to download videos"
, runtime =
    (../Podenv.dhall).Nix
      "github:NixOS/nixpkgs/7dd2083f8f68c32df7fb23f4bc4881010ccbfac8#yt-dlp"
, command = [ "yt-dlp" ]
, capabilities = (../Podenv.dhall).Capabilities::{
  , network = True
  , cwd = True
  , terminal = True
  }
}
