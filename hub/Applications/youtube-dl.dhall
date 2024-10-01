(../Podenv.dhall).Application::{
, description = Some "command-line program to download videos"
, runtime =
    (./nix.dhall).useInstallables
      [ "github:NixOS/nixpkgs/84bd184f8179ff6999d6d10e6b0ca575b4118674#yt-dlp" ]
, command = [ "yt-dlp" ]
, capabilities = (../Podenv.dhall).Capabilities::{
  , network = True
  , cwd = True
  , terminal = True
  }
}
