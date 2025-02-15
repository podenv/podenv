let Podenv = ../Podenv.dhall

let setup = ../Builders/nix.dhall

let -- | A pinned reference for the nixpkgs
    nixpkgs =
      "github:NixOS/nixpkgs/8b5b6723aca5a51edf075936439d9cd3947b7b2c"

let -- | Setup a Nix runtime with nixpkgs packages
    use =
      \(name : Text) -> Podenv.Nix "${nixpkgs}#${name}"

let useExample = assert : use "bash" === Podenv.Nix "${nixpkgs}#bash"

let default =
      Podenv.Application::{
      , description = Some "Run the nix command"
      , capabilities = Podenv.Capabilities::{
        , terminal = True
        , interactive = True
        , network = True
        }
      , runtime = use "nix"
      }

in  { setup, use, default }
