-- | This application includes a builder
let Podenv = ../Podenv.dhall

let debian = ../Builders/debian.dhall

let mkUse =
      \(version : Text) ->
      \(packages : List Text) ->
        Podenv.Container (debian.image version packages)

let default =
      \(version : Text) ->
        Podenv.Application::{
        , description = Some "Debian ${version} shell"
        , runtime = Podenv.Image (debian.image-ref version)
        , volumes = debian.mkVolumes version
        , capabilities = Podenv.Capabilities::{
          , terminal = True
          , interactive = True
          , network = True
          , rw = True
          }
        }

in  { default = default "latest", bullseye.use = mkUse "bullseye" }
