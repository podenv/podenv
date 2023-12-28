{ Application = ./Application.dhall
, ApplicationResource = ./ApplicationResource.dhall
, Metadata = ./Metadata.dhall
, Capabilities = ./Capabilities.dhall
, Network = ./Network.dhall
, ContainerBuild = ./ContainerBuild.dhall
, Image = (./Runtime.dhall).Image
, Nix = (./Runtime.dhall).Nix
, Flakes = ./Flakes.dhall
, Container = (./Runtime.dhall).Container
, Rootfs = (./Runtime.dhall).Rootfs
, Containerfile =
    let -- "A helper function to simplify documentation..."
        func
        : Text -> ./Runtime.dhall
        = \(containerfile : Text) ->
            (./Runtime.dhall).Container
              (./ContainerBuild.dhall)::{ containerfile }

    in  func
}
