{ Application = ./Application.dhall
, Capabilities = ./Capabilities.dhall
, ContainerBuild = ./ContainerBuild.dhall
, System = ./System.dhall
, Tcp = (./Provider.dhall).Tcp
, Image = (./Runtime.dhall).Image
, Nix = (./Runtime.dhall).Nix
, Container = (./Runtime.dhall).Container
, Containerfile =
    let -- "A helper function to simplify documentation..."
        func
        : Text -> ./Runtime.dhall
        = \(containerfile : Text) ->
            (./Runtime.dhall).Container
              (./ContainerBuild.dhall)::{ containerfile }

    in  func
}
