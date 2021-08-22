-- | The application runtime:
-- Image: an existing image ref
-- Nix: a nix expression
-- Container: a static container build
< Image : Text | Nix : Text | Container : (./ContainerBuild.dhall).Type >
