-- | The application runtime:
-- Image: an existing image ref
-- Nix: a nix installable flake app or package
-- DevShell: a nix devshell
-- Shell: a list of nix installable available in PATH
-- Container: a static container build
-- Rootfs: a local directory
< Image : Text
| Nix : Text
| DevShell : Text
| Shell : List Text
| Container : (./ContainerBuild.dhall).Type
| Rootfs : Text
>
