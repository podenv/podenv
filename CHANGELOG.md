# Changelog

## next

- added XDG_CURRENT_DESKTOP environment
- added XAUTHORITY environment
- changed Nix runtime definition to contain a single installable
- added support for local flake usage

## 0.6.0 (2024-10-16)

- added katrain and cgoban app
- updated blender and obs app

## 0.5.0 (2024-05-11)

- the `--ps` command can take a app name to return the pod id.
- set the SDL_VIDEODRIVER env for wayland

## 0.4.0 (2023-12-28)

- the podenv/hub submodule is now part of the podenv repository to simplify updates.

## 0.3.0 (2023-10-27)

- application name is now optional: when unset, multiple instance can run in parallel.
- the `--network` argument now takes an argument: private, host or a shared name.
- add `--ps` argument to list running app.
- add `--syscap` argument to add capabilities(7).
- add support for nix flake binary cache.
- use zenity to log non-interactive message.
- after use, podenv may propose to update the runtime when the build is old.
- fix --dri and --wayland with nvidia.
- support latest podman.

## 0.2.0 (2022-07-26)

- restrict ssh capability to the agent socket.
- home volume is set to the namespace name if a namespace is defined.
- allow passing args directly to a command: `podenv nix:github:podenv/devenv#emacs --version`
- fix syscaps argument for bwrap.
- use system chcon to set label.
- add --manifest argument to show every configuration context.

## 0.1.0 (2022-05-02)

- Initial release
