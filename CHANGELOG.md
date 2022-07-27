# Changelog

## 0.3.0 (next)

- application name is now optional: when unset, multiple instance can run in parallel.
- the `--network` argument now takes an argument: private, host or a shared name.
- add `--ps` argument to list running app.

## 0.2.0 (2022-07-26)

- restrict ssh capability to the agent socket.
- home volume is set to the namespace name if a namespace is defined.
- allow passing args directly to a command: `podenv nix:github:podenv/devenv#emacs --version`
- fix syscaps argument for bwrap.
- use system chcon to set label.
- add --manifest argument to show every configuration context.

## 0.1.0 (2022-05-02)

- Initial release
