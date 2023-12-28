# Try Nix with Podenv

This tutorial teachs you how to use the Nix package manager with Podenv.
If you don't have the `podenv` command already installed, read the [Install and use podenv tutorial](./install.md) first.

> All the podenv commands are executed inside rootless containers so that they don't modify your host.

# Usage

You can run most nix commands by prefixing them with podenv:

```ShellSession
$ podenv nix shell nixpkgs#cowsay --command cowsay hi o/
```

To run local application, adds the `--cwd` argument. For example, to run [devenv](https://github.com/podenv/devenv) you can:

```ShellSession
$ git clone https://github.com/podenv/devenv
$ cd devenv
$ podenv --cwd nix run .#emacs-minimal
```

Run a fully qualified installable:

```ShellSession
$ podenv nix run github:NixOS/nixpkgs/b569c6ef73baceebea085a143d84b0391ab18ce7#dhall -- --version
1.40.2
```

Display a flake content:

```ShellSession
$ podenv nix flake show github:podenv/modularix
```

Run an existing application defined with nix, for example [vcv.dhall](https://github.com/podenv/podenv/blob/main/hub/Applications/vcv.dhall):

```ShellSession
$ podenv vcv
```

> The vcv podenv application is a shortcut for `podenv --dri --x11 --alsa --pipewire nix:github:podenv/modularix#vcv`

# Manual usage

You can also add nix to an existing Podenv application.

## Create the store

Run this command to ensure the nix store is configured. This is automatically done when running `podenv nix`, but feel free to run it again to check everything is working:

```ShellSession
$ podenv nix.setup
[Display the nix version]
```

> You can check what the [nix.setup application](https://github.com/podenv/podenv/blob/main/hub/Builders/nix.dhall) does by running `podenv --show nix.setup`.

## Setup the environment

Attach the Nix package manager to an existing application, for example a fedora container:

```ShellSession
$ podenv nixify fedora
```

> The [nixify](https://github.com/podenv/podenv/blob/main/hub/Applications/nixify.dhall) function adds the store volume and the nix command to the PATH.

Then run nix command inside the container:

```ShellSession
fedora-bash$ nix run nixpkgs#hello -- -g "Hello from podenv"
[Display the greeting message]
```

## Nixpkgs channel

The `nix.setup` does not configure the nixpkgs channel, thus to use commands that require a global `nixpkgs`,
run these commands:

```ShellSession
fedora-bash$ nix-channel --add https://nixos.org/channels/nixpkgs-unstable
fedora-bash$ nix-channel --update
```

> This setups a `~/.nix-channels` and `~/.nix-defexpr/` configuration.
> Though it is recommended to use the new nix commands which does not requires such existing channel configuration.

This conclude the tutorial on how to use Nix with podenv.
