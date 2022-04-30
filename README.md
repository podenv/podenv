# podenv: a container wrapper

> Note that this is a work in progress, please get in touch if you are interested.

Podenv provides a declarative interface to manage containerized applications.
Using rootless containers, podenv let you run applications seamlessly.

## Overview and scope

The goal of podenv is to implement a modern application launcher:

- [Highlevel capability system](#capabilities)
- [Runtime agnostic](#runtimes)
- [VPN network namespace](#namespace)
- [Functional configuration](#configuration)
- [Simple to use](#usages)

Podenv differs from [toolbx](https://containertoolbx.org/) or [flatpak](https://flatpak.org/):

- Isolation by default: network or home directory access need to be explicitely enabled.
- Unopinionated runtime: applications are provided by distro packages or Containerfile.
- High level command line interface.

## Features

### <a name="capabilities"></a>Capabilities

Share resources with simple toggles:

- `--wayland` graphical display.
- `--pipewire` access audio and video streams.
- `--dbus` share the dbus session.
- See the full list in this configuration schema: [Capabilities.Type](https://github.com/podenv/hub/blob/main/schemas/Capabilities.dhall)

Mount directories with smart volumes:

- `--volume ~` share the home directory.
- `--volume web:~` use a volume named `web` for the container home.
- `--hostfile ./document.pdf` share a single file.

### <a name="runtimes"></a>Runtimes

Podenv works with multiple runtimes:

- Podman for container image and Containerfile.
- Bubblewrap for local rootfs and Nix Flakes.

The runtime integration is decoupled from the application description
so that more options can be added in the future.

### <a name="namespaces"></a>Namespace

Applications can share resources using the `--namespace NAME` option.
For example, a browser application can be attached to the network of a VPN application.
Checkout the [Configure a VPN howto](./docs/howtos/vpn.md).

### <a name="namespaces"></a>Configuration

Applications are user-defined with functionnal and re-usable expressions:

#### Firefox with a fedora container

```dhall
Application::{
, name = "firefox"
, description = Some "Mozilla Firefox"
, runtime = (./fedora.dhall).latest.useGraphic [ "firefox" ]
, command = [ "firefox", "--no-remote" ]
, capabilities = Capabilities::{ wayland = True, network = True }
}
```

The fedora useGraphic function defines a custom Containerfile:

```dhall
\(pkgs : List Text) ->
ContainerBuild::{
, containerfile =
    ''
    FROM fedora:latest
    RUN ${./mkUser.dhall "fedora"}
    RUN dnf install -y mesa-dri-drivers pipewire-libs
    RUN dnf update -y
    RUN dnf install -y ${concatSep " " pkgs}
    ''
, image_home = Some "/home/fedora"
, image_update = Some "dnf update -y"
}
```

> The graphic packages layer is shared by the other apps.

#### Nix Flakes

Podenv support the [Nix installables syntax](https://nixos.org/manual/nix/stable/command-ref/new-cli/nix.html#installables):

```dhall
Application::{
, name = "polyglot"
, description = Some "Tool to count lines of source code."
, runtime = Nix Flakes::{ installables = [ "github:podenv/polyglot.nix" ]}
, capabilities = Capabilities::{ cwd = True }
}
```

#### Hub

By default, podenv uses the [podenv/hub](https://github.com/podenv/hub) collection.
Run `podenv --list` to see the available applications.


## <a name="usages"></a>Usage

Podenv provides a simple command line: `podenv [--caps] application-name [args]`.

Here are some common use cases:

### Applications

```ShellSession
$ podenv gimp ./image.png
```

… runs the following command: `podman run [wayland args] --volume $(pwd)/image.png:/data/image.png localhost/gimp /data/image.png`

If necessary, podenv builds a local image using the Containerfile defined by the application.

### Container image

```ShellSession
$ podenv --rw --network --root --cwd --shell image:ubi8
```

… runs the following command: `podman run --rm -it --detach-keys '' --volume $(pwd):/data:Z --workdir /data --volume ~/.local/share/podenv/volumes/image-ubi8-home:/root ubi8 /bin/bash`

By default podenv mounts a local volumes for the home directory.

### Bubblewrap chroot

Extract a container image and execute it with bubblewrap:

```ShellSession
$ podenv --volume rawhide:/mnt image:fedora:rawhide bash -c "tar --one-file-system -cf - / | tar -C /mnt -xf -"
$ podenv --network --rw --root rootfs:rawhide
```

… extracts the rootfs with: `podman run --rm --read-only=true --network none --volume ~/.local/share/podenv/volumes/rawhide:/mnt fedora:rawhide bash -c "tar ..."`

… and, runs the following command: `bwrap [unshare args] --bind ~/.local/share/podenv/volumes/rawhide / --bind ~/.local/share/podenv/volumes/rootfs-7e08b7-home /root /bin/sh`

This is useful to avoid polluting the container storage.

### Nix flakes

```ShellSession
$ podenv nixpkgs#hello
```

… runs the installable using bubblewrap: `bwrap [unshare args] --bind ~/.local/share/podenv/volumes/nix-store /nix --bind ~/.local/share/podenv/volumes/nix-cache ~/.cache/nix --clearenv --setenv NIX_SSL_CERT_FILE /etc/pki/tls/certs/ca-bundle.crt nix --extra-experimental-features "nix-command flakes" run nixpkgs#hello`

If necessary, podenv automatically installs the Nix toolchain using bubblewrap with the [nix.setup application](https://github.com/podenv/hub/blob/main/Builders/nix.dhall).


# Documentation

Podenv documentation is organized into the following [four sections][documentation]:

[documentation]: https://www.divio.com/en/blog/documentation/

## Tutorials

These guides help you get your hands dirty with working examples:

* [Install and use podenv](./docs/tutorials/install.md)
* [Use an application](./docs/tutorials/use.md)
* [Create an application](./docs/tutorials/create.md)

## Howtos

These cookbooks teach you how to solve specific tasks:

* [Configure a VPN](./docs/howtos/vpn.md)
* [Setup a custom image](./docs/howtos/image.md)
* [Contribute to podenv](./docs/howtos/contribute.md)

## Discussions

These posts explain the context and motivation behind this tool:

* [Declarative containers](./docs/discussions/declarative-containers.md)
* [Using dhall for configuration](./docs/discussions/dhall-configuration.md)
* [Roadmap](./docs/discussions/roadmap.md)

## References

These comprehensive resources cover details that other texts will gloss over:

* [Command line](./docs/references/command-line.md)
* [Configuration](./docs/references/configuration.md)
