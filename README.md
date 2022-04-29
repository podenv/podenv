# podenv: a podman wrapper

> Note that this is a work in progress, please get in touch if you are interested.

Podenv provides a declarative interface to manage containerized application.
Using rootless containers, podenv let you run applications seamlessly.

## Overview and scope

The main goal of podenv is to convert an application definition into a podman command.

At a high level, the scope of podenv is the following:

* Highlevel capability system to define generic container resources.
* Support desktop application with VPN network namespace.
* User friendly command line interface.
* Functional configuration.

## Usage

Here are some demo use cases:

### Container image

Run a container image: `podenv --rw --cwd --shell image:ubi8`

… starts the following command: `podman run -it --detach-keys '' --network none --rm --volume $(pwd):/data:Z --workdir /data ubi8 /bin/bash`

### Bubblewrap chroot

Run a rootfs: `podenv --shell rootfs:/`

… starts the following command: `bwrap --die-with-parent --unshare-pid --unshare-ipc --unshare-uts --unshare-net --ro-bind /usr /usr --ro-bind /lib64 /lib64 --ro-bind /bin /bin --ro-bind /sbin /sbin --ro-bind /etc /etc --proc /proc --dev /dev --perms 01777 --tmpfs /tmp --clearenv --setenv HOME /var/home/fedora /bin/sh`

### Nix flakes

Run a nix package: `podenv nixpkgs#hello`

… runs the installable using bubblewrap.

### Applications

Run a podenv application: `podenv gimp ./image.png`

… builds a container and starts the following command: `podman run [wayland args] --volume $(pwd)/image.png:/data/image.png gimp /data/image.png`

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
