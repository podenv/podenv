# podenv: a podman wrapper

Podenv provides a declarative interface to manage containers' environment.

## Overview and scope

At a high level, the scope of podenv is the following:

* Highlevel capability system to define container resources.
* Automatic image management to optimize as much as possible the runtime memory.
* Support desktop application and custom network isolation for VPNs.
* Convenient command line and configuration files.

Podenv is similar to [toolbox](https://github.com/debarshiray/toolbox) or
[flatpak](https://flatpak.org/) and it is inspired by
[nix-shell](https://nixos.org/nixos/nix-pills/developing-with-nix-shell.html)
and [guix environment](https://guix.gnu.org/manual/en/html_node/Invoking-guix-environment.html).


# Documentation

Podenv documentation is organized into the following [four sections][documentation]:

[documentation]: https://www.divio.com/en/blog/documentation/

## Tutorials

These guides help you get your hands dirty with working examples:

* [Install and use podenv](./docs/tutorials/install.md)
* [Create an environment](./docs/tutorials/create.md)
* [Use an environment](./docs/tutorials/use.md)

## Howtos

These cookbooks teach you how to solve specific tasks:

* [Configure a VPN](./docs/howtos/vpn.md)
* [Contribute to podenv](./docs/howtos/contribute.md)

## Discussions

These posts explain the context and motivation behind this tool:

* [Declarative containers](./docs/discussions/declarative-containers.md)
* [Roadmap](./docs/discussions/roadmap.md)

## References

These comprehensive resources cover details that other texts will gloss over:

* [Command line](./docs/references/command-line.md)
* [Configuration](./docs/references/configuration.md)
