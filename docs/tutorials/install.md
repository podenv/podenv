# Install and use podenv

This tutorial teachs you how to install and use podenv.


## Requirements

To use podenv you need a Linux system with at least python-3.7 and [podman](https://podman.io).
Podenv may also uses PyYAML and [buildah](https://buildah.io).

For example, on a [fedora](https://getfedora.org) workstation, run this command to install the requirements:

```bash
sudo dnf install -y python3-pyyaml podman buildah
```

## Installation in PATH

Podenv is in early development without any release.
To install it you have to clone the project:

```bash
mkdir -p ~/git/github.com/podenv/podenv
git clone https://github.com/podenv/podenv ~/git/github.com/podenv/podenv
python3 -mpip install --user ~/git/github.com/podenv/podenv
```

You can verify the podenv command is successfully installed by running:

```bash
$ type -p podenv
/home/tristanc/.local/bin/podenv
```

## Developper installation

Podenv can also be executed directly from the checkout with this alias:

```bash
alias podenv='env PYTHONPATH=~/git/github.com/podenv/podenv python3 ~/git/github.com/podenv/podenv/podenv/main.py'
```


## Usage

Podenv is designed to be used from the command line or behind a desktop shortcut.

On the first invocation, podenv creates a default configuration in `~/.config/podenv`
and it will ask if you want to setup the default registry:

```bash
$ podenv --list
Press enter to sync registry https://github.com/podenv/hub [Yn]: ⏎
2019-12-01 19:05:40 INFO  podenv - Cloning https://github.com/podenv/hub
Cloning into '/home/tristanC/.config/podenv/registries/github-com:podenv:hub'...
[skip]
NAME                 PARENT          REGISTRY                DESCRIPTION
emacs                emacs-nox       github.com/podenv/hub   Extensible text editor
...
```

The `--list` argument just list the available environments.
To start an environment, just use its name as the argument.
Try using the environment named `xeyes` to verify your installation is correct:

```bash
$ podenv xeyes
[googly eyes appear]
```

This should display the `xeyes` window. To do that, podenv built a container
image as described by the `xeyes` environment, then it used the podman command
to run the image by giving it access to your graphical desktop.

Checkout the next tutorials to learn how to create a new environment:
[Create an enviroment](./create.md).
