# podenv: a podman wrapper

Podenv provides a declarative interface to manage containers' environment with
[podman](https://podman.io/).


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


## Examples

In a `~/.config/podenv/config.yaml`file, define desktop application:

```yaml
environments:
  base:
    image: registry.fedoraproject.org/fedora:30
    capabilities:
      autoUpdate: True
      seccomp: True
      selinux: True
    environ:
      LC_ALL: en_US.UTF-8
      SHLVL: 3
      TERM: xterm
    imageCustomizations:
      - sed -e 's/nodocs//' -i /etc/dnf/dnf.conf

  shell:
    capabilities:
      terminal: True
    overlays:
      - bash
    command:
      - /bin/bash

  ansible:
    parent: shell
    capabilities:
      network: True
      uidmap: True
    packages:
      - ansible
      - python3-openstackclient
    mounts:
      ~/.config/openstack: ~/.config/openstack

  firefox:
    imageCustomizations:
      - dnf install -y https://download1.rpmfusion.org/free/fedora/rpmfusion-free-release-$(grep ^VERSION_ID= /etc/os-release | cut -d= -f2).noarch.rpm
    packages:
      - firefox
      - ffmpeg
    capabilities:
      ipc: True
      pulseaudio: True
      x11: True
      network: True
    command:
      - firefox
      - --no-remote
    shmsize: 4g

  firefox-webgl:
    parent: firefox
    packages:
      - libvdpau-va-gl
      - mesa-dri-drivers
      - libva-intel-driver
      - libva-intel-hybrid-driver
    capabilities:
      dri: True
      network: True

  webreview:
    parent: firefox
    args:
      - https://review.opendev.org
    home: ~/.cache/firefox-review.opendev.org

  bluejeans:
    parent: firefox-webgl
    packages:
      - chromium
    capabilities:
      webcam: True
      seccomp: False
    command:
      - chromium-browser
      - https://redhat.bluejeans.com
```


Command may contains `$@` to include command line arguments, or `$1` to
mount a file argument inside the container, for examples:

```yaml
  # podenv youtube-dl $url  # download file to local directory
  youtube-dl:
    packages:
      - youtube-dl
    capabilities:
      network: True
      mountCwd: True
    command:
      - youtube-dl
      - $@

  # podenv pdf $filepath  # open local file
  pdf:
    packages:
      - mupdf
    capabilities:
      x11: True
    command:
      - mudpf
      - $1
```

Network can be shared:

```yaml
  corpvpn:
    provides:
      network: corpvpn
    packages: ["openvpn"]
    capabilities:
      network: True
      root: True
      tun: True
      seccomp: False
      uidmap: True
    syscaps: ["NET_ADMIN", "SYS_RESOURCE", "SETGID", "SETUID"]
    home: ~/.config/corpvpn/
    command:
      - openvpn
      - --config
      - corp.config

  corpbrowser:
    requires:
      network: corpvpn
    parent: firefox
```

And the configuration can be included in a local `default.podenv` file such
as the one from this project:

```bash
$ cat ./default.podenv
capabilities:
  mountCwd: True
packages:
  - python3-mypy
  - python3-flake8
command:
  - make
$ podenv
```


## Install and usage

```
$ python3 -mpip install --user .
$ podenv --help
usage: podenv [-h] [--verbose] [--shell] [-p PACKAGE] [--root] [--no-root]
              [--privileged] [--no-privileged] [--terminal] [--no-terminal]
              [--ipc] [--no-ipc] [--x11] [--no-x11] [--pulseaudio]
              [--no-pulseaudio] [--ssh] [--no-ssh] [--webcam] [--no-webcam]
              [--dri] [--no-dri] [--tun] [--no-tun] [--seccomp] [--no-seccomp]
              [--ptrace] [--no-ptrace] [--network] [--no-network] [--mountCwd]
              [--no-mountCwd] [--mountRun] [--no-mountRun] [--autoUpdate]
              [--no-autoUpdate] [--uidmap] [--no-uidmap]
              [env] [args [args ...]]

podenv - a podman wrapper

positional arguments:
  env
  args

optional arguments:
  -h, --help            show this help message and exit
  --verbose
  --shell               Run bash instead of the profile command
  -p PACKAGE, --package PACKAGE
                        Add a package to the environment
  --root                Enable capability: run as root
  --no-root             Disable root capability
  --privileged          Enable capability: run as privileged container
  --no-privileged       Disable privileged capability
  --terminal            Enable capability: interactive mode
  --no-terminal         Disable terminal capability
  --ipc                 Enable capability: share host ipc
  --no-ipc              Disable ipc capability
  --x11                 Enable capability: share x11 socket
  --no-x11              Disable x11 capability
  --pulseaudio          Enable capability: share pulseaudio socket
  --no-pulseaudio       Disable pulseaudio capability
  --ssh                 Enable capability: share ssh agent and keys
  --no-ssh              Disable ssh capability
  --webcam              Enable capability: share webcam device
  --no-webcam           Disable webcam capability
  --dri                 Enable capability: share graphic device
  --no-dri              Disable dri capability
  --tun                 Enable capability: share tun device
  --no-tun              Disable tun capability
  --seccomp             Enable capability: enable seccomp
  --no-seccomp          Disable seccomp capability
  --ptrace              Enable capability: enable ptrace
  --no-ptrace           Disable ptrace capability
  --network             Enable capability: enable network
  --no-network          Disable network capability
  --mountCwd            Enable capability: mount cwd to /data
  --no-mountCwd         Disable mountCwd capability
  --mountRun            Enable capability: mount home and tmp to host tmpfs
  --no-mountRun         Disable mountRun capability
  --autoUpdate          Enable capability: keep environment updated
  --no-autoUpdate       Disable autoUpdate capability
  --uidmap              Enable capability: map host uid
  --no-uidmap           Disable uidmap capability
```


## Design

Podenv mainly generates podman command line arguments.
When necessary, podenv may also:

* build and update image with buildah.
* create infra pod to persist namespaces.
* create and set runtime directory labels.

The `podenv.env` module contains the main capability logic,
while the `podenv.pod` module does all the IO.


## Roadmap

Here are some features missing for completness:

* Automatic squash of old layers.
* Proper documentation and integration tests.
* Cleanup runDir.
* Support packages from pypi, hackage, ...
* Support rootfs.
* Expose image tag version to support rollback.
* Post actions, for example to backup container volumes.
* Automatic start of required container, for example to start pulseaudio on demand.
