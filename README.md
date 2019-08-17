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
      auto-update: True
      seccomp: True
      selinux: True
    environ:
      LC_ALL: en_US.UTF-8
      SHLVL: 3
      TERM: xterm

  shell:
    capabilities:
      terminal: True
      mount-run: True
    overlays:
      - .bashrc: |
          if [ -f /etc/bashrc ]; then
            . /etc/bashrc
          fi
          export PS1="\[\033[01;32m\]\h \[\033[01;34m\]\w \$ \[\033[00m\]"
          alias ls='ls -ap --color=auto'
    command:
      - /bin/bash
    imageCustomizations:
      - sed -e 's/nodocs//' -i /etc/dnf/dnf.conf

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

  video-conf:
    parent: firefox-webgl
    packages:
      - chromium
    capabilities:
      webcam: True
      seccomp: False
    command:
      - chromium-browser
      - https://jitsi.org/
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
      mount-cwd: True
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
  mount-cwd: True
  uidmap: True
packages:
  - python3-mypy
  - python3-flake8
  - make
command:
  - make
$ podenv
```

## Environment definition

The environment definition attributes are:

Name                 | Type            | Doc                                      |
-------------------- | --------------- | ---------------------------------------- |
name                 | str             | The name of the environment              |
description          | Optional[str]   | Environment description                  |
parent               | str             | A parent environment name to inherit attributes from. |
desktop              | Optional[DesktopEntry] | A desktop launcher entry file definition |
image                | str             | The container image reference            |
rootfs               | str             | The path of a rootfs                     |
dns                  | str             | A custom DNS server                      |
imageCustomizations  | List[str]       | List of shell commands to execute and commit in the image |
packages             | List[str]       | List of packages to be installed in the image |
command              | ExecArgs        | Container starting command               |
args                 | ExecArgs        | Optional arguments to append to the command |
environ              | Dict[str, str]  | User environ(7)                          |
syscaps              | List[str]       | List of system capabilities(7)           |
mounts               | Dict[str, str]  | Extra mountpoints                        |
capabilities         | Dict[str, bool] | List of capabilities                     |
provides             | Dict[str, str]  | List of objects the environment provides |
requires             | Dict[str, str]  | List of objects the environment requires |
overlays             | List[Overlay]   | List of overlay to copy in runtime directory |
home                 | str             | Container home path mount                |
shmsize              | str             | The shm-size value string                |
ports                | List[str]       | List of port to expose on the host       |


The available capabilities are:

Name                 | Doc                                                        |
-------------------- | ---------------------------------------------------------- |
root                 | run as root                                                  |
privileged           | run as privileged container                                  |
terminal             | interactive mode                                             |
ipc                  | share host ipc                                               |
x11                  | share x11 socket                                             |
pulseaudio           | share pulseaudio socket                                      |
git                  | share .gitconfig and excludesfile                            |
editor               | setup editor env                                             |
ssh                  | share ssh agent and keys                                     |
gpg                  | share gpg agent                                              |
webcam               | share webcam device                                          |
dri                  | share graphic device                                         |
tun                  | share tun device                                             |
seccomp              | enable seccomp                                               |
selinux              | enable SELinux                                               |
setuid               | enable setuid                                                |
ptrace               | enable ptrace                                                |
network              | enable network                                               |
mount-cwd            | mount cwd to /data                                           |
mount-run            | mount home and tmp to host tmpfs                             |
auto-update          | keep environment updated                                     |
uidmap               | map host uid                                                 |

## Install and usage

```bash
$ python3 -mpip install --user .
$ podenv --help
usage: podenv [-h] [--verbose] [--shell] [-p PACKAGE] [--root] [--no-root]
              [--privileged] [--no-privileged] [--terminal] [--no-terminal]
              [--ipc] [--no-ipc] [--x11] [--no-x11] [--pulseaudio]
              [--no-pulseaudio] [--git] [--no-git] [--editor] [--no-editor]
              [--ssh] [--no-ssh] [--gpg] [--no-gpg] [--webcam] [--no-webcam]
              [--dri] [--no-dri] [--tun] [--no-tun] [--seccomp] [--no-seccomp]
              [--selinux] [--no-selinux] [--setuid] [--no-setuid] [--ptrace]
              [--no-ptrace] [--network] [--no-network] [--mount-cwd]
              [--no-mount-cwd] [--mount-run] [--no-mount-run] [--auto-update]
              [--no-auto-update] [--uidmap] [--no-uidmap]
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
  --no-root             Disable root capibility
  --privileged          Enable capability: run as privileged container
  --no-privileged       Disable privileged capibility
  --terminal            Enable capability: interactive mode
  --no-terminal         Disable terminal capibility
  --ipc                 Enable capability: share host ipc
  --no-ipc              Disable ipc capibility
  --x11                 Enable capability: share x11 socket
  --no-x11              Disable x11 capibility
  --pulseaudio          Enable capability: share pulseaudio socket
  --no-pulseaudio       Disable pulseaudio capibility
  --git                 Enable capability: share .gitconfig and excludesfile
  --no-git              Disable git capibility
  --editor              Enable capability: setup editor env
  --no-editor           Disable editor capibility
  --ssh                 Enable capability: share ssh agent and keys
  --no-ssh              Disable ssh capibility
  --gpg                 Enable capability: share gpg agent
  --no-gpg              Disable gpg capibility
  --webcam              Enable capability: share webcam device
  --no-webcam           Disable webcam capibility
  --dri                 Enable capability: share graphic device
  --no-dri              Disable dri capibility
  --tun                 Enable capability: share tun device
  --no-tun              Disable tun capibility
  --seccomp             Enable capability: enable seccomp
  --no-seccomp          Disable seccomp capibility
  --selinux             Enable capability: enable SELinux
  --no-selinux          Disable selinux capibility
  --setuid              Enable capability: enable setuid
  --no-setuid           Disable setuid capibility
  --ptrace              Enable capability: enable ptrace
  --no-ptrace           Disable ptrace capibility
  --network             Enable capability: enable network
  --no-network          Disable network capibility
  --mount-cwd           Enable capability: mount cwd to /data
  --no-mount-cwd        Disable mount-cwd capibility
  --mount-run           Enable capability: mount home and tmp to host tmpfs
  --no-mount-run        Disable mount-run capibility
  --auto-update         Enable capability: keep environment updated
  --no-auto-update      Disable auto-update capibility
  --uidmap              Enable capability: map host uid
  --no-uidmap           Disable uidmap capibility
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

Future goals include:

* Automatic squash of old layers: (depends on https://github.com/containers/buildah/issues/1778)
* Proper documentation and integration tests.
* Cleanup runDir.
* Support packages from pypi, hackage, ...
* Expose image tag version to support rollback.
* Pre and post actions, possibly using ansible tasks.
* Automatic start and stop of required container.
