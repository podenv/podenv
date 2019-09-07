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

Environments can be shared through the [hub](https://github.com/podenv/hub).

In a `~/.config/podenv/config.yaml`file, define desktop application:

```yaml
environments:
  browser:
    parent: firefox
    home: ~/.config/firefox

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

  webreview:
    parent: firefox-light
    args:
      - https://review.opendev.org
    home: ~/.config/firefox-review.opendev.org

  video-conf:
    parent: firefox
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
    parent: fedora
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
    parent: fedora
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
    parent: fedora
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
    network: corpvpn
    parent: firefox
```

And the configuration can be included in a local `default.podenv` file such
as the one from this project:

```bash
$ cat ./default.podenv
description: 'Run podenv unittest'
parent: fedora
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
url                  | Optional[str]   | Application home page                    |
parent               | str             | A parent environment name to inherit attributes from |
desktop              | Optional[DesktopEntry] | A desktop launcher entry file definition |
image                | str             | The container image reference            |
rootfs               | str             | The path of a rootfs                     |
packages             | List[str]       | List of packages to be installed in the image |
image-tasks          | List[Task]      | List of ansible like command to commit to the image |
pre-tasks            | List[Task]      | List of ansible like command to run before the command |
post-tasks           | List[Task]      | List of ansible like command to run after the pod exited |
command              | ExecArgs        | Container starting command               |
args                 | ExecArgs        | Optional arguments to append to the command |
environ              | Dict[str, str]  | User environ(7)                          |
vars                 | Dict[str, str]  | Extra environ vars to be used for command substitution only |
syscaps              | List[str]       | List of system capabilities(7)           |
mounts               | Dict[str, str]  | Extra mountpoints                        |
capabilities         | Dict[str, bool] | List of capabilities                     |
network              | str             | Name of a network to be shared by multiple environment |
requires             | StrOrList       | List of required environments            |
overlays             | List[Overlay]   | List of overlay to copy in runtime directory |
home                 | str             | Container home path mount                |
shmsize              | str             | The shm-size value string                |
ports                | List[str]       | List of port to expose on the host       |
system-type          | str             | Set image system type                    |
dns                  | str             | A custom DNS server                      |
image-customizations | List[str]       |                                          |


The available capabilities are:

Name                 | Doc                                                        |
-------------------- | ---------------------------------------------------------- |
manage-image         | manage the image with buildah                                |
branch-image         | branch the image for this environment                        |
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
alsa                 | share alsa device                                            |
dri                  | share graphic device                                         |
kvm                  | share kvm device                                             |
tun                  | share tun device                                             |
seccomp              | enable seccomp                                               |
selinux              | enable SELinux                                               |
setuid               | enable setuid                                                |
ptrace               | enable ptrace                                                |
network              | enable network                                               |
foreground           | work around application that goes into background            |
mount-cwd            | mount cwd to /data                                           |
mount-home           | mount home to host home                                      |
mount-run            | mount home and tmp to host tmpfs                             |
mount-cache          | mount image build cache                                      |
auto-update          | keep environment updated                                     |
uidmap               | map host uid                                                 |

## Install and usage

```bash
$ python3 -mpip install --user .
$ podenv --help
usage: podenv [-h] [--verbose] [--list] [--shell] [--net NET] [--home HOME]
              [-p PACKAGE] [-e ENVIRON] [-i IMAGE] [-b BASE] [-t TAG]
              [--manage-image] [--no-manage-image] [--branch-image]
              [--no-branch-image] [--root] [--no-root] [--privileged]
              [--no-privileged] [--terminal] [--no-terminal] [--ipc]
              [--no-ipc] [--x11] [--no-x11] [--pulseaudio] [--no-pulseaudio]
              [--git] [--no-git] [--editor] [--no-editor] [--ssh] [--no-ssh]
              [--gpg] [--no-gpg] [--webcam] [--no-webcam] [--alsa] [--no-alsa]
              [--dri] [--no-dri] [--kvm] [--no-kvm] [--tun] [--no-tun]
              [--seccomp] [--no-seccomp] [--selinux] [--no-selinux] [--setuid]
              [--no-setuid] [--ptrace] [--no-ptrace] [--network]
              [--no-network] [--foreground] [--no-foreground] [--mount-cwd]
              [--no-mount-cwd] [--mount-home] [--no-mount-home] [--mount-run]
              [--no-mount-run] [--mount-cache] [--no-mount-cache]
              [--auto-update] [--no-auto-update] [--uidmap] [--no-uidmap]
              [env] [args [args ...]]

podenv - a podman wrapper

positional arguments:
  env
  args

optional arguments:
  -h, --help            show this help message and exit
  --verbose
  --list                List available environments
  --shell               Run bash instead of the profile command
  --net NET             Set the network (host or env name)
  --home HOME           Set the home directory path
  -p PACKAGE, --package PACKAGE
                        Add a package to the environment
  -e ENVIRON, --environ ENVIRON
                        Set an environ variable
  -i IMAGE, --image IMAGE
                        Override the image name
  -b BASE, --base BASE  Override the base environment name
  -t TAG, --tag TAG     Set the image tag
  --manage-image        Enable capability: manage the image with buildah
  --no-manage-image     Disable manage-image capibility
  --branch-image        Enable capability: branch the image for this
                        environment
  --no-branch-image     Disable branch-image capibility
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
  --alsa                Enable capability: share alsa device
  --no-alsa             Disable alsa capibility
  --dri                 Enable capability: share graphic device
  --no-dri              Disable dri capibility
  --kvm                 Enable capability: share kvm device
  --no-kvm              Disable kvm capibility
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
  --foreground          Enable capability: work around application that goes
                        into background
  --no-foreground       Disable foreground capibility
  --mount-cwd           Enable capability: mount cwd to /data
  --no-mount-cwd        Disable mount-cwd capibility
  --mount-home          Enable capability: mount home to host home
  --no-mount-home       Disable mount-home capibility
  --mount-run           Enable capability: mount home and tmp to host tmpfs
  --no-mount-run        Disable mount-run capibility
  --mount-cache         Enable capability: mount image build cache
  --no-mount-cache      Disable mount-cache capibility
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
* Pre and post actions, possibly using ansible tasks.
* Automatic start and stop of required container.
