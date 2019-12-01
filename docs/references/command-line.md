# Command line interface

The podenv command line excepts an environment's `name` and optional `arguments`.

There are a few switches that results in different actions:

* `--show` dry run, show the enviroment definition and the resulting configuration.
* `--list` list the enviroments.

All the other arguments are override for the environment definition, for example,
to enable or disable network access use `--network` or `--no-network`.

## Usage

Here is the output of the `--help`:

```bash
usage: podenv [-h] [--verbose] [--config CONFIG] [--show] [--list] [--shell]
              [--net NET] [--home HOME] [-p PACKAGE] [-e ENVIRON] [-i IMAGE]
              [-b BASE] [-t TAG] [--manage-image] [--no-manage-image]
              [--branch-image] [--no-branch-image] [--root] [--no-root]
              [--privileged] [--no-privileged] [--terminal] [--no-terminal]
              [--ipc] [--no-ipc] [--x11] [--no-x11] [--pulseaudio]
              [--no-pulseaudio] [--git] [--no-git] [--editor] [--no-editor]
              [--ssh] [--no-ssh] [--gpg] [--no-gpg] [--webcam] [--no-webcam]
              [--alsa] [--no-alsa] [--dri] [--no-dri] [--kvm] [--no-kvm]
              [--tun] [--no-tun] [--seccomp] [--no-seccomp] [--selinux]
              [--no-selinux] [--setuid] [--no-setuid] [--ptrace] [--no-ptrace]
              [--network] [--no-network] [--foreground] [--no-foreground]
              [--mount-cwd] [--no-mount-cwd] [--mount-home] [--no-mount-home]
              [--mount-run] [--no-mount-run] [--mount-cache]
              [--no-mount-cache] [--auto-update] [--no-auto-update] [--uidmap]
              [--no-uidmap]
              [env] [args [args ...]]

podenv - a podman wrapper

positional arguments:
  env
  args

optional arguments:
  -h, --help            show this help message and exit
  --verbose
  --config CONFIG       The config path
  --show                Print the environment info and exit
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
