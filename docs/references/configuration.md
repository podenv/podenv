# Configuration references

This document references the configuration file format.

## Environment definition

The environment definition attributes are:

Name                 | Type            | Doc                                      |
-------------------- | --------------- | ---------------------------------------- |
name                 | str             | The name of the environment              |
description          | Optional[str]   | Environment description                  |
url                  | Optional[str]   | Application home page                    |
parent               | str             | A parent environment name to inherit attributes from |
abstract             | bool            | Set to True to indicate the environment can't be used directly |
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
sysctls              | List[str]       | List of sysctl(8)                        |
volumes              | Dict[str, Volume] | List of volumes                          |
mounts               | Dict[str, Optional[str]] | Extra mountpoints                        |
capabilities         | Dict[str, bool] | List of capabilities                     |
network              | str             | Name of a network to be shared by multiple environment |
add-hosts            | Dict[str, str]  | Custom hostname,ip to configure in the container |
requires             | StrOrList       | List of required environments            |
overlays             | List[Overlay]   | List of overlay to copy in runtime directory |
home                 | str             | Container home path mount                |
shmsize              | str             | The shm-size value string                |
ports                | List[str]       | List of port to expose on the host       |
system-type          | str             | Set image system type                    |
dns                  | str             | A custom DNS server                      |
container-file       | str             | Containerfile content                    |
image-customizations | List[str]       |                                          |


## Capabilities list

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
