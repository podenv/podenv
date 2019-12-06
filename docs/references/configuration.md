# Configuration references

This document references the configuration file format.

## Environment definition

The environment definition attributes are:

Name                 | Type            | Doc                                      |
-------------------- | --------------- | ---------------------------------------- |
name                 | str             | The name of the environment              |
description          | Optional[str]   | Environment description                  |
url                  | Optional[str]   | Application home page                    |
capabilities         | Dict[str, bool] | List of capabilities                     |
image                | str             | The container image reference            |
container-file       | Optional[Union[str, List[Task]]] | Containerfile content                    |
container-update     | Optional[Union[str, List[Task]]] | Containerfile update content             |
packages             | Optional[List[str]] | List of required packages                |
command              | Optional[ExecArgs] | Container starting command               |
pre-tasks            | Optional[List[Task]] | List of ansible like command to run before the command |
post-tasks           | Optional[List[Task]] | List of ansible like command to run after the pod exited |
work-dir             | Optional[Path]  | The container workdir                    |
environ              | Optional[Dict[str, str]] | User environ(7)                          |
syscaps              | Optional[List[str]] | List of system capabilities(7)           |
sysctls              | Optional[List[str]] | List of sysctl(8)                        |
volumes              | Optional[Volumes] | List of volumes                          |
mounts               | Optional[Mounts] | Extra mountpoints                        |
network              | Optional[str]   | Name of a shared network                 |
add-hosts            | Optional[Dict[str, str]] | Custom hostname,ip to configure in the container |
ports                | Optional[List[str]] | List of port to expose on the host       |
dns                  | Optional[str]   | A custom DNS server                      |
home                 | Optional[str]   | Container home path mount                |
desktop              | Optional[DesktopEntry] | A desktop launcher entry file definition |


## Capabilities list

The available capabilities are:

Name                 | Doc                                                        |
-------------------- | ---------------------------------------------------------- |
root                 | run as root                                                  |
privileged           | run as privileged container                                  |
terminal             | interactive mode                                             |
hostfiles            | enable host files access                                     |
large-shm            | mount a 4gb shm                                              |
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
uidmap               | map host uid                                                 |
