# Use an environment

This tutorial teachs you how to use an existing environment.

## List available environments

By default, podenv setup the environments defined in [podenv/hub](https://github.com/podenv/hub),
and you can get the list by using the `--list` argument:

```console
$ podenv --list
NAME                 DESCRIPTION
[...]
firefox              Mozilla Firefox
firefox-light        Mozilla Firefox (without dri)
[...]
```

## Get an environment details

You can get an environment details, such as it's Containerfile or the capabilities it needs
by using the `--show` argument. Let's compare firefox and firefox-light:

```console
$ podenv --show firefox
[+] Containerfile:
# Use registry.fedoraproject.org
FROM registry.fedoraproject.org/fedora:31
# Create user
RUN useradd -u 1000 -d /home/user -m user
# Install rpmfusion repository
RUN dnf install -y https://download1.rpmfusion.org/free/fedora/rpmfusion-free-release-31.noarch.rpm
# Install packages
RUN dnf install -y firefox gdouros-symbola-fonts mesa-dri-drivers libva-intel-driver libva-intel-hybrid-driver ffmpeg

[+] Capabilities:
dri large-shm network pulseaudio seccomp x11

[+] Command line:
podman run --security-opt label=disable -v /etc/machine-id:/etc/machine-id:ro -v /run/user/1000/pulse:/run/user/1000/pulse -v /tmp/.X11-unix:/tmp/.X11-unix --device /dev/dri -e DISPLAY=:0 -e PULSE_SERVER=/run/user/1000/pulse/native --user user --uidmap 1000:0:1 --uidmap 0:1:1000 --uidmap 1001:1001:64535 --shm-size=4g localhost/podenv/firefox firefox --no-remote

$ podenv --show firefox-light
[+] Containerfile:
# Use registry.fedoraproject.org
FROM registry.fedoraproject.org/fedora:31
# Create user
RUN useradd -u 1000 -d /home/user -m user
# Install packages
RUN dnf install -y firefox

[+] Capabilities:
large-shm network seccomp x11

[+] Command line:
podman run --security-opt label=disable -v /tmp/.X11-unix:/tmp/.X11-unix -e DISPLAY=:0 --user user --uidmap 1000:0:1 --uidmap 0:1:1000 --uidmap 1001:1001:64535 --shm-size=4g localhost/podenv/firefox-light firefox --no-remote
```

As you can see, firefox-light doesn't have the `dri` or `pulseaudio` capability.


## Start an environment

You can start an environment by giving it's name to the podenv argument:

```console
$ podenv firefox
[...]
[firefox window appears]
```

Podenv automatically built the container image if it wasn't already available:

```console
$ podman images
REPOSITORY                                  TAG                   IMAGE ID       CREATED          SIZE
localhost/podenv/firefox                    latest                713d6fa70a9f   46 seconds ago   556 MB
```

## Configure an environment

The firefox environment doesn't have access to the host files and when you close the window,
the browser configuration, its bookmarks and the history are lost. To prevent that, most
environment expect you to pick a storage location for their data. You can use the
`--home` command line argument to define a host directory as the environment home:

```console
$ podenv --home ~/.config/firefox-pod firefox
[firefox window appears]

$ ls -ap ~/.config/firefox-pod/
.cache/  .mozilla/
```

More generally, some settings can be configured from the command line, they
are called *environment execution arguments*, and you can get the list from
the `--help` output.


## Update an environment

The last thing you want to do is keep the environment updated, in particular
the one that have network access. To do that, podenv provides an in-place
update where the image is not rebuilt from scratch:

```console
$ podenv --update firefox
STEP 1: FROM localhost/podenv/firefox
STEP 2: RUN dnf update -y
[...]
Complete!
STEP 3: COMMIT podenv/firefox
Getting image source signatures
Copying blob ac0b803c5612 skipped: already exists
Copying blob 6100b00a5235 skipped: already exists
Copying blob 30e985d0b5bb done
Copying config 622df31ab3 done
Writing manifest to image destination
Storing signatures
622df31ab30b863514913557a6f0c3b2f8e287404a7792a9efc0506db56c8618
```

If the update broke your environment, you can rollback by restoring
the previous image. You can get it's id by running:

```console
$ podman images --all
REPOSITORY                                  TAG                   IMAGE ID       CREATED          SIZE
localhost/podenv/firefox                    latest                622df31ab30b   6 minutes ago    735 MB
<none>                                      <none>                713d6fa70a9f   21 minutes ago   556 MB

$ podman tag 713d6fa70a9f localhost/podenv/firefox
```

Finally, there is a limit to the number of update you can perform,
and after sometime it's better to re-create the image from scratch.
To do that, use the `--rebuild` argument to force a clean build:

```console
$ podenv --rebuild firefox
```

This conclude the tutorial on how to use an environment.
By this point you should understand how podenv manage the execution and how
update works.
