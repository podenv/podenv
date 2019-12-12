# Create an environment

This tutorial teachs you how to create a new environment for
the [GIMP - GNU Image Manipulation Program](https://gimp.org) application.

## Create the gimp environment definition

We don't want to define the environment each time we use it, thus we record
it's configuration to a file, for example at **~/.config/podenv/gimp.yaml**
with this content:

```yaml
name: gimp
description: "GNU Image Manipulation Program"
container-file: |
  FROM registry.fedoraproject.org/fedora
  RUN dnf install -y gimp
```

And you can validate the configuration is in place by running this command:

```console
$ podenv --config ~/.config/podenv/gimp.yaml --list
NAME   DESCRIPTION
gimp   GNU Image Manipulation Program
```

We don't want to set the configuration file path each time, thus we define
an environment variable:

```console
$ export PODENV_CONFIG=~/.config/podenv/gimp.yaml
$ podenv --list
NAME   DESCRIPTION
gimp   GNU Image Manipulation Program
```

To execute this environment, you run this command:

```console
$ podenv gimp
[+] Building localhost/podenv/gimp with ~.cache/podenv/containerfiles/Containerfile.podenv-gimp.tmp
    because: ~/.cache/podenv/containerfiles/Containerfile.podenv-gimp doesn't exists
STEP 1: FROM registry.fedoraproject.org/fedora
STEP 2: RUN dnf install -y gimp
[...]
Complete!
STEP 3: COMMIT podenv/gimp
Getting image source signatures
Writing manifest to image destination
```

Podenv successfully built the image:

```console
$ podman images
REPOSITORY                          TAG                   IMAGE ID       CREATED          SIZE
localhost/podenv/gimp               latest                3ec91e4b571c   11 minutes ago   722 MB
```

However, the environment didn't start because podenv default configuration
doesn't give access to the terminal.

You can get the actual podman command by using the **--show** argument:

```console
$ podenv --show gimp
[+] Containerfile:
FROM registry.fedoraproject.org/fedora
RUN dnf install -y gimp

[+] Command line:
podman run --hostname gimp --network none --user 1000 localhost/podenv/gimp
```

As you can see, podenv also disable network access by default (`--network none`) and
starts the container with an unprivileged user (`--user 1000`).
Next we see how to add capabilities.


## Activate the terminal capability

It is often useful to get a shell terminal in a container. We can add the `terminal` capability
by using the `--terminal` command line argument:

```console
$ podenv --terminal gimp
bash-5.0$ exit
```

Using the `terminal` capability to start a shell is useful to debug a container.
And since the image may define a non-shell command,
it is better to use the `--shell` command line argument to also force the command.
Let's use it to start gimp:

```console
$ podenv --shell gimp
base-5.0$ gimp
Cannot open display:
base-5.0$ exit
```

The gimp command failed because the environment doesn't have access to the
window system. While we could fix that by using the `--x11` capability on the
command line, we'll see how to properly configure the gimp environment next.


## Configure the environment capabilities

We would like to be able to use the gimp environment without special command line argument.
Since the gimp environment will always need the `x11` capability, you update the
configuration file **~/.config/podenv/gimp.yaml** to:

```yaml
name: gimp
description: "GNU Image Manipulation Program"
container-file: |
  FROM registry.fedoraproject.org/fedora
  RUN dnf install -y gimp
command: gimp
capabilities:
  x11: true
environ:
  HOME: /tmp
```

We added the `x11` capability and defined a *HOME* environment variable because the image
doesn't have unprivileged user.

We also added the `command` attribute so that gimp starts by default instead
of the container entrypoint.

Now you can use the environment by running this command:

```console
$ podenv gimp
[gimp window appears]
```

However, the environment doesn't have access to the host filesystem.
Any file modification are trashed when the environment die.


## Access files from the environments

We would like our environment to be able to access and modify files from the host
filesystem. The `--hostfiles` capability takes care of exposing host path
command line argument to the container.

You can open a single file using:

```console
$ podenv --hostfiles gimp /usr/share/pixmaps/fedora-logo.png
[fedora logo appears in gimp]
```

You can access the host local directory in the container */data/* directory using:

```console
$ podenv --hostfiles gimp .
```


This concludes the tutorial on how to create a new environment.
By this point you should understand how podenv can be configured and how
capabilities work.
