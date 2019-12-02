# Create an environment

This tutorial teachs you how to create a new environment for
the [GIMP - GNU Image Manipulation Program](https://gimp.org) application.

## Create the gimp environment definition

You need to create a configuration file at the default location: **~/.config/podenv/config.dhall**
with this content:

```dhall
{ environments =
    [ { name = "gimp"
      , description = Some "GNU Image Manipulation Program"
      , container-file =
          ''
          FROM registry.fedoraproject.org/fedora
          RUN dnf install -y gimp
          ''
      }
    ]
}
```

And you can validate the configuration is in place by running this command:

```console
$ podenv --list
NAME   DESCRIPTION
gimp   GNU Image Manipulation Program
```

To execute this environment, you run this command:

```console
$ podenv gimp
STEP 1: FROM registry.fedoraproject.org/fedora
[...]
Complete!
STEP 3: COMMIT podenv/gimp
Getting image source signatures
Writing manifest to image destination
Error: chdir: No such file or directory: OCI runtime command not found error
```

Podenv successfully built the image:

```console
$ podman images
REPOSITORY                          TAG                   IMAGE ID       CREATED          SIZE
localhost/podenv/gimp               latest                3ec91e4b571c   11 minutes ago   722 MB
```

However, the environment didn't start because podenv default configuration
requires an unprivileged user to exist in the image.

You can get the podman command line by using the **--show** or **--verbose**
argument:

```console
$ podenv --show gimp
Environment:
Env(name='gimp', description='GNU Image Manipulation Program', ...)
Command line:
podman run --hostname gimp --network none --workdir /home/user -e HOME=/home/user -e XDG_RUNTIME_DIR=/run/user/1000 --user 1000 gimp
```

As you can see, podenv disable network access by default (`--network none`) and
starts the container with an unprivileged user (`--workdir /home/user --user 1000`).
However, the environment we defined doesn't have an unprivileged user.


## Running the environment as root

You need to activate the `root` capability to run the image using the default (container) root user:

```console
$ podenv --verbose --root gimp
Running podman run --rm --name gimp --hostname gimp --network none --workdir /root -e HOME=/root -e XDG_RUNTIME_DIR=/run/user/0 podenv/gimp
Complete
```

This time the command succeeded but the environment wasn't interactive and the container exited.
You need to use the **--shell** argument to get an interactive access:

```console
$ podenv --root gimp --shell
[root@gimp ~]# gimp
Cannot open display:
```

As you can see, podenv disable host access by default. You need to use the `x11` capability
for the gimp command.


## Configure the environment capabilities

We would like to be able to use the gimp environment without special command line argument.
Since the gimp environment will always need the `x11` capability, you update the
configuration file **~/.config/podenv/config.dhall** to:

```dhall
{ environments =
    [ { name = "gimp"
      , description = Some "GNU Image Manipulation Program"
      , capabilities = { x11 = True, root = True }
      , command = [ "gimp" ]
      , container-file =
          ''
          FROM registry.fedoraproject.org/fedora
          RUN dnf install -y gimp
          ''
      }
    ]
}
```

This change also added the `command` attribute so that gimp starts by default instead
of the container entrypoint.

Now you can use the environment by running this command:

```console
$ podenv gimp
[gimp window appears]
```

However, the environment doesn't have access to any files.
Any files modification will be trashed when the environment die.


## Access files from the environments

We would like our environment to be able to access and modify the current working
directory content.

You can start the environment with the `--mount-cwd` capability to expose the host
cwd to the environment **/data** directory.

You can also updates the command attribute to support automatic file argument to
be available in the environment.

With this gimp environment definition:

```dhall
{- ~/.config/podenv/config.dhall -}
{ environments =
    [ { name = "gimp"
      , description = Some "GNU Image Manipulation Program"
      , capabilities = { x11 = True, root = True, mount-cwd = True }
      , command = [ "gimp", "\$1" ]
      , container-file =
          ''
          FROM registry.fedoraproject.org/fedora
          RUN dnf install -y gimp
          ''
      }
    ]
}
```

You can now start podenv like so:

```console
$ cd ~/Pictures
$ podenv gimp
[gimp starts with /data being ~/Pictures]
$ gimp ~/git/github.com/podenv/docs/logo.svg
[gimp starts with the logo.svg file arg being mounted in the environment]
```

This concludes the tutorial on how to create a new environment.
By this point you should understand how podenv can be configured and how
capabilities work.


> Please note that is still a work in progress and the above examples may become easier.
> In particular the workdir may defaults to '/' to prevent the first error,
> Then host files access may be refactored to auto-detect path arguments.
