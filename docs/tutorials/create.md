# Create an application

This tutorial teachs you how to create a new application for
the [GIMP - GNU Image Manipulation Program](https://gimp.org) application.


## Create the gimp application definition

We don't want to define the application each time we use it, thus we record
it's configuration in a file, for example at *~/.config/podenv/gimp.dhall*
write this content:

```dhall
(env:PODENV).Application::{
, name = "gimp"
, runtime =
    (env:PODENV).Containerfile
      ''
      FROM registry.fedoraproject.org/fedora
      RUN dnf install -y gimp
      ''
}
```

Validate the configuration is in place by running this command:

```ShellSession
$ podenv --config ~/.config/podenv/gimp.dhall --list
```

We don't want to set the configuration file path each time, thus we define
a shell alias:

```ShellSession
$ alias podenv-gimp="podenv --config ~/.config/podenv/gimp.dhall"
```

To execute this application, you run this command:

```ShellSession
$ podenv-gimp
[building container]
STEP 1: FROM registry.fedoraproject.org/fedora
STEP 2: RUN dnf install -y gimp
```

However, the application didn't start because the podenv default configuration
doesn't give access to the terminal.

Get the actual podman command by using the `--show` argument:

```ShellSession
$ podenv-gimp --show
Raw command: podman run --rm --network none --name gimp localhost/gimp
```

As you can see, podenv also disables network access by default (`--network none`).
Next we see how to add capabilities.


## Start a shell with the terminal capability

It is often useful to get a shell terminal in a container. We can add the `terminal` capability
by using the `--terminal` command line argument:

```console
$ podenv-gimp --terminal
bash-5.0# exit
```

Using the `terminal` capability to start a shell is useful to debug a container.
And since the image may define a non-shell command,
it is better to use the `--shell` command line argument to also force the command.
Let's use it to start gimp:

```console
$ podenv-gimp --shell
base-5.0# gimp
Cannot open display:
```

The gimp command failed because the application doesn't have access to the
window system. While we could fix that by using the `--x11` capability on the
command line, we'll see how to properly configure the gimp application next.


## Configure the application

We would like to be able to use the gimp application without special command line argument.
Since the gimp application will always need the `x11` capability, you update the
configuration file *~/.config/podenv/gimp.dhall* to:

```dhall
(env:PODENV).Application::{
, name = "gimp"
, runtime =
    (env:PODENV).Containerfile
      ''
      FROM registry.fedoraproject.org/fedora
      RUN dnf install -y gimp
      ''
, capabilities = (env:PODENV).Capabilities::{ x11 = True }
, command = [ "gimp" ]
}
```

> Note: when gimp version 3 is available, x11 can be replaced by wayland.

We added the `x11` capability and defined the `command` attribute so that
gimp starts by default instead of the container entrypoint.

Check out the resulting podman configuration:

```ShellSession
$ podenv-gimp --show
Raw command: podman run --rm --security-opt label=disable --network none --env DISPLAY=:3 --mount type=tmpfs,destination=/dev/shm --volume /tmp/.X11-unix:/tmp/.X11-unix --name gimp localhost/gimp gimp
```

Now you can use the application by running this command:

```console
$ podenv-gimp
[gimp window appears]
```

However, the application doesn't have access to the host filesystem.
Any file modifications are trashed when the application die.


## Provide access to the host files

We would like our application to be able to access and modify files from the host
filesystem. The `--hostfile` capability takes care of exposing host path
command line argument to the container.

You can access a file using:

```ShellSession
$ podenv-gimp --hostfile /usr/share/pixmaps/fedora-logo.png
[fedora logo appears in gimp]
```

## Add the application to the global configuration

We would like to be able to run the new application directly using `podenv`.
You can add it to the global config by adding `// { gimp = ./gimp.dhall }` to
the *~/.config.podenv/config.dhall* file, for example:

```dhall
(env:PODENV).Hub // { gimp = ./gimp.dhall }
```

… then you can now run:

```ShellSession
$ podenv gimp --show
```

This concludes the tutorial on how to create a new application.
By this point you should understand how podenv can be configured and how
capabilities work.
