# Install and use podenv

This tutorial teachs you how to install and use podenv.

## Binary installation

Download the `x86_64-podenv` binary from the release page and put in your PATH.

You can verify the podenv command is successfully installed by running:

```ShellSession
$ podenv --help
```

> Checkout the [contribute](../howtos/contribute.md) documentation for source install instructions.

## Usage

At its simplest form, a podenv application is defined by its image name.
Such application can be loaded directly from the command line:

```ShellSession
$ podenv --shell image:ubi8
```

Podenv can use pre-configured applications.

> On first run, podenv setup the one defined in the [podenv/hub](https://github.com/podenv/hub).

```ShellSession
$ podenv --list
```

… that can be executed by their name instead of `image:`.

Try running the application named `xeyes` to verify your installation is correct:

```bash
$ podenv xeyes
[googly eyes appear]
```

This should display the `xeyes` window. To do that, podenv built a container
image as described by the `xeyes` application, then it used the podman command
to run the image by giving it access to your graphical desktop.

Checkout the next tutorials to learn how to create a new application:
[Create an enviroment](./create.md).
