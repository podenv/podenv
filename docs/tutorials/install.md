# Install and use podenv

This tutorial teachs you how to install and use podenv.

## Binary installation

Download the `podenv-x86_64-linux.tar.bz2` archive from the release page and put in your PATH:

```bash
curl -L https://github.com/podenv/podenv/releases/download/v0.6.0/podenv-x86_64-linux.tar.bz2 -o - | tar -xjvf - -C ~/.local/
```

Or install the Fedora package from [this Copr repo](https://copr.fedorainfracloud.org/coprs/petersen/podenv/).

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

> On first run, podenv setup the one defined in the [podenv/hub](https://github.com/podenv/podenv/tree/main/hub).

```ShellSession
$ podenv --list
```

… that can be executed by their name instead of `image:`.

Try running the application named `eglgears` to verify your installation is correct:

```bash
$ podenv eglgears
[spinning 3d gears appear]
```

This should display a mesa demo window. To do that, podenv built a container
image as described by the `eglgears` application, then it used the podman command
to run the image by giving it access to your graphical desktop.

Checkout the next tutorials to learn how to create a new application:
[Create an enviroment](./create.md).
