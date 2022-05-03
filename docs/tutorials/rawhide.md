# Work with rawhide

This tutorial teachs you how to install distro packages inside a bubblewrap container with Podenv.
If you don't have the `podenv` command already installed, read the [Install and use podenv tutorial](./install.md) first.

Run a shell with podman:

```ShellSession
$ podenv --rw --network --root --shell fedora.rawhide
```

We can avoid polluting the container storage by extracing the image to a volume:

```ShellSession
$ podenv --volume rawhide:/mnt image:fedora:rawhide bash -c "tar --one-file-system -cf - / | tar -C /mnt -xf -"
```

Run a shell with bubblewrap:

```ShellSession
$ podenv  --rw --network --root rootfs:rawhide
```

That way, installing packages persist between executions.
