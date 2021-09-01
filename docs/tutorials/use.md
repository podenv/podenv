# Use an application

This tutorial teachs you how to use an existing application.

## List available applications

By default, podenv setup the applications defined in [podenv/hub](https://github.com/podenv/hub),
and you can get the list by using the `--list` argument:

```console
$ podenv --list
[...]
```

## Get an application details

You can get an application details by using the `--show` argument.

```console
$ podenv --show firefox
[...]
```

## Start an application

You can start an application by giving it's name to the podenv argument:

```console
$ podenv firefox
[firefox window appears]
```

Podenv automatically built the container image if it wasn't already available:

```console
$ podman images
REPOSITORY                                  TAG                   IMAGE ID       CREATED          SIZE
localhost/podenv/firefox                    latest                713d6fa70a9f   46 seconds ago   556 MB
```

## Configure an application

The firefox application doesn't have access to the host files and when you close the window,
the browser configuration, its bookmarks and the history are lost. To prevent that, most
application expect you to pick a storage location for their data. You can use the
`--home` command line argument to define a host directory as the application home:

```console
$ podenv --home ~/.config/firefox-pod firefox
[firefox window appears]

$ ls -ap ~/.config/firefox-pod/
.cache/  .mozilla/
```

More generally, some settings can be configured through the command line,
get the list from the `--help` output.

## Update an application

Podenv enables in-place container build using the `--update` argument:

```console
$ podenv --update firefox
STEP 1: FROM localhost/podenv/firefox
STEP 2: RUN dnf update -y
[...]
Complete!
STEP 3: COMMIT podenv/firefox
```

If the update is broken, you can rollback by restoring
the previous image. You can get it's id by running:

```console
$ podman images --all
REPOSITORY                                  TAG                   IMAGE ID       CREATED          SIZE
localhost/podenv/firefox                    latest                622df31ab30b   6 minutes ago    735 MB
<none>                                      <none>                713d6fa70a9f   21 minutes ago   556 MB

$ podman tag 713d6fa70a9f localhost/podenv/firefox
```

Finally you can force clean rebuild by cleaning up the `~/.cache/podenv` directory.

This conclude the tutorial on how to use an application.
By this point you should understand how podenv manage the runtime and how update works.
