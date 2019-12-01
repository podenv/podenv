# Declarative containers

## Podenv design

Podenv mainly generates podman command line arguments.
When necessary, podenv may also:

* build and update image with buildah.
* create infra pod to persist namespaces.
* create and set runtime directory labels.

The `podenv.env` module contains the main capability logic,
while the `podenv.pod` module does all the IO.
