# Create an environment

This tutorial teachs you how to create a new environment to
start the [GIMP - GNU Image Manipulation Program](https://gimp.org) program.

TODO...

Command may contains `$1` to mount a file argument inside the container,
for examples:

```yaml
  # podenv gimp $filepath  # open local file
  pdf:
    parent: fedora
    packages:
      - gimp
    capabilities:
      x11: True
    command:
      - gimp
      - $1
```
