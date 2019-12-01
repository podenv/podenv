# Use an environment

This tutorial teachs you how to use an existing environment.

TODO...

```yaml
environments:
  webreview:
    parent: firefox-light
    args:
      - https://review.opendev.org
    home: ~/.config/firefox-review.opendev.org

  video-conf:
    parent: firefox
    packages:
      - chromium
    capabilities:
      webcam: True
      seccomp: False
    command:
      - chromium-browser
      - https://jitsi.org/
```


And the configuration can be included in a local `default.podenv` file such
as the one from this project:

```bash
$ cat ./default.podenv
description: 'Run podenv unittest'
parent: fedora
capabilities:
  mount-cwd: True
  uidmap: True
packages:
  - python3-mypy
  - python3-flake8
  - make
command:
  - make
$ podenv
```
