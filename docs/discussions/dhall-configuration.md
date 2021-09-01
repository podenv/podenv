# Using dhall for configuration?

Using dhall for podenv configuration might sound a bit hipster,
however, it is effective for the following reasons:

## Programmable configuration language

Dhall enables an efficient configuration schema that can be programmed
to reduce podenv runtime complexicity.
Here is a comparaison between the legacy format and the new dhall configuration:

### Legacy format

The legacy configuration looked like that:

```yaml
environments:
  - name: fedora
    image: registry.fedoraproject.org/fedora:30

  - name: firefox-light
    parent: fedora
    packages:
      - firefox
    capabilities:
      x11: true

  - name: firefox
    parent: firefox-light
    image-tasks:
      - name: "Install rpmfusion"
        shell: dnf install -y https://download1.rpmfusion.org/free/...
    packages:
      - ffmpeg
      - mesa-dri-drivers
    capabilities:
      dri: true
```

When loading the `firefox` environment, podenv resolved parent inheritance and
created this intermediary configuration:

```yaml
name: firefox
image: registry.fedoraproject.org/fedora:30
packages:
  - firefox
  - ffmpeg
  - mesa-dri-drivers
capabilities:
  x11: true
  dri: true
image-tasks:
  - name: "Install rpmfusion"
    shell: ...
```

Then podenv detected the image package manager to be able to
generate a `firefox` container image using this recipe:

```Dockerfile
FROM registry.fedoraproject.org/fedora:30
RUN dnf install -y https://download1.rpmfusion.org/free/...
RUN dnf install -y firefox ffmpeg mesa-dri-drivers
```

While this configuration format is easy to use, it requires a lot of
work from the podenv runtime:

* schema validations to detect incorrect capabilities or attributes.
* parent inheritance, e.g. when to extend or override attributes?
* knowledge of package manager, e.g. podenv supported dnf, apt, pip and guix package.

One of the goals was to be able to share multiple environments' packages
inside a single image in order to benefit from shared libraries and
reduced memory usage.


### Dhall schema

We can now explicitely define how images are built, while being
able to combine multiple environment when needed.

Using dhall, we can implement complex logic in the configuration layer,
simplifying the runtime.


## Imports

Dhall makes podenv's previous registry system obsolete
as dhall can natively import configurations.

## Safety

Dhall implements a powerful type system that prevents breaking things.
For example, this expression fails the dhall evaluation:

```bash
$ dhall <<< '[{ name = "firefox"}, {nam = "mumble"}]'
Error: List elements should all have the same type

{ - name : …
, + nam : …
}
```

## Conclusion

Dhall renders most of the legacy podenv logic obsolete while enabling
functional configuration.
