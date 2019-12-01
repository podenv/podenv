# Using dhall for configuration?

> Note that this is a work in progress and the legacy
> format is still implemented.

Using dhall for podenv configuration might sound a bit hipster,
however, it is effective for the following reasons:

## Programmable configuration language

Dhall enables an efficient configuration schema that can be programmed
to reduce podenv runtime complexicity.
Here is a comparaison between the legacy format and dhall configuration:

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
    parent: firefox
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

And it updated the image using:

```Dockerfile
FROM podenv/firefox
RUN dnf update -y
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

The equivalent dhall configuration looks like this:

```dhall
{ environments =
    [ { name = "firefox"
      , container-file =
          ''
          FROM registry.fedoraproject.org/fedora:30
          RUN dnf install -y https://download1.rpmfusion.org/free/...
          RUN dnf install -y firefox ffmpeg mesa-dri-drivers
          ''
      , container-update = Some 'RUN dnf update -y'
      , capabilities = { x11 = True, dri = True }
      }
    ]
}
```

The syntax looks odd because it is a new language;
it seems like podenv just lost the ability to share an image with
multiple environments. However, as you can see below, this isn't the case.

Dhall, a [programmable configuration language](https://docs.dhall-lang.org/discussions/Programmable-configuration-files.html),
can implement functions:

```dhall
let fedoraImage =
          \(packages : List Text)
      ->  let concat = https://prelude.dhall-lang.org/Text/concatSep

          in  { container-file =
                  ''
                  FROM registry.fedoraproject.org/fedora:30
                  RUN dnf install -y https://dowload1.rpmfusion.org/free/...
                  RUN dnf install -y ${concat " " packages}
                  ''
              , container-update = Some "RUN dnf update -y"
              }

in  { environments =
        [     fedoraImage [ "firefox", "ffmpeg", "mesa-dri-drivers" ]
          //  { name = "firefox", capabilities = { x11 = True, dri = True } }
        ]
    }
```

We can now explicitely define how images are built, while being
able to combine multiple environment when needed.

Using dhall, we can implement complex logic in the configuration layer,
simplifying the runtime.

## Imports

Dhall makes podenv's previous registry system obsolete
as dhall can natively import configurations.

The user doesn't have to implement the above `fedoraImage` function,
as the function could be imported from a remote location.

Podenv provides a [Prelude](https://github.com/podenv/podenv/blob/master/podenv/dhall/package.dhall),
but using it is optional and the user configuration
can load definitions from any HTTP location, including [ipfs](https://ipfs.io)
gateway.

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

Dhall's list elements shall all have the same signature.
Podenv Prelude includes [schemas](https://github.com/podenv/podenv/blob/master/podenv/dhall/types/Env.dhall)
that can be used to prevent such mistakes:

```dhall
-- ./test.dhall
let Podenv = ./podenv/package.dhall

in  Podenv.Schemas.Env::{ name = "firefox", command = [ "firefox" ] }
```
```console
$ dhall-to-yaml --file ./test.dhall
Error: Expression doesn't match annotation

{ - image : …
, …
}
```

The above example is incorrect because an Env needs an image attribute:

```dhall
-- ./test.dhall
let Podenv = ./podenv/package.dhall

in  Podenv.Schemas.Env::{
    , name = "firefox"
    , command = [ "firefox" ]
    , image = "firefox"
    }
```
```console
$ dhall-to-yaml --file ./test.dhall
command:
- firefox
image: firefox
name: firefox
```

This is incredibly powerful when refactoring a configuration or when
an import changes un-expectedly: dhall ensures the resulting configuration
is consistent.

## Conclusion

Dhall produces a configuration object and this can still
be written in plain YAML. However, the plain configuration become less flexible
because some of the podenv logic can be expressed with dhall.

The good news is that dhall renders most of the podenv [defaults.py](https://github.com/podenv/podenv/blob/master/podenv/defaults.py) and [pod.py](https://github.com/podenv/podenv/blob/30d2543786e0a1a30f500a82cbc04c11013f080e/podenv/pod.py#L227-L887)
modules obsolete and makes podenv configuration more flexible.
For example, this enables:

* A less obscure and better image management,
* Custom configurations such as assigning a home volume and network to a group of environments, and
* Dynamic definition.

Again, please note that is still a work in progress and dhall support is not fully implemented.
