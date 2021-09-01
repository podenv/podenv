# Custom Image

In this how-to guide we'll see how to use a custom image.

## Create a custom image

In a file named *~/.config/podenv/image.dhall* write:

```dhall
-- | A custom runtime
let Podenv = env:PODENV

let Hub = ./Hub.dhall

let ca =
      Text/show
        ''
        -----BEGIN CERTIFICATE-----
        <insert tls CA content>
        -----END CERTIFICATE-----
        ''

let packages =
      let base =
            [ "iproute"
            , "procps-ng"
            , "psmisc"
            , "nmap-ncat"
            , "make"
            , "git"
            , "rsync"
            , "openvpn"
            ]

      let python = [ "python3-pyyaml", "python3-pexpect", "python3-pip" ]

      let gfx =
            [ "xdg-desktop-portal-gtk"
            , "qt5-qtwayland"
            , "pipewire-gstreamer"
            , "mozilla-ublock-origin"
            , "firefox"
            , "ffmpeg"
            ]

      in  base # python # gfx

let -- | Ensure the same image name is used for all the application
    set-image =
      { image_name = Some "podenv" }

let ver = "34"

let extra =
      ''
      RUN dnf install -y https://download1.rpmfusion.org/free/fedora/rpmfusion-free-release-${ver}.noarch.rpm
      RUN echo ${ca} > /etc/pki/ca-trust/source/anchors/corp.pem && update-ca-trust
      ENV KRB5CCNAME=/home/fedora/.ticket
      ''

in  Podenv.Container (Hub.fedora.useImage ver extra packages // set-image)
```

## Override applications

In a file named *~/.config/podenv/local.dhall* write:

```
-- | My applications
let Hub = ./Hub.dhall

let my-image = { runtime = ./image.dhall }

in { firefox = Hub.firefox.default // my-image
   , openvpn = Hub.openvpn // my-image
   }
```

And update the default set, in *~/.config/podenv/config.dhall*:

```
./Hub.dhall // ./local.dhall
```

Check that firefox uses the new image:

```ShellSession
$ podenv --show firefox
[should display the CA setup in the containerfile]
```
