# Configure a VPN

In this how-to guide we'll see how to use podenv to run the openvpn application
from the [hub](https://github.com/podenv/podenv/tree/main/hub).

## Create an openvpn container

The first thing we need to do is setup the openvpn configuration file.
Create a directory named *~/.config/openvpn* and write a configuration file
such as:

```bash
mkdir -p ~/.config/openvpn
cat << EOF > ~/.config/openvpn/ovpn.conf
client
dev vpn0
dev-type tun
nobind
user nobody
group nobody
# Copy the certificate in ~/.config/openvpn/example.pem
ca example.pem
# Set the remote host here
remote vpn.example.com
# Auth option
auth-user-pass
EOF
```

Then start the application using this command:

```ShellSession
$ podenv --volume ~/.config/openvpn:~/ openvpn ovpn.conf
Enter Auth Username:
```

Note that the openvpn process is started in a container namespace.
That means the new network interface is not available from the host namespace.

## Join the network namespace manually

There are multiple ways to join the openvpn namespace.

Using the podman exec command:

```console
$ podman exec -it openvpn bash
```

Using the podman run command:

```console
$ podman run -it --network container:openvpn --rm fedora
```

Using the nsenter command:

```console
$ sudo nsenter --net --target $(pidof openvpn) -- ip link show vpn0
```

## Share network between applications

When joining the network manually, the session may loose connectivity when
the openvpn container dies. And when openvpn get restarted, the foreign processes
need to rejoin the new namespace.

Podenv can provide a persistent network namespace by using an *infra*
container to keep the namespace alive. This can be activated by using the
`--namespace` command line.

Restart the `openvpn` container using:

```console
$ podenv --volume ~/.config/openvpn:~/ --network ovpn openvpn ovpn.conf
```

Then other application can join the namespace, for example:

```console
$ podenv --network ovpn firefox
```

Using podenv namespace, the openvpn container can be restarted when needed, and
the other applications will recover the connectivity transparently.


## Persist the configuration

Using command line arguments to manage network can be difficult.
To simplify podenv usage, it is recommended to create a static configuration.

Write this configuration file in *~/.config/podenv/local.d/corp.dhall*:

```dhall
let Podenv = env:PODENV

let ns =
      \(name : Text) ->
      \(application : Podenv.Application.Type) ->
        Podenv.ApplicationResource::{
        , application
        , volumes = [ "~/.config/${name}:~/" ]
        , network = Podenv.Network.Shared "corp-vpn"
        }

in  { vpn = (ns "openvpn" Podenv.Hub.openvpn) with metadata.name = Some "vpn"
    , web = ns "firefox-vpn" Podenv.Hub.firefox.default
    , bridge = ns "bridge" (Podenv.Hub.ssh.client "user@internal-host")
    }
```

Three new applications are now configured to share the namespace:

```ShellSession
$ podenv --list
corp.bridge: Application (OpenSSH client)
corp.vpn: Application (VPN solution)
corp.web: Application (Mozilla Firefox)
```

That can be used like so:

```ShellSession
$ podenv corp.vpn ovpn.conf
[vpn connection dialog]
```

```ShellSession
$ podenv corp.web
[firefox window started in vpn]
```

You might want to customize the image to include extra tools and internal CA.
Checkout the [Custom Image](./image.md) howto next.
