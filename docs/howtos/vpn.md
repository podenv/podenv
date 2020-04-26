# Configure a VPN

Podenv enables network to be shared between multiple environments.
In this how-to guide we'll see how to use podenv to start openvpn and how
to start further environment inside the new network.

## Create an openvpn container

The first thing we need to do is setup the openvpn configuration file.
Create a directory named *~/.config/openvpn* and write a configuration file
such as:

```console
mkdir -p ~/.config/openvpn
cat << EOF > ~/.config/openvpn/ovpn.conf
client
dev vpn0
dev-type tun
nobind
user nobody
group nobody
# Copy the certificate in ~/.config/openvpn/localhost.pem
ca localhost.pem
# Set the remote host here
remote localhost
# Auth option
auth-user-pass
EOF
```

Then you can start the environment using this command:

```console
$ podenv --home ~/.config/openvpn openvpn ovpn.conf
Enter Auth Username:
```

Note that the openvpn process is started in a container namespace.
That means the new network interface and its routes are not
available from the host namespace.

## Join the network namespace

There are multiple ways to join the openvpn namespace.

Using the podman exec command:

```console
$ podman exec -it openvpn ip link show vpn0
```

Using the podman run command:

```console
$ podman run -it --network container:openvpn --rm fedora
```

Using the nsenter command:

```console
$ sudo nsenter --net --target $(pidof openvpn) -- ip link show vpn0
```

## Share environment network

When joining network directly, the process may loose connectivity when
the openvpn container dies. And when openvpn get restarted, the foreign processes
need to rejoin the new namespace.

Podenv can provide a persistent network namespace by using an *infra*
container to keep the namespace alive. This can be activated by using the
`--net` command line with an arbritary name.
Restart the **openvpn** container using:

```console
$ podenv --home ~/.config/openvpn --net ovpn openvpn ovpn.conf
```

Then other environment can join the namespace, for example:

```console
$ podenv --net ovpn firefox
```

With that setup, the openvpn container can be restarted when needed, and
the other environment will recover the connectivity transparently.


## Persist the configuration

Using command line arguments to manage network can become difficult.
To simplify podenv usage, it's better to specify the configuration
of commonly used environment.

Here is an example configuration for such setup:

```dhall
let Podenv = env:PODENV_PRELUDE

let Hub = env:PODENV_HUB

let vpn-envs =
      [ Hub.Environments.OpenVPN // { home = Some "~/.config/openvpn" }
      , Hub.Environments.Firefox.Extended
      , Hub.Environments.Shell
      ]

let work-envs =
      Hub.Functions.mapEnv
        (     \(env : Podenv.Env.Type)
          ->  Hub.Runtimes.Fedora.Create.Latest
                (env // { name = "work-" ++ env.name, network = Some "work" })
        )

in Hub.Defaults # work-envs vpn-envs
```

The statements mean:

* Podenv is the provided types and Hub is the podenv/hub package
* vpn-envs is a list of environment we want to use inside our vpn
* work-envs is a function that will apply a function to each environments using the `mapEnv` function.
* work-envs is applied to vpn-envs and the results is added to the list of environments.

work-envs does three things to it's parameter:

* Setup a Fedora runtime
* Change the environment name to be prefixed by "work-"
* Configure the network to be "work"

We now have three new environments:

```console
$ podenv --list
NAME                 DESCRIPTION
[...]
work-firefox         Mozilla Firefox
work-openvpn         VPN solutions
work-shell           A simple terminal
```

That we can use like so:

```console
$ podenv work-openvpn ovpn.conf
[vpn connection dialog]
```

```console
$ podenv work-firefox
[firefox window started in work vpn]
```
