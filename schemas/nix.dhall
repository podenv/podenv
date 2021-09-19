-- | This is a copy of the hub/Builders/nix.dhall so that podenv can build nix expression without the hub.
-- Regenerate using: `dhall <<< ../hub/Builders/nix.dhall > schemas/nix.dhall`
{ capabilities =
  { alsa = False
  , cwd = False
  , dbus = False
  , dri = False
  , gitroot = False
  , gpg = False
  , hostfile = False
  , interactive = False
  , keep = False
  , kerberos = False
  , kvm = False
  , network = True
  , pipewire = False
  , pulseaudio = False
  , root = False
  , ssh = False
  , terminal = False
  , tun = False
  , video = False
  , wayland = False
  , x11 = False
  }
, command =
  [ "bash"
  , "-c"
  , "test -d /nix/var || ( cd /tmp && curl -OL https://nixos.org/releases/nix/nix-2.3.15/nix-2.3.15-x86_64-linux.tar.xz && echo 'aae346f0ee447efa042c38e320aee0368e3c6c7fa331d76f708bbe8539f694fa  nix-2.3.15-x86_64-linux.tar.xz' | sha256sum -c && tar xf nix-2.3.15-x86_64-linux.tar.xz && /tmp/nix-2.3.15-x86_64-linux/install && rm -r /tmp/nix-*-x86_64-linux* && ln -s /nix/var/nix/profiles/per-user/user/profile /nix/var/nix/profiles/default && /nix/var/nix/profiles/default/bin/nix-collect-garbage --delete-old && /nix/var/nix/profiles/default/bin/nix-store --optimise && /nix/var/nix/profiles/default/bin/nix-store --verify --check-contents ); nix --version"
  ]
, description = Some "Setup a nix store in a podman volume"
, environ = [] : List Text
, home = None Text
, name = ""
, namespace = None Text
, provide = None < Tcp : Natural >
, runtime =
    < Container :
        { containerfile : Text
        , image_home : Optional Text
        , image_name : Optional Text
        , image_update : Optional Text
        , image_volumes : List Text
        }
    | Image : Text
    | Nix : Text
    >.Container
      { containerfile =
          ''
          # Adapted from https://github.com/NixOS/docker/blob/master/Dockerfile
          FROM registry.access.redhat.com/ubi8:latest
          ARG USER_UID
          RUN dnf update -y && dnf install -y xz ncurses-compat-libs && useradd -u ''${USER_UID} -d /home/user -m user && mkdir -p /home/user/.cache /home/user/.config /run/user/''${USER_UID} /run/user/0 && chown -R ''${USER_UID}:''${USER_UID} /home/user /run/user/ && mkdir -m 0755 -p /etc/nix && echo 'sandbox = false' > /etc/nix/nix.conf && dnf clean all
          RUN ln -sf /nix/var/nix/profiles/default/etc/profile.d/nix.sh /etc/profile.d/ && mkdir -m 0755 -p /etc/nix && echo -e 'sandbox = false\nbuild-users-group =' > /etc/nix/nix.conf
          ENV USER=user
          ENV HOME=/home/user
          ENV PATH=/nix/var/nix/profiles/default/bin:/bin:/sbin
          ENV GIT_SSL_CAINFO=/etc/pki/tls/certs/ca-bundle.crt
          ENV NIX_SSL_CERT_FILE=/etc/pki/tls/certs/ca-bundle.crt
          ENV NIX_PATH=/nix/var/nix/profiles/per-user/user/channels
          ''
      , image_home = Some "/home/user"
      , image_name = Some "local-nix"
      , image_update = None Text
      , image_volumes = [ "nix-store:/nix" ]
      }
, syscaps = [] : List Text
, volumes = [ "nix-store:/nix", "nix-profiles:/profile" ]
}
