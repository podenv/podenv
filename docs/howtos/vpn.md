# Configure a VPN

TODO...


Network can be shared:

```yaml
  corpvpn:
    parent: fedora
    network: corpvpn
    packages: ["openvpn"]
    capabilities:
      network: True
      root: True
      tun: True
      seccomp: False
      uidmap: True
    syscaps: ["NET_ADMIN", "SYS_RESOURCE", "SETGID", "SETUID"]
    home: ~/.config/corpvpn/
    command:
      - openvpn
      - --config
      - corp.config

  corpbrowser:
    network: corpvpn
    parent: firefox
```
