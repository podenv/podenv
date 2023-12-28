(../Podenv.dhall).Application::{
, description = Some "VPN solution"
, runtime = (./fedora.dhall).latest.use [ "openvpn", "iproute" ]
, command = [ "openvpn" ]
, syscaps = [ "NET_ADMIN", "SYS_RESOURCE", "SETUID", "SETGID" ]
, capabilities = (../Podenv.dhall).Capabilities::{
  , root = True
  , network = True
  , tun = True
  , interactive = True
  , terminal = True
  }
}
