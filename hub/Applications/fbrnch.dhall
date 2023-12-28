let Podenv = ../Podenv.dhall

in  Podenv.Application::{
    , description = Some "Tool to update fedora packages branches"
    , runtime =
        (./fedora.dhall).latest.use
          [ "fbrnch", "krb5-workstation", "openssh-clients" ]
    , command = [ "fbrnch" ]
    , capabilities = Podenv.Capabilities::{ network = True, cwd = True }
    }
