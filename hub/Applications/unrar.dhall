let Podenv = ../Podenv.dhall

in  Podenv.Application::{
    , description = Some "Extract archive"
    , command = [ "unrar" ]
    , runtime = (./fedora.dhall).latest.use [ "unrar" ]
    , capabilities = Podenv.Capabilities::{ cwd = True }
    }
