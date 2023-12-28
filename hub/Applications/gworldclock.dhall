let Podenv = ../Podenv.dhall

in  Podenv.Application::{
    , description = Some "Displays time and date in specified time zones."
    , runtime = (./debian.dhall).bullseye.use [ "gworldclock" ]
    , command = [ "gworldclock" ]
    , capabilities = Podenv.Capabilities::{ x11 = True }
    }
