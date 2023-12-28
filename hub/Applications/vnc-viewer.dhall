(../Podenv.dhall).Application::{
, description = Some "Remote display client"
, runtime = (./fedora.dhall).latest.useGraphic [ "tigervnc" ]
, command = [ "vncviewer" ]
, capabilities = (../Podenv.dhall).Capabilities::{ x11 = True, network = True }
}
