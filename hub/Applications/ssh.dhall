let Podenv = ../Podenv.dhall

let client =
      \(host : Text) ->
        Podenv.Application::{
        , description = Some "OpenSSH client"
        , runtime = (./fedora.dhall).latest.use [ "openssh-clients" ]
        , command = [ "ssh", host ]
        , capabilities = Podenv.Capabilities::{
          , ssh = True
          , network = True
          , terminal = True
          , interactive = True
          }
        }

in  { client }
