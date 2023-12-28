-- | Network utility
let Podenv = ../Podenv.dhall

let http-server =
      \(port : Text) ->
        Podenv.Application::{
        , runtime = Podenv.Image ((../Builders/fedora.dhall).image-ref "latest")
        , command = [ "python3", "-u", "-m", "http.server", port ]
        , namespace = Some "host"
        , capabilities = Podenv.Capabilities::{ network = True, cwd = True }
        }

in  { http-server }
