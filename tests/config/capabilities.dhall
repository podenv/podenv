{- A minimal dhall config -}
let Podenv = ./podenv/package.dhall

in  Podenv.Schemas.Config::{
    , system = { dns = None Text }
    , environments =
        [ Podenv.Schemas.Env::{
          , name = "shell"
          , image = "fedora"
          , command = [ "/bin/bash" ]
          , capabilities = Podenv.Schemas.Capabilities::{ terminal = Some True }
          }
        ]
    }
