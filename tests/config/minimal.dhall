{- A minimal dhall config -}
let Podenv = ./podenv/package.dhall

in  Podenv.Schemas.Config::{
    , system = { dns = None Text }
    , environments =
        [ Podenv.Schemas.Env::{ name = "shell", command = [ "/bin/bash" ] } ]
    }
