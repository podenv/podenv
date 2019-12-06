{- A minimal dhall config -}
let Podenv = env:PODENV_PRELUDE

in  [ Podenv.Schemas.Env::{
      , name = "shell"
      , image = Some "fedora"
      , command = Some [ "/bin/bash" ]
      , capabilities = Podenv.Schemas.Capabilities::{ terminal = Some True }
      }
    ]
