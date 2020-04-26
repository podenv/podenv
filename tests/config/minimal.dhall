{- A minimal dhall config -}
let Podenv = env:PODENV_PRELUDE

in  [ Podenv.Env::{
      , name = "shell"
      , image = Some "fedora"
      , command = Some [ "/bin/bash" ]
      }
    ]
