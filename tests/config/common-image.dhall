{- Change the image of envs to use another dhall config -}
let Podenv = ./podenv/package.dhall

let Env = Podenv.Types.Env

let basic-env
    : forall (name : Text) -> Env
    =     \(name : Text)
      ->  Podenv.Schemas.Env::{ name = name, image = name, command = [ name ] }

let default-envs = [ basic-env "firefox", basic-env "emacs" ]

let update-image
    : forall (image : Text) -> forall (envs : List Env) -> List Env
    =     \(image : Text)
      ->  Podenv.Prelude.List.map
            Env
            Env
            (\(env : Env) -> env // { image = image })

let image = "shared-image-name"

in  Podenv.Schemas.Config::{
    , system = { dns = None Text }
    , environments = update-image image default-envs
    }
