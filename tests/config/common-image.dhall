{- Change the image of envs to use another dhall config -}
let Podenv = env:PODENV_PRELUDE

let Env = Podenv.Env.Type

let basic-env
    : forall (name : Text) -> Env
    = \(name : Text) ->
        Podenv.Env::{ name, image = Some name, command = Some [ name ] }

let default-envs = [ basic-env "firefox", basic-env "emacs" ]

let update-image
    : forall (image : Text) -> forall (envs : List Env) -> List Env
    = \(image : Text) ->
        (env:PODENV_HUB).Prelude.List.map
          Env
          Env
          (\(env : Env) -> env // { image = Some image })

let image = "shared-image-name"

in  update-image image default-envs
