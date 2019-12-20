{-
The main environment declaration.
../defaults/Env.dhall contains the defaults
../schemas/Env.dhall is the schema to be used like so:

`let my-env = (../schemas/Env.dhall)::{name = "my-env"}`
-}
{ name : Text
, image : Optional Text
, capabilities : ./Capabilities.dhall
, container-file : Optional (List ./Task.dhall)
, container-update : Optional (List ./Task.dhall)
, build-env : Optional ./BuildEnv.dhall
, description : Optional Text
, url : Optional Text
, mounts : Optional (List ./Mount.dhall)
, volumes : Optional (List ./Volume.dhall)
, packages : Optional (List Text)
, hostname : Optional Text
, home : Optional Text
, work-dir : Optional Text
, command : Optional (List Text)
, user : Optional ./User.dhall
, environ : Optional ./Environ.dhall
, network : Optional Text
, ports : Optional (List Text)
, syscaps : Optional (List Text)
, add-hosts : Optional (List { Name : Text, IP : Text })
, pre-tasks : Optional (List ./Task.dhall)
}
