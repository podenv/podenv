{ description = None Text
, image = None Text
, url = None Text
, capabilities = ./Capabilities.dhall
, container-file = None (List ../types/Task.dhall)
, container-update = None (List ../types/Task.dhall)
, build-env = None ../types/BuildEnv.dhall
, environ = None ../types/Environ.dhall
, packages = None (List Text)
, network = None Text
, ports = None (List Text)
, syscaps = None (List Text)
, command = None (List Text)
, mounts = None (List ../types/Mount.dhall)
, add-hosts = None (List { Name : Text, IP : Text })
, pre-tasks = None (List ../types/Task.dhall)
}
