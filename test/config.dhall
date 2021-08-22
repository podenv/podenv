let Podenv = ../package.dhall

let fedora =
      \(pkgs : List Text) ->
        Podenv.Container
          Podenv.ContainerBuild::{ containerfile = "FROM fedora" }

in  { firefox = Podenv.Application::{ runtime = fedora [ "firefox" ] } }
