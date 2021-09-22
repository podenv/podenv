let Podenv = ../hub/Podenv.dhall

let fedora =
      \(pkgs : List Text) ->
        Podenv.Container
          Podenv.ContainerBuild::{ containerfile = "FROM fedora" }

in  { firefox.runtime = fedora [ "firefox" ] }
