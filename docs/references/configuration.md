# Configuration references

This document references the configuration file format.

## Application

An application is defined with the [Application](https://github.com/podenv/podenv/blob/main/hub/schemas/Application.dhall) schema.
This includes the [Runtime](https://github.com/podenv/podenv/blob/main/hub/schemas/Runtime.dhall) and the [Capabilities](https://github.com/podenv/podenv/blob/main/hub/schemas/Capabilities.dhall) schemas.

## Configuration

The podenv configuration can either be:

- A single application: `podenv --config https://raw.githubusercontent.com/podenv/hub/main/Applications/audacity.dhall`.
- A collection: `podenv --config https://github.com/podenv/hub/blob/main/Applications/package.dhall audacity`.

## Collection

When the configuration is a collection, podenv traverses the record and build the application list using dot separated selectors.

Given:

```dhall
{ firefox = {
    default = {=},
    codecs = {=}
}, gimp = {=}
}
```

… the application list is:

- firefox
- firefox.codecs
- gimp

> The `default` record attribute is exposed as the parent name.

## Weakly typed

Configuration can also be weakly typed:

```dhall
{ name = "toolbox"
, description = Some "A simple toolbox"
, runtime.image = "fedora:latest"
, capabilities = {
  , terminal = True
  , interactive = True
  , network = True
  }
}
```

… which is loaded as:

```dhall
Application.defaults // record // {
, runtime = Runtime.Image "fedora:latest"
, capabilities = Capabilities.defaults // record.capabilities
}
```

## Functional Application

When the application needs a parameter to be useful, for example a file viewer needs a file path, then the application can be defined as function. For example:

```dhall
-- mupdf.dhall
\(file : Text) -> (env:PODENV).Application::{
  command = ["mupdf", file],
  runtime = (env:PODENV).Image "localhost/mupdf"
}
```

… can be used by providing the parameter on the command line:

```ShellSession
$ podenv mupdf ./my-file.pdf
```

> Such application is listed as: `mupdf: λ file → Application`

Application transformation can also be defined as:

```dhall
-- nixify.dhall : Add Nix to an existing application
\(app : Podenv.Application.Type) ->
      app
  //  { environ =
          [ "NIX_SSL_CERT_FILE=/etc/pki/tls/certs/ca-bundle.crt" ] # app.environ
      , volumes = [ "nix-store:/nix" ] # app.volumes
      }
```

… which can be used by providing the target application on the command line:

```ShellSession
$ podenv nixify image:ubi8
```

> Such application is listed as: `nixify: λ app → app`
