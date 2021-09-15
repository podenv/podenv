# Configuration references

This document references the configuration file format.

## Application

### Attributes

See [Application.Type](../../schemas/Application.dhall)

### Capabilities list

See [Capabilities.Type](../../schemas/Capabilities.dhall)

## Runtime

Application defines a runtime, see [Runtime.Type](../../schemas/Runtime.dhall).

## Functional application

Application can be function:

### Application argument

When the application needs a parameter to be useful, for example a file viewer needs a file path, then the application can be defined as function. For example:

```dhall
-- mupdf.dhall
let mupdf = \(file : Text) -> (env:PODENV).Application::{
  command = ["mupdf", file]
}
in mupdf
```

… which can be provided on the command line

```ShellSession
$ podenv --config ./mupdf.dhall ./my-file.pdf
```
