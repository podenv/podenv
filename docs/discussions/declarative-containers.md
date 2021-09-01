# Declarative containers

## Podenv design

Podenv converts an application into a runtime configuration:

- `Podenv.Application` module contains the main capability logic,
- `Podenv.Build` module describes how to build optional runtime,
- `Podenv.Runtime` module executes the application.
