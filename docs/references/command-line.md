# Command line interface

The podenv command line excepts an environment's `name` and optional `arguments`.

There are a few switches that results in different actions:

* `--show` dry run, show the enviroment definition and the resulting configuration.
* `--list` list the enviroments.

All the other arguments are override for the environment definition, for example,
to enable or disable network access use `--network` or `--no-network`.

## Usage

Here is the output of the `--help`:

```bash
podenv - a podman wrapper

Usage: podenv [--version] [--list] [--list-caps] [--show] [--config ARG]
              [--update] [--verbose] [--shell] [--namespace ARG] [--name NAME]
              [--env ENV] [-v|--volume VOLUME] [APP] [ARGS]

Available options:
  --version                Show version
  --list                   List available applications
  --list-caps              List available capabilities
  --show                   Show the environment without running it
  --config ARG             A config expression
  --update                 Update the runtime
  --verbose                Increase verbosity
  --shell                  Start a shell instead of the application command
  --namespace ARG          Share a network ns
  --name NAME              Container name
  --env ENV                Extra env 'KEY=VALUE'
  -v,--volume VOLUME       Extra volumes 'volume|hostPath[:containerPath]'
  APP                      Application config name or image:name or nix:expr
  ARGS                     Application args
  -h,--help                Show this help text
```
