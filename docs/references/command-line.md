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

Usage: podenv [--list] [--list-caps] [--show] [--config ARG] [--update]
              [--verbose] [--shell] [--namespace ARG] [--home PATH]
              [--name NAME] [--env ENV] [--volume VOLUME] [APP] [ARGS]

Available options:
  --list                   List available applications
  --list-caps              List available capabilities
  --show                   Show the environment without running it
  --update                 Update the runtime
  --verbose                Increase verbosity
  --shell                  Start a shell instead of the application command
  --namespace ARG          Share a network ns
  --home PATH              Host path for application home
  --name NAME              Container name
  --env ENV                Extra env 'KEY=VALUE'
  --volume VOLUME          Extra volumes 'volume|hostPath[:containerPath]'
  APP                      Application config name or image:name or nix:expr
  ARGS                     Application args
  -h,--help                Show this help text
```
