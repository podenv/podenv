# Command line interface

The podenv command line excepts an environment's `name` and optional `arguments`.

There are a few switches that results in different actions:

* `--show` dry run, show the enviroment definition and the resulting configuration.
* `--list` list the enviroments.
* `--list-caps` list the available capabilities.

## Usage

Here is the output of the `--help`:

```bash
podenv - a container wrapper

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

## Smart volumes

The volumes syntax is flexible: `host:container`

- The container part can be omited, in that case it maps to the host.
- The host part can be a path or a volume name.
- `~/` resolve to the HOME value for the host, and the application home for the container.

For example:

- `--volume ~` share the home directory.
- `--volume web:~` use a volume named `web` for the container home.
- `--volume /srv` share an absolute path.
