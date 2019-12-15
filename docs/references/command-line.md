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
usage: podenv [--verbose] [--debug] [--config FILE] [--expr EXPR]
              [--show] [--list] [--list-caps] [--rebuild ENV] [--update ENV]
              [--shell] [--net NAME] [--home PATH] [--image IMAGE] [--environ KEY=VALUE]

podenv - a podman wrapper

commands:
  --list          List the environments
  --list-caps     List available capabilities
  --show          Print the environments
  --update ENV    Update environment image
  --rebuild ENV   Rebuild environment image
  ENV [ARGS]      Execute an environment

environment execution arguments:
  --home PATH     Set environment home directory
  --net  NAME     Set environment network
  --image IMAGE   Set environment image
  --shell         Run a shell instead of the default command
  --environ K=V   Add an environ(5) variable
  --CAP           Activate a capability
  --no-CAP        Disable a capability

flags:
  --verbose       Prints logs such as exec argv
  --debug         Prints even more log
  --config PATH   Set the config file path,
                  (defaults to PODENV_CONFIG or ~/.config/podenv/config.dhall)
  --expr EXPR     Evaluate a config expression from the command line
```
