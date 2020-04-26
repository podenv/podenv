# Edit the configuration

Podenv configuration is defined with the [dhall](https://dhall-lang.org) language.
This tutorial teachs you some dhall basics, how to edit the configuration, and
add a new environment.


## Start the dhall-editor

We want to use a proper editor to update the configuration, thus we are
going to start the environment already provided by podenv. Though any
text editor could work too.

The hub provides an environment to edit the podenv configuration,
start the editor using:

```console
$ podenv --expr '(env:PODENV_HUB).Environments.Emacs.ConfigEditor'
[~/.config/podenv/config.dhall content appears in an emacs window]
```

For the purpose of this tutorial, here are the keybindings you need to know:

| Keybinding | Action                             |
| ---------- | ---------------------------------- |
| C-x C-s    | Format and save the file           |
| F5         | Evaluate the configuration         |
| C-x o      | Move cursor to the other buffer    |
| q          | Close the evaluation buffer        |
| C-x C-q    | Quit emacs                         |

> * `C-x C-s` means that you have to maintain pressed the `Control` key while pressing `x` and `s`.
> * `C-x o` means that you have to first press `Control` and `x` together, then release the control keys and press `o`.
> * `F5` means press the Function key 5.

The default configuration should contains this expression:

```dhall
(env:PODENV_HUB).Defaults
```

Press `F5` to evaluate the file. A new buffer should appear with the result of the expression,
which is the list of the defaults environments.


## The default configuration

The expression you evaluated is composed of two elements:

* `(env:PODENV_HUB)` is resolved to the environment variable PODENV_HUB which is set by podenv
  and it's value is the location of the [podenv/hub/package.dhall](https://github.com/podenv/hub/blob/master/package.dhall).
* `.Defaults` is the value of the Defaults key of the hub package record, which is a list of environments.

There are two things to note:

* Dhall automatically load the content of a path or url as an expression. This is a great way to compose
  and re-use code.
* A record is like a dictionary defined like so `{ KeyName = "KeyValue" }` and you can access its value using
  the `.` notation: `({ KeyName = "KeyValue" }).KeyName` (which evaluate to "KeyValue").


## The `let` ... `in` notation

We want to describe complex things and we will use the `let` ... `in` notation
to make the configuration easier to read. `let` enables us to define symbols
to be used in the `in` expression. For example we can rewrite the
default configuration as:

```dhall
let Hub = env:PODENV_HUB

in Hub.Defaults
```

If you press `F5` the result is identical as before.

Podenv also provides another convenient package that contains the schemas of
the environment attributes, we can add it to our configuration like so:

```dhall
let Podenv = env:PODENV_PRELUDE

let Hub = env:PODENV_HUB

let default-envs = Hub.Defaults

in default-envs : List Podenv.Env.Type
```

In the above example we used the ` : ` symbol to provide a type annotation.
` : ` is used to indicate what is the expected type of the previous expression or symbol.

There is one thing to note: `let` ... `in` notation can be used nested in any expression. For example:

```dhall
let a-symbol = let var1 = 42
               let var2 = 43
               in var1 + var2

in  let result = a-symbol + 1
    in result
```

Evaluates to `86`.


## The `::` schemas notation

We want to describe our own environment. Replace the configuration with:

```dhall
let Podenv = env:PODENV_PRELUDE

let Hub = env:PODENV_HUB

let default-envs = Hub.Defaults

let my-new-env = { name = "my-new-env" }

in  my-new-env : Podenv.Env.Type
```

We added a new symbol named `my-new-env` which is a record with a single key `name`.
Then the `in` expression indicate that the evaluation of this configuration is a `Env.Type`.

Pressing `F5` to evaluate the configuration results in this error:

```console
Error: Expression doesn't match annotation

…
{ - container-file : …
, - command : …
[...]
}
```

Each element prefixed with a `-` indicate that our record is missing a key. That is because a `Env.Type`
is composed of many attributes that we didn't set. To fix that, we use the `Env` schema to provides
default values. Replace the `my-new-env` value by:

```dhall
let my-new-env = Podenv.Env::{ name = "my-new-env" }
```

That evaluates without error because the schemas took care of setting the default value.

If you made a typo like so:

```dhall
let my-new-env = Podenv.Env::{ nam = "my-new-env" }
```

Then the evaluation fails with:

```console
Error: Expression doesn't match annotation

{ - name : …
, + nam : …
, …
}
```

A single element is prefixed with a `-` meaning that the record is missing the `name` attribute.
The typo `nam` is prefixed with a `+` meaning that the record includes an extra, unknown attribute `nam`.


## The `//` operator

Creating a new environment from scratch is beyond the scope of this tutorial. Instead we are going
to modify an existing environment using the `//` operator. This operator let us merge two records
by overwritting the key of the left records by the one of the right.

Replace the configuration file with:

```dhall
let Podenv = env:PODENV_PRELUDE

let Hub = env:PODENV_HUB

let default-envs = Hub.Defaults

let my-env =
          Hub.Environments.Emacs.IDE
      //  { name = "my-emacs", image = Some "localhost/podenv/test" }

in  my-env
```

We created a new environment by replacing the name and image of the `Emacs.IDE` definition:

```console
$ podenv --list
NAME       DESCRIPTION
my-emacs   Extensible text editor
```

## The `#` operator

The podenv configuration can either be a single environment, or a list of environments.
We want to still have access to the defaults environments, thus we use the `#` operator
to concatenate our environment to the initial list:

```dhall
let Podenv = env:PODENV_PRELUDE

let Hub = env:PODENV_HUB

let default-envs = Hub.Defaults

let my-env =
          Hub.Environments.Emacs.IDE
      //  { name = "my-emacs", image = Some "localhost/podenv/test" }

in  default-envs # [ my-env ]
```

```console
$ podenv --list
NAME                 DESCRIPTION
[...]
firefox              Mozilla Firefox
[...]
my-emacs             Extensible text editor
[...]
```

## Function application

Our new environment is still missing a valid `image`. The Hub.Environments objects doesn't
usually specify the runtime. To specify the runtime, we'll use a function. Most functions
in podenv takes an environment as a parameter and returns a new environment.

Write this final configuration:

```dhall
let Hub = env:PODENV_HUB

let my-env = Hub.Environments.Emacs.IDE // { name = "my-emacs" }

in  Hub.Defaults # [ Hub.Runtimes.Fedora.Create.Latest my-env ]
```

The `Hub.Runtimes.Fedora.Create.Latest` is a function that takes care of adding
a Containerfile to the environment.

To summarize:

* We defined new symbols using the `let` ... `in` notation.
* We created a new env constructed with the *Emacs.IDE* object, of which we changed the name to *my-emacs* using the `//` operator.
* We added the new env to the list of defaults environment using the `#` operator.
* We specified a Fedora runtime by applying the *Runtimes.Fedora.Create.Latest* function.

* We could have created a new environment using the `::` schema operator.
* We could have annotated the type of the expressions using the ` : ` notation.

This concludes the tutorial on how to edit the podenv configuration.
By this point, you should understand how dhall works and how to use the Hub package.

To learn more about dhall, you can check out [Learn Dhall in Y minutes](https://learnxinyminutes.com/docs/dhall/) or the official [Getting Started](https://docs.dhall-lang.org/tutorials/Getting-started_Generate-JSON-or-YAML.html).
