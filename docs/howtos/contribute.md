# Contribute to podenv

Podenv is a free software in early development. Issues or changes are most welcome.

* If something doesn't work as expected, please [open an issue](https://github.com/podenv/podenv/issues/new).
* Inacurate documentation or found a typo, please click the **edit** button to propose a fix.
* Create a pull request for missing features.

## Source install

To work on this project you need a Haskell toolchain, for example on fedora:

```ShellSession
$ sudo dnf install -y ghc cabal-install && cabal update
```

Build and run the project:

```ShellSession
$ cabal run podenv -- --help
```

## Run tests

```ShellSession
$ cabal test
```

Re-run a single test with:

```ShellSession
$ cabal test --test-option=--match --test-option="/unit tests/$name/"
```

## Nix shell

Run local hoogle service:

```ShellSession
$ nix-shell --arg withHoogle true --command "hoogle server -p 8080 --local --haskell"
```

Auto run the tests with ghcid:

```ShellSession
$ nix-shell --command "ghcid --command='cabal v2-repl test:tests' --test 'Main.main'"
```
