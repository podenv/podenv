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

Install in your PATH:

```ShellSession
$ cabal install exe:podenv --installdir=$HOME/.local/bin
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

Run ci test:

```ShellSession
$ nix develop .#ci -c run
```

Run local hoogle service:

```ShellSession
$ nix develop .#hoogle -c run
```

Auto run the tests with ghcid:

```ShellSession
$ nix develop --command ghcid-test
```
