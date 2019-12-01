# Contribute to podenv

Podenv is a free software in early development. Issues or changes are most welcome.

* If something doesn't work as expected, please [open an issue](https://github.com/podenv/podenv/issues/new).
* Inacurate documentation or found a typo, please click the **edit** button to propose a fix.
* Create a pull request for missing features.

## Update podenv

For example, to add a new capability, first create a branch:

```bash
$ git branch new-cap-name
```

Update the code:

```bash
$ $EDITOR podenv/env.py
```

Test the change using the [Developper installation](../tutorials/install.md):

```bash
$ alias podenv="env PYTHONPATH=$(pwd) python3 $(pwd)/podenv/main.py"
$ podenv --new-cap-name shell --verbose
```

Run the tests by using the `podenv` command at the root of the project:

```bash
$ podenv  # this just uses make to run the tests
```

Open a pull request using [git-pull-request](https://github.com/Mergifyio/git-pull-request).

```bash
$ python3 -mpip install --user git-pull-request
$ git pull-request
```

Or using the `git-pull-request` environment:

```bash
$ podenv git-pull-request
```

To save git pull-request passwords, use this git config:

```bash
$ git config --global credential.helper 'store --file ~/.config/git/credentials'
```
