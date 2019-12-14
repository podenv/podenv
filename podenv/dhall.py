# Copyright 2019 Red Hat
#
# Licensed under the Apache License, Version 2.0 (the "License"); you may
# not use this file except in compliance with the License. You may obtain
# a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations
# under the License.

"""
This module interfaces with dhall-lang
"""

import json
from pathlib import Path
from subprocess import Popen, PIPE
from typing import Any, Dict, Optional, Union

DEFAULT_PATH = Path("~/.local/bin/dhall-to-json").expanduser()
DEFAULT_URL = "https://github.com/dhall-lang/dhall-haskell/releases/download/"\
    "1.28.0/dhall-json-1.6.0-x86_64-linux.tar.bz2"
DEFAULT_HASH = \
    "b9917603fa58295f211dde384c073f8482343d445ad9bfab8919ae2eaca6bda7"

Input = Union[Path, str]
Env = Optional[Dict[str, str]]


def _install() -> None:
    # TODO: implement opt-out
    import urllib.request
    from hashlib import sha256

    req = urllib.request.urlopen(DEFAULT_URL)
    data = req.read()
    digest = sha256(data).hexdigest()
    if digest != DEFAULT_HASH:
        raise RuntimeError(
            f"{DEFAULT_URL}: expected '{DEFAULT_HASH}' got '{digest}")
    DEFAULT_PATH.parent.mkdir(parents=True, exist_ok=True)
    p = Popen(["tar", "-xjf", "-", "--strip-components=2",
               "-C", str(DEFAULT_PATH.parent)],
              stdin=PIPE)
    p.communicate(data)
    if p.wait():
        raise RuntimeError(f"{DEFAULT_URL}: couldn't extract")


def _load(input: Input, env: Env = None, debug: bool = False) -> Any:
    cmd = [str(DEFAULT_PATH if DEFAULT_PATH.exists() else "dhall-to-json")]
    if debug:
        cmd.append("--explain")
    if env and not env.get('PATH'):
        env['PATH'] = ':'.join(['/bin', '/usr/local/bin'])
    if isinstance(input, str):
        proc = Popen(
            cmd, stdin=PIPE, stdout=PIPE, stderr=PIPE, env=env)
        proc.stdin.write(input.encode('utf-8'))
    else:
        proc = Popen(
            cmd + ["--file", str(input)], stdout=PIPE, stderr=PIPE, env=env)
    stdout, stderr = proc.communicate()
    if stderr:
        raise RuntimeError(f"Dhall error:" + stderr.decode('utf-8'))
    return json.loads(stdout.decode('utf-8'))


def load(input: Input, env: Env = None, debug: bool = False) -> Any:
    try:
        return _load(input, env, debug)
    except FileNotFoundError:
        _install()
        return _load(input, env, debug)


if __name__ == "__main__":
    print(load('let x = "Hello dhall" in x'))
