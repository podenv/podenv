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
This module handles the configuration schema.
"""

from __future__ import annotations
from os import environ
from pathlib import Path
from typing import Any, Dict, Union
from yaml import safe_load

from podenv.dhall import load as dhall_load
from podenv.env import Env, loadEnv


defaultConfig = Path("~/.config/podenv/config.dhall")
localConf = Path("default.podenv")


def loadDhallConfig(configFile: Union[str, Path]) -> Any:
    def pathEnv(env: str, default: str) -> Path:
        return Path(default if environ.get(env) is None else
                    environ[env]).expanduser()

    hub = pathEnv(
        "PODENV_HUB", "~/git/github.com/podenv/hub/package.dhall")
    prelude = pathEnv(
        "PODENV_PRELUDE",
        "~/git/github.com/podenv/podenv/podenv/dhall/package.dhall")
    if not prelude.exists():
        prelude = Path(__file__).parent / "dhall" / "package.dhall"
    try:
        dhallPackage = dict(
            HOME=environ["HOME"],
            PODENV_HUB=str(hub),
            PODENV_PRELUDE=str(prelude),
        )
    except KeyError:
        raise RuntimeError("HOME environment variable is required")
    return dhall_load(configFile, dhallPackage)


def transformSchema(schema: Any) -> Dict[str, Env]:
    """Transform input schema"""
    # normalize env list to a map
    envs: Dict[str, Env] = {}
    if isinstance(schema, list):
        for env in schema:
            envs[env['name']] = loadEnv(env)
    else:
        envs[schema['name']] = loadEnv(schema)
    return envs


def loadConfig(skipLocal: bool = False,
               configStr: str = "",
               configFile: Path = defaultConfig) -> Dict[str, Env]:
    # Resolve config location
    config: Union[str, Path]
    if configStr:
        config = configStr
    elif not skipLocal and localConf.exists():
        config = localConf
    elif configFile != defaultConfig:
        config = configFile.expanduser()
        if not config.exists():
            raise RuntimeError(f"{configFile}: No such file or directory")
    else:
        configFile = defaultConfig.expanduser()
        if not configFile.exists():
            configFile.parent.mkdir(parents=True, exist_ok=True)
            configFile.write_text("(env:PODENV_HUB).Defaults\n")
        config = configFile

    # Load config schema
    schema: Any
    if isinstance(config, Path) and config.name.endswith(".yaml"):
        schema = safe_load(config.read_text())
    else:
        schema = loadDhallConfig(config)

    return transformSchema(schema)


def getEnv(conf: Dict[str, Env], envName: str) -> Env:
    try:
        variant = envName.split(".")
        if len(variant) == 2:
            envName = variant[-1]
            variantName = variant[0]
        env: Env = conf[envName]
        if len(variant) == 2:
            env.name = f"{variantName}-{envName}"
    except KeyError:
        raise RuntimeError(f"{envName}: couldn't load the environment")
    return env
