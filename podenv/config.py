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
This module handles configuration schema.
"""

from __future__ import annotations
from pathlib import Path
from textwrap import dedent
from typing import Any, Dict, List, Optional
from yaml import safe_load

from podenv.env import Env


class Config:
    def __init__(self, schema: Dict[str, Any]) -> None:
        self.dns: Optional[str] = schema.get('system', {}).get('dns')
        self.default: str = schema.get('system', {}).get('defaultEnv', 'shell')
        self.envs: Dict[str, Env] = {}
        self.overlaysDir: Optional[Path] = None
        for envName, envSchema in schema.get('environments', {}).items():
            self.envs[envName] = Env(envName, **envSchema)


def initConfig(configDir: Path, configFile: Path) -> None:
    defaultConfig = dedent("""\
        # Podenv configuration file
        ---
        # Site local variable
        system:
          # Set IP of local resolver when using dnsmasq
          dns: null
          defaultEnv: shell

        environments:
          # Default environment
          base:
            image: registry.fedoraproject.org/fedora:30
          shell:
            command: ["/bin/bash"]
        """)
    configDir.mkdir()
    configFile.write_text(defaultConfig)


def initOverlays(overlayDir: Path) -> None:
    overlayDir.mkdir()
    bashOverlay = overlayDir / "bash"
    bashOverlay.mkdir()
    (bashOverlay / ".bashrc").write_text(dedent(r"""\
        if [ -f /etc/bashrc ]; then
            . /etc/bashrc
        fi
        export PS1="\[\033[01;32m\]\h \[\033[01;34m\]\w \$ \[\033[00m\]"
        alias ls='ls -ap --color=auto'
    """))


def loadConfig(configDir: Path = Path("~/.config/podenv")) -> Config:
    configDir = configDir.expanduser()
    configFile = configDir / "config.yaml"
    if not configDir.exists():
        initConfig(configDir, configFile)
    conf = Config(safe_load(configFile.read_text()))
    conf.overlaysDir = configDir / "overlay"
    if not conf.overlaysDir.exists():
        initOverlays(conf.overlaysDir)
    # Look for local conf
    localConf = Path("default.podenv")
    if localConf.exists():
        # Create profile name
        envName = Path().resolve().name
        conf.envs[envName] = Env(envName, **safe_load(localConf.read_text()))
        conf.default = envName
    return conf


def loadEnv(conf: Config, envName: Optional[str]) -> Env:
    if not envName:
        envName = conf.default

    def resolvParents(parent: Optional[str], history: List[str]) -> None:
        if not parent:
            if "base" in history:
                return
            parent = "base"
        if parent in history:
            raise RuntimeError("Circular dependencies detected %s in %s" % (
                parent, history))
        parentEnv = conf.envs[parent]
        resolvParents(parentEnv.parent, history + [parent])
        env.applyParent(parentEnv)

    try:
        variant = envName.split(".")
        if len(variant) == 2:
            envName = variant[-1]
            variantName = variant[0]
        env: Env = conf.envs[envName]
        if len(variant) == 2:
            env.name = f"{variantName}-{envName}"
        resolvParents(env.parent, [])
    except KeyError:
        raise RuntimeError(f"{envName}: couldn't find environment")

    env.overlaysDir = conf.overlaysDir
    env.runDir = Path("/tmp/podenv") / env.name
    if conf.dns:
        env.dns = conf.dns
    return env
