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
from logging import getLogger
from pathlib import Path
from textwrap import dedent
from typing import Any, Dict, List, Optional
from yaml import safe_load

from podenv.pod import canUpdate, execute, outdated
from podenv.env import Env, UserNotif
from podenv import defaults


log = getLogger("podenv")
podhub = "https://github.com/podenv/hub"


def urlToFilename(url: str) -> str:
    return url.replace('https://', '').replace('http://', '').replace(
        '/', ':').replace('.', '-')


def attributesToCamelCase(envSchema: Dict[Any, Any]) -> Dict[Any, Any]:
    """Convert attribute from yaml to dataclass camelCase"""
    for hyphenKey in (
            "image-customizations",
            "image-tasks",
            "system-type",
            "pre-tasks",
            "post-tasks",
    ):
        if hyphenKey in envSchema:
            words = hyphenKey.split('-')
            camelCase = words[0] + words[1].capitalize()
            envSchema[camelCase] = envSchema.pop(hyphenKey)
    return envSchema


def askConfirmation(msg: str) -> bool:
    print(msg + " [Yn]: ", end='')
    try:
        return input().strip().lower() in ("", "y", "yes")
    except KeyboardInterrupt:
        print()
        return False


def syncRegistry(url: str, localPath: Path) -> None:
    if not any(map(lambda scheme: url.startswith(f"{scheme}://"),
                   ("https", "file"))):
        raise RuntimeError(f"Can't sync registry over insecure channel {url}")
    try:
        if not (localPath / ".git").exists():
            if url.startswith("https://") and not askConfirmation(
                    f"Press enter to sync registry {url}"):
                return
            localPath.mkdir(parents=True, exist_ok=True)
            log.info("Cloning %s", url)
            execute(["git", "clone", url, str(localPath)])
        else:
            log.info("Updating %s", url)
            execute(["git", "pull"], cwd=localPath)
        (localPath / ".git").touch()
        # TODO: Validate signature
    except RuntimeError:
        log.warning(f"Couldn't update {url}")


class Config:
    def __init__(self, configFile: Path) -> None:
        schema: Dict[str, Any] = safe_load(configFile.read_text())
        schema.setdefault('system', {})
        self.dns: Optional[str] = schema['system'].get('dns')
        self.default: str = schema['system'].get('default-env', 'shell')
        self.defaultCap: Dict[str, bool] = schema['system'].get(
            'default-capabilities', dict(selinux=True, seccomp=True))
        self.envs: Dict[str, Env] = {}
        self.overlaysDir: Optional[Path] = None
        self.loadEnvs(
            defaults.environments, Path(defaults.__file__), "internal")
        distConfig = Path("/usr/share/podenv/config.yaml")
        if distConfig.exists():
            self.loadEnvs(safe_load(
                distConfig.read_text()), distConfig, "included")
        for registry in schema.get("system", {}).get("registries", []):
            localPath = (configFile.parent / "registries" /
                         urlToFilename(registry))
            extraConfigFile = localPath / "config.yaml"
            if canUpdate() and (not extraConfigFile.exists() or
                                outdated(localPath / ".git")):
                syncRegistry(registry, localPath)
            if extraConfigFile.exists():
                self.loadEnvs(
                    safe_load(extraConfigFile.read_text()),
                    extraConfigFile,
                    registry)

        self.loadEnvs(schema.get('environments', {}), configFile, "local")

    def loadEnvs(
            self,
            schema: Dict[str, Any],
            configFile: Path,
            registryName: str) -> None:
        for envName, envSchema in schema.items():
            if "." in envName:
                raise RuntimeError(
                    f"'.' is not allowed in environment name {envName}")
            self.envs[envName] = Env(
                envName,
                configFile=configFile,
                registryName=registryName,
                **attributesToCamelCase(envSchema))


def initConfig(configDir: Path, configFile: Path) -> None:
    ps1 = "\\[\\033[01;32m\\]\\h \\[\\033[01;34m\\]\\w \\$ \\[\\033[00m\\]"
    defaultConfig = dedent("""\
        # Podenv configuration file
        ---
        # Site local variable
        system:
          # Set IP of local resolver when using dnsmasq
          dns: null
          default-env: shell
          default-capabilities:
            seccomp: True
            selinux: True
          registries:
            - {podhub}

        environments:
          # Local environments
          shell:
            command: ["/bin/bash"]
            parent: fedora
            capabilities:
              terminal: True
              mount-run: True
            environ:
              SHLVL: 3
              TERM: xterm
            overlays:
              - .bashrc: |
                  if [ -f /etc/bashrc ]; then
                    . /etc/bashrc
                  fi
                  export PS1="{ps1}"
                  alias ls='ls -ap --color=auto'
        """.format(ps1=ps1, podhub=podhub))
    configDir.mkdir(parents=True, exist_ok=True)
    configFile.write_text(defaultConfig)


def initOverlays(overlayDir: Path) -> None:
    overlayDir.mkdir()
    bashOverlay = overlayDir / "bash"
    bashOverlay.mkdir()
    (bashOverlay / ".bashrc").write_text(dedent(r"""
        if [ -f /etc/bashrc ]; then
            . /etc/bashrc
        fi
        export PS1="\[\033[01;32m\]\h \[\033[01;34m\]\w \$ \[\033[00m\]"
        alias ls='ls -ap --color=auto'
    """))


def loadConfig(
        userNotif: UserNotif,
        skipLocal: bool = False,
        configDir: Path = Path("~/.config/podenv")) -> Config:
    configDir = configDir.expanduser()
    configFile = configDir / "config.yaml"
    if not configFile.exists():
        initConfig(configDir, configFile)
    conf = Config(configFile)
    conf.overlaysDir = configDir / "overlay"
    if not conf.overlaysDir.exists():
        initOverlays(conf.overlaysDir)
    # Look for local conf
    localConf = Path("default.podenv")
    if not skipLocal and localConf.exists():
        # Create profile name
        envName = Path().resolve().name
        conf.envs[envName] = Env(
            envName,
            configFile=localConf,
            registryName="local-file",
            **attributesToCamelCase(safe_load(localConf.read_text())))
        conf.default = envName
        userNotif(f"Using ./{localConf} for {envName}")
    return conf


def loadEnv(
        conf: Config, envName: Optional[str], baseName: Optional[str]) -> Env:
    if not envName:
        envName = conf.default

    def resolvParents(parent: Optional[str], history: List[str]) -> None:
        if not parent and (baseName and baseName not in history):
            parent = baseName
        if not parent:
            return
        if parent in history:
            raise RuntimeError("Circular dependencies detected %s in %s" % (
                parent, history))
        if parent not in conf.envs:
            log.error(
                "%s: parent does not exist (history: %s)", parent, history)
        parentEnv = conf.envs[parent]
        if not parentEnv.parent and baseName:
            # Override last parent with base
            if baseName not in conf.envs:
                log.error("%s: parent does not exist", baseName)
            parentEnv = conf.envs[baseName]
            parent = baseName
        env.applyParent(parentEnv)
        resolvParents(parentEnv.parent, history + [parent])

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
        raise RuntimeError(f"{envName}: couldn't load the environment")

    # Apply default cap
    for capName, capValue in conf.defaultCap.items():
        if capName not in env.capabilities:
            env.capabilities[capName] = capValue

    env.overlaysDir = conf.overlaysDir
    env.runDir = Path("/tmp/podenv") / env.name
    if conf.dns:
        env.dns = conf.dns
    return env
