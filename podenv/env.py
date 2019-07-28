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

from __future__ import annotations
import json
from abc import ABC, abstractmethod
from dataclasses import dataclass, field, fields
from pathlib import Path
from typing import Callable, Dict, List, Optional, Set, Tuple, Union


ExecArgs = List[str]
Requirements = List[str]
Info = Dict[str, Union[str, Requirements]]


class Runtime(ABC):
    System = {
        "rpm": {
            "commands": {
                "install": "dnf install -y ",
                "update": "dnf update -y ",
            }
        }
    }

    def __init__(self, metadataPath: Path):
        self.metadataPath = metadataPath
        # TODO: discover system type
        self.commands = self.System["rpm"]["commands"]
        self.info: Info = {}

    def updateInfo(self, info: Info) -> None:
        self.info.update(info)
        self.metadataPath.write_text(json.dumps(self.info))

    def loadInfo(self) -> None:
        self.info = json.loads(self.metadataPath.read_text())

    def exists(self) -> bool:
        return self.metadataPath.exists()

    @abstractmethod
    def getExecName(self) -> str:
        ...

    @abstractmethod
    def create(self) -> None:
        ...

    @abstractmethod
    def update(self) -> None:
        ...

    @abstractmethod
    def install(self, packages: Set[str]) -> None:
        ...


@dataclass
class ExecContext:
    environ: Dict[str, str] = field(default_factory=dict)
    mounts: Dict[Path, Path] = field(default_factory=dict)
    execArgs: ExecArgs = field(default_factory=list)
    home: Path = field(default_factory=Path)
    cwd: Path = field(default_factory=Path)

    def args(self, *args: str) -> None:
        self.execArgs.extend(args)


@dataclass
class Env:
    name: str
    image: str = ""
    rootfs: str = ""
    command: ExecArgs = field(default_factory=list)
    parent: str = ""
    environ: Dict[str, str] = field(default_factory=dict)
    capabilities: Dict[str, bool] = field(default_factory=dict)
    provides: Dict[str, str] = field(default_factory=dict)
    requires: Dict[str, str] = field(default_factory=dict)
    packages: List[str] = field(default_factory=list)

    # Internal attribute
    runtime: Optional[Runtime] = None
    ctx: ExecContext = field(default_factory=ExecContext)
    runDir: Optional[Path] = None
    autoUpdate: bool = False

    def applyParent(self, parentEnv: Env) -> None:
        for attr in fields(Env):
            if attr.name in ('name', 'parent'):
                continue
            attrValue = getattr(parentEnv, attr.name)
            if getattr(self, attr.name) in (None, "", [], {}):
                setattr(self, attr.name, attrValue)
            elif isinstance(attrValue, list):
                if attr.name == "command":
                    continue
                # List are extended
                setattr(self, attr.name, getattr(self, attr.name) +
                        getattr(parentEnv, attr.name))
            elif isinstance(attrValue, dict):
                # Dictionary are updated in reverse
                mergedDict = getattr(parentEnv, attr.name)
                mergedDict.update(getattr(self, attr.name))
                setattr(self, attr.name, mergedDict)

    def __post_init__(self) -> None:
        for cap in self.capabilities:
            if cap not in ValidCap:
                raise RuntimeError(f"{self.name}: unknown cap {cap}")


def rootCap(active: bool, ctx: ExecContext, _: Env) -> None:
    "run as root"
    if active:
        ctx.home = Path("/root")
        ctx.environ["XDG_RUNTIME_DIR"] = "/run/user/0"
        ctx.environ["HOME"] = "/root"
    else:
        ctx.home = Path("/home/user")
        ctx.environ["XDG_RUNTIME_DIR"] = "/run/user/1000"
        ctx.environ["HOME"] = "/home/user"
        ctx.args("--user", "1000")
    ctx.environ["HOME"] = str(ctx.home)


def uidmapCap(active: bool, ctx: ExecContext, _: Env) -> None:
    "map host uid"
    if active:
        ctx.args("--uidmap", "1000:0:1", "--uidmap", "0:1:999",
                 "--uidmap", "1001:1001:%s" % (2**16 - 1001))


def privilegedCap(active: bool, ctx: ExecContext, _: Env) -> None:
    "run as privileged container"
    if active:
        ctx.args("--privileged")


def terminalCap(active: bool, ctx: ExecContext, _: Env) -> None:
    "interactive mode"
    if active:
        ctx.args("-it")


def networkCap(active: bool, ctx: ExecContext, env: Env) -> None:
    "enable network"
    networkNamespace = None
    if env.requires.get("network"):
        networkNamespace = "container:net-" + env.requires["network"]
    elif env.provides.get("network"):
        networkNamespace = "container:net-" + env.provides["network"]

    if not active and not networkNamespace:
        networkNamespace = "none"

    if networkNamespace:
        ctx.args("--network", networkNamespace)


def mountCwdCap(active: bool, ctx: ExecContext, _: Env) -> None:
    "mount cwd to /data"
    if active:
        ctx.cwd = Path("/data")
        ctx.mounts[ctx.cwd] = Path()


def mountRunCap(active: bool, ctx: ExecContext, env: Env) -> None:
    "mount home and tmp to host tmpfs"
    if active:
        if env.runDir is None:
            raise RuntimeError("runDir isn't set")
        ctx.mounts[ctx.home] = env.runDir / "home"
        ctx.mounts[Path("/tmp")] = env.runDir / "tmp"


def autoUpdateCap(active: bool, _: ExecContext, env: Env) -> None:
    "keep environment updated"
    if active:
        env.autoUpdate = True


Capability = Callable[[bool, ExecContext, Env], None]
Capabilities: List[Tuple[str, Optional[str], Capability]] = [
    (func.__name__[:-3], func.__doc__, func) for func in [
        rootCap,
        uidmapCap,
        privilegedCap,
        terminalCap,
        networkCap,
        mountCwdCap,
        mountRunCap,
        autoUpdateCap,
    ]]
ValidCap: Set[str] = set([cap[0] for cap in Capabilities])


def prepareEnv(env: Env) -> Tuple[str, ExecArgs, ExecArgs]:
    """Generate podman exec args based on capabilities"""
    for name, _, capability in Capabilities:
        capability(env.capabilities.get(name, False), env.ctx, env)

    args = ["--hostname", env.name]
    if env.ctx.cwd == Path():
        env.ctx.cwd = env.ctx.home
    args.append("--workdir=" + str(env.ctx.cwd))

    for mount in sorted(env.ctx.mounts.keys()):
        args.extend(["-v", "{hostPath}:{containerPath}".format(
            hostPath=env.ctx.mounts[mount].expanduser().resolve(),
            containerPath=mount)])

    for e, v in sorted(env.ctx.environ.items()):
        args.extend(["-e", "%s=%s" % (e, v)])

    # Convenient default setting
    if not env.command:
        env.command = ["/bin/bash"]
        args.append("-it")

    return env.name, args + env.ctx.execArgs, env.command


def cleanupEnv(env: Env) -> None:
    ...
