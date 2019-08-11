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
import os
from abc import ABC, abstractmethod
from dataclasses import dataclass, field, fields
from pathlib import Path
from typing import Callable, Dict, List, Optional, Set, Tuple, Union
try:
    import selinux  # type: ignore
    HAS_SELINUX = True
except ImportError:
    HAS_SELINUX = False


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
        },
        "apt": {
            "commands": {
                "install": "apt-get install -y ",
                "update": "apt-get update -y "
            }
        },
    }

    def __init__(self, metadataPath: Path):
        self.metadataPath = metadataPath
        self.commands: Dict[str, str] = {}
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

    @abstractmethod
    def customize(self, commands: List[Tuple[str, str]]) -> None:
        ...


@dataclass
class ExecContext:
    """The intermediary podman context representation"""
    environ: Dict[str, str] = field(default_factory=dict)
    mounts: Dict[Path, Path] = field(default_factory=dict)
    syscaps: List[str] = field(default_factory=list)
    execArgs: ExecArgs = field(default_factory=list)
    home: Path = field(default_factory=Path)
    cwd: Path = field(default_factory=Path)
    xdgDir: Path = field(default_factory=Path)
    seLinuxLabel: str = ""
    seccomp: str = ""
    networkNamespace: str = ""

    def args(self, *args: str) -> None:
        self.execArgs.extend(args)

    def getArgs(self) -> ExecArgs:
        args = []

        if self.seLinuxLabel:
            args.extend(["--security-opt", f"label={self.seLinuxLabel}"])
        if self.seccomp:
            args.extend(["--security-opt", f"seccomp={self.seccomp}"])

        if self.cwd == Path():
            self.cwd = self.home
        args.extend(["--workdir", str(self.cwd)])

        for mount in sorted(self.mounts.keys()):
            args.extend(["-v", "{hostPath}:{containerPath}".format(
                hostPath=self.mounts[mount].expanduser().resolve(),
                containerPath=mount)])

        for cap in set(self.syscaps):
            args.extend(["--cap-add", cap])

        for e, v in sorted(self.environ.items()):
            args.extend(["-e", "%s=%s" % (e, v)])

        return args + self.execArgs


@dataclass
class Env:
    """The user provided container representation"""
    name: str = field(default="", metadata=dict(
        doc="The name of the environment"))
    parent: str = field(default="", metadata=dict(
        doc="A parent environment name to inherit attributes from."))
    image: str = field(default="", metadata=dict(
        doc="The container image reference"))
    rootfs: str = field(default="", metadata=dict(
        doc="The path of a rootfs"))
    dns: str = field(default="", metadata=dict(
        doc="A custom DNS server"))
    imageCustomizations: List[str] = field(default_factory=list, metadata=dict(
        doc="List of shell commands to execute and commit in the image"))
    packages: List[str] = field(default_factory=list, metadata=dict(
        doc="List of packages to be installed in the image"))
    command: ExecArgs = field(default_factory=list, metadata=dict(
        doc="Container starting command"))
    args: ExecArgs = field(default_factory=list, metadata=dict(
        doc="Optional arguments to append to the command"))
    environ: Dict[str, str] = field(default_factory=dict, metadata=dict(
        doc="User environ(7)"))
    syscaps: List[str] = field(default_factory=list, metadata=dict(
        doc="List of system capabilities(7)"))
    mounts: Dict[str, str] = field(default_factory=dict, metadata=dict(
        doc="Extra mountpoints"))
    capabilities: Dict[str, bool] = field(default_factory=dict, metadata=dict(
        doc="List of capabilities"))
    provides: Dict[str, str] = field(default_factory=dict, metadata=dict(
        doc="List of objects the environment provides"))
    requires: Dict[str, str] = field(default_factory=dict, metadata=dict(
        doc="List of objects the environment requires"))
    overlays: List[str] = field(default_factory=list, metadata=dict(
        doc="List of overlay to copy in runtime directory"))
    home: str = field(default="", metadata=dict(
        doc="Container home path mount"))
    shmsize: str = field(default="", metadata=dict(
        doc="The shm-size value string"))

    # Internal attribute
    runtime: Optional[Runtime] = field(default=None, metadata=dict(
        internal=True))
    ctx: ExecContext = field(default_factory=ExecContext, metadata=dict(
        internal=True))
    runDir: Optional[Path] = field(default=None, metadata=dict(
        internal=True))
    overlaysDir: Optional[Path] = field(default=None, metadata=dict(
        internal=True))
    autoUpdate: bool = field(default=False, metadata=dict(
        internal=True))

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
        ctx.xdgDir = Path("/run/user/0")
    else:
        ctx.home = Path("/home/user")
        ctx.xdgDir = Path("/run/user/{uid}".format(uid=os.geteuid()))
        ctx.args("--user", "{uid}".format(uid=os.geteuid()))
    ctx.environ["XDG_RUNTIME_DIR"] = str(ctx.xdgDir)
    ctx.environ["HOME"] = str(ctx.home)


def getUidMap(_: Env) -> ExecArgs:
    return ["--uidmap", "1000:0:1", "--uidmap", "0:1:999",
            "--uidmap", "1001:1001:%s" % (2**16 - 1001)]


def uidmapCap(active: bool, ctx: ExecContext, env: Env) -> None:
    "map host uid"
    if active:
        ctx.execArgs.extend(getUidMap(env))


def privilegedCap(active: bool, ctx: ExecContext, _: Env) -> None:
    "run as privileged container"
    if active:
        ctx.args("--privileged")


def terminalCap(active: bool, ctx: ExecContext, _: Env) -> None:
    "interactive mode"
    if active:
        ctx.args("-it")
        ctx.args("--detach-keys", "ctrl-e,e")


def networkCap(active: bool, ctx: ExecContext, env: Env) -> None:
    "enable network"
    if env.requires.get("network"):
        ctx.networkNamespace = "container:net-" + env.requires["network"]
    elif env.provides.get("network"):
        ctx.networkNamespace = "container:net-" + env.provides["network"]

    if not active and not ctx.networkNamespace:
        ctx.networkNamespace = "none"

    if ctx.networkNamespace:
        ctx.args("--network", ctx.networkNamespace)


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
        if not ctx.mounts.get(ctx.home) and not env.home:
            ctx.mounts[ctx.home] = env.runDir / "home"
        if not ctx.mounts.get(Path("/tmp")):
            ctx.mounts[Path("/tmp")] = env.runDir / "tmp"


def ipcCap(active: bool, ctx: ExecContext, env: Env) -> None:
    "share host ipc"
    if active:
        ctx.args("--ipc=host")


def x11Cap(active: bool, ctx: ExecContext, env: Env) -> None:
    "share x11 socket"
    if active:
        ctx.mounts[Path("/tmp/.X11-unix")] = Path("/tmp/.X11-unix")
        ctx.environ["DISPLAY"] = ":0"


def pulseaudioCap(active: bool, ctx: ExecContext, env: Env) -> None:
    "share pulseaudio socket"
    if active:
        ctx.mounts[Path("/etc/machine-id:ro")] = Path("/etc/machine-id")
        ctx.mounts[ctx.xdgDir / "pulse"] = \
            Path(ctx.environ["XDG_RUNTIME_DIR"]) / "pulse"


def sshCap(active: bool, ctx: ExecContext, env: Env) -> None:
    "share ssh agent and keys"
    if active:
        ctx.environ["SSH_AUTH_SOCK"] = os.environ["SSH_AUTH_SOCK"]
        sshSockPath = Path(os.environ["SSH_AUTH_SOCK"])
        ctx.mounts[Path(sshSockPath)] = sshSockPath
        ctx.mounts[ctx.home / ".ssh"] = Path("~/.ssh")


def gpgCap(active: bool, ctx: ExecContext, env: Env) -> None:
    "share gpg agent"
    if active:
        gpgSockDir = Path(os.environ["XDG_RUNTIME_DIR"]) / "gnupg"
        ctx.mounts[ctx.xdgDir / "gnupg"] = gpgSockDir
        ctx.mounts[ctx.home / ".gnupg"] = Path("~/.gnupg")


def webcamCap(active: bool, ctx: ExecContext, env: Env) -> None:
    "share webcam device"
    if active:
        ctx.args("--device", "/dev/video0")


def driCap(active: bool, ctx: ExecContext, env: Env) -> None:
    "share graphic device"
    if active:
        ctx.args("--device", "/dev/dri")


def tunCap(active: bool, ctx: ExecContext, env: Env) -> None:
    "share tun device"
    if active:
        ctx.args("--device", "/dev/net/tun")


def selinuxCap(active: bool, ctx: ExecContext, env: Env) -> None:
    "enable SELinux"
    if not active:
        ctx.seLinuxLabel = "disable"


def seccompCap(active: bool, ctx: ExecContext, env: Env) -> None:
    "enable seccomp"
    if not active:
        ctx.seccomp = "unconfined"


def ptraceCap(active: bool, ctx: ExecContext, env: Env) -> None:
    "enable ptrace"
    if active:
        ctx.syscaps.append("SYS_PTRACE")


def setuidCap(active: bool, ctx: ExecContext, env: Env) -> None:
    "enable setuid"
    if active:
        for cap in ("SETUID", "SETGID"):
            ctx.syscaps.append(cap)


def autoUpdateCap(active: bool, _: ExecContext, env: Env) -> None:
    "keep environment updated"
    if active:
        env.autoUpdate = True


Capability = Callable[[bool, ExecContext, Env], None]
Capabilities: List[Tuple[str, Optional[str], Capability]] = [
    (func.__name__[:-3], func.__doc__, func) for func in [
        rootCap,
        privilegedCap,
        terminalCap,
        ipcCap,
        x11Cap,
        pulseaudioCap,
        sshCap,
        gpgCap,
        webcamCap,
        driCap,
        tunCap,
        seccompCap,
        selinuxCap,
        setuidCap,
        ptraceCap,
        networkCap,
        mountCwdCap,
        mountRunCap,
        autoUpdateCap,
        uidmapCap,
    ]]
ValidCap: Set[str] = set([cap[0] for cap in Capabilities])


def validateEnv(env: Env) -> None:
    """Sanity check and warn user about missing setting"""
    def warn(msg: str) -> None:
        print(f"\033[93m{msg}\033[m")

    # Check if SELinux will block socket access
    if env.capabilities.get("selinux"):
        for cap in ("x11", "tun"):
            if env.capabilities.get(cap):
                warn(
                    f"SELinux is disabled because capability '{cap}' need "
                    "extra type enforcement that are not currently supported.")
                selinuxCap(False, env.ctx, env)

    # Check for uid permissions
    if not env.capabilities.get("root") and not env.capabilities.get("uidmap"):
        for cap in ("x11", "pulseaudio", "ssh", "gpg"):
            if env.capabilities.get(cap):
                warn(
                    f"UIDMap is required because '{cap}' need "
                    "DAC access to the host file")
                uidmapCap(True, env.ctx, env)
                break

    # Check for system capabilities
    if env.capabilities.get("tun") and "NET_ADMIN" not in env.ctx.syscaps:
        warn(f"NET_ADMIN capability is needed by the tun device")
        env.ctx.syscaps.append("NET_ADMIN")

    # Check mount points labels
    if env.capabilities.get("selinux") and HAS_SELINUX:
        label = "container_file_t"
        for hostPath in env.ctx.mounts.values():
            hostPath = hostPath.expanduser().resolve()
            if hostPath.exists() and \
               selinux.getfilecon(str(hostPath))[1].split(':')[2] != label:
                warn(f"SELinux is disabled because {hostPath} doesn't have "
                     f"the {label} label.")
                selinuxCap(False, env.ctx, env)

    # Check mount points permissions
    for hostPath in env.ctx.mounts.values():
        hostPath = hostPath.expanduser().resolve()
        if hostPath.exists() and not os.access(str(hostPath), os.R_OK):
            warn(f"{hostPath} is not readable by the current user.")

    # Check for home mount point
    if env.overlays and not env.ctx.mounts.get(env.ctx.home):
        warn(f"overlay needs a home mount point, "
             "mountRun capability is enabled.")
        mountRunCap(True, env.ctx, env)


def prepareEnv(env: Env, cliArgs: List[str]) -> Tuple[str, ExecArgs, ExecArgs]:
    """Generate podman exec args based on capabilities"""
    # Apply capabilities
    for name, _, capability in Capabilities:
        capability(env.capabilities.get(name, False), env.ctx, env)

    # Apply extra settings from the environment definition:
    args = ["--hostname", env.name]
    if env.dns and "--network" not in env.ctx.execArgs:
        args.append(f"--dns={env.dns}")
    if env.shmsize:
        args.append(f"--shm-size={env.shmsize}")
    if env.home:
        env.ctx.mounts[env.ctx.home] = Path(env.home).expanduser().resolve()

    env.ctx.syscaps.extend(env.syscaps)
    env.ctx.environ.update(env.environ)

    for containerPath, hostPath in env.mounts.items():
        if containerPath.startswith("~/"):
            env.ctx.mounts[env.ctx.home / containerPath[2:]] = Path(hostPath)
        else:
            env.ctx.mounts[Path(containerPath)] = Path(hostPath)

    # Look for file argument requirement
    fileArg: Optional[Path] = None
    if "$1" in env.command:
        if len(cliArgs) > 1:
            raise RuntimeError("Multiple file input %s" % cliArgs)
        if cliArgs:
            fileArg = Path(cliArgs[0]).expanduser().resolve(strict=True)
            env.ctx.mounts[Path("/tmp") / fileArg.name] = fileArg

    commandArgs: List[str] = []
    for command in env.command:
        if command == "$@" and cliArgs:
            commandArgs += cliArgs
        elif command == "$1" and fileArg:
            commandArgs.append("/tmp/" + fileArg.name)
        else:
            commandArgs.append(command)

    # Only use cli args when env isn't a shell
    if not commandArgs or commandArgs[-1] != "/bin/bash":
        for arg in env.args + cliArgs:
            commandArgs.append(arg)

    # Sanity checks
    validateEnv(env)

    # OCI doesn't let you join a netns without the userns when using uidmap...
    if env.capabilities.get("uidmap") and env.ctx.networkNamespace.startswith(
            "container:"):
        env.ctx.args("--userns", env.ctx.networkNamespace)

    return env.name, args + env.ctx.getArgs(), list(map(str, commandArgs))


def cleanupEnv(env: Env) -> None:
    ...
