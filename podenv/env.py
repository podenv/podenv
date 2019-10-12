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
This module handles environment definitions.
"""

from __future__ import annotations
import base64
import copy
import os
import re
import shlex
from abc import ABC, abstractmethod
from dataclasses import dataclass, field, fields
from pathlib import Path
from textwrap import dedent
from typing import Callable, Dict, List, Optional, Set, Tuple, Union
try:
    import selinux  # type: ignore
    HAS_SELINUX = True
except ImportError:
    HAS_SELINUX = False


ExecArgs = List[str]
Requirements = List[str]
Info = Dict[str, Union[str, Requirements]]
Overlay = Union[str, Dict[str, str]]
UserNotif = Callable[[str], None]
Task = Dict[str, Union[str, Dict[str, str]]]
StrOrList = Union[str, List[str]]


def asList(param: StrOrList = []) -> List[str]:
    if isinstance(param, list):
        return param
    return shlex.split(param)


def pipFilter(packages: Set[str]) -> Set[str]:
    return set(filter(lambda x: x.startswith("pip:"), packages))


def packageFilter(packages: Set[str]) -> Set[str]:
    return packages.difference(pipFilter(packages))


def taskToCommand(task: Task) -> str:
    """Convert ansible like task to shell string"""
    task = copy.copy(task)
    command = []
    whenCondition: Union[None, str, Dict[str, str]] = None
    if task.get("delegate_to"):
        if task["delegate_to"] != "host":
            raise RuntimeError(f"Task delegate_to is incorrect: {task}")
        command.append("run_local_%s" % task.pop("delegate_to"))
    if task.get("when") or task.get("unless"):
        if task.get("unless"):
            whenCondition = "! %s" % task.pop("unless")
        else:
            whenCondition = task.pop("when")
        if not isinstance(whenCondition, str):
            raise RuntimeError(f"Invalid when condition {whenCondition}")
        command.append(f"if {whenCondition}; then true")

    if task.get("name"):
        if "'" in task["name"]:
            raise RuntimeError(f"Task name can't have ': {task['name']}")
        command.append("echo '%s'" % task.pop("name"))
    if task.get("block"):
        block = task.pop("block")
        if isinstance(block, list):
            for blockTask in map(taskToCommand, block):
                command.append(blockTask)
        else:
            raise RuntimeError(f"Invalid block task {block}")
    elif task.get("command") or task.get("shell"):
        if task.get("command"):
            cmd = task.pop("command")
        else:
            cmd = task.pop("shell")
        command.append(str(cmd).rstrip('\n'))
    elif task.get("copy"):
        copyTask = task.pop("copy")
        if not isinstance(copyTask, dict):
            raise RuntimeError(f"Invalid copy task {copyTask}")
        copyContent = copyTask.pop("content")
        copyDest = copyTask.pop("dest")
        if copyTask:
            raise RuntimeError(f"Unsupported copy attribute: {copyTask}")
        command.append("echo {b64content} | base64 -d > {dest}".format(
            b64content=base64.b64encode(
                copyContent.encode('utf-8')).decode('ascii'),
            dest=copyDest))
    else:
        raise RuntimeError(f"Unsupported task: {task}")
    if task:
        raise RuntimeError(f"Unsupported task attribute: {task}")
    if whenCondition:
        command.append("fi")
    return "; ".join(command)


def safeFormat(string: str, variables: Dict[str, str]) -> str:
    """Implement python str.format that supports foreign keys"""
    result = string
    for k, v in variables.items():
        result = result.replace("{%s}" % k, v)
    return result


def dictFormat(d: Dict[str, str], vars: Dict[str, str]) -> Dict[str, str]:
    """Format a dictionary keys anv values"""
    return dict(map(lambda x:
                    (safeFormat(str(x[0]), vars),
                     safeFormat(str(x[1]), vars)),
                    d.items()))


class Runtime(ABC):
    @abstractmethod
    def exists(self, autoUpdate: bool) -> bool:
        ...

    @abstractmethod
    def loadInfo(self) -> None:
        ...

    @abstractmethod
    def getCustomizations(self) -> List[str]:
        ...

    @abstractmethod
    def getInstalledPackages(self) -> List[str]:
        ...

    @abstractmethod
    def enableExtra(self, extra: str, env: Env) -> None:
        ...

    @abstractmethod
    def getInstalledExtra(self, extra: str) -> Set[str]:
        ...

    @abstractmethod
    def getRuntimeArgs(self) -> ExecArgs:
        ...

    @abstractmethod
    def getSystemMounts(self, withTmp: bool) -> ExecArgs:
        ...

    @abstractmethod
    def setSystemType(self, systemType: str) -> None:
        ...

    @abstractmethod
    def needUpdate(self) -> bool:
        ...

    @abstractmethod
    def getExecName(self) -> ExecArgs:
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
    def updateExtra(self, extra: str) -> None:
        ...

    @abstractmethod
    def installExtra(self, extra: str, packages: Set[str]) -> None:
        ...

    @abstractmethod
    def customize(self, commands: List[Tuple[str, str]]) -> None:
        ...


@dataclass
class VolumeInfo:
    """A volume information"""
    name: str = ""
    volumeName: str = ""
    readOnly: bool = field(default=False)

    def getContainerPath(self) -> Path:
        if self.name == "home":
            return Path("~/")
        elif self.name == "git":
            return Path("~/git")
        elif self.name.startswith("lib-"):
            return Path("/var/lib") / self.name.split("lib-", 1)[-1]
        elif self.name.startswith("cache-"):
            return Path("~/.cache") / self.name.split("cache-", 1)[-1]
        elif self.name.startswith("config-"):
            return Path("~/.config") / self.name.split("config-", 1)[-1]
        else:
            raise RuntimeError(f"Unknown volume {self.name}")


Volume = Union[str, VolumeInfo]


@dataclass
class ExecContext:
    """The intermediary podman context representation"""
    environ: Dict[str, str] = field(default_factory=dict)
    mounts: Dict[Path, Union[Path, VolumeInfo]] = field(default_factory=dict)
    syscaps: List[str] = field(default_factory=list)
    sysctls: List[str] = field(default_factory=list)
    devices: List[Path] = field(default_factory=list)
    uidmaps: List[str] = field(default_factory=list)
    home: Path = field(default_factory=Path)
    cwd: Path = field(default_factory=Path)
    xdgDir: Path = field(default_factory=Path)
    addHosts: Dict[str, str] = field(default_factory=dict)
    detachKeys: str = ""
    interactive: bool = False
    user: int = 0
    privileged: bool = False
    seLinuxLabel: str = ""
    seccomp: str = ""
    namespaces: Dict[str, str] = field(default_factory=dict)

    def hasNetwork(self) -> bool:
        return not self.namespaces.get("network") or \
            self.namespaces["network"] != "none"

    def hasDirectNetwork(self) -> bool:
        return not self.namespaces.get("network") or \
            self.namespaces["network"] == "host"

    def getHosts(self) -> ExecArgs:
        args = []
        for hostName, hostIp in self.addHosts.items():
            args.extend(["--add-host", f"{hostName}:{hostIp}"])
        return args

    def getArgs(self) -> ExecArgs:
        args = []

        if self.seLinuxLabel:
            args.extend(["--security-opt", f"label={self.seLinuxLabel}"])
        if self.seccomp:
            args.extend(["--security-opt", f"seccomp={self.seccomp}"])

        for ns, val in self.namespaces.items():
            args.extend([f"--{ns}", val])

        if self.hasDirectNetwork():
            args.extend(self.getHosts())

        if self.cwd == Path():
            self.cwd = self.home
        args.extend(["--workdir", str(self.cwd)])

        for mount in sorted(self.mounts.keys()):
            hostMount = self.mounts[mount]
            if isinstance(hostMount, Path):
                hostPath = str(hostMount.expanduser().resolve())
            else:
                hostPath = f"{hostMount.volumeName}"
            args.extend(["-v", "{hostPath}:{containerPath}".format(
                hostPath=hostPath,
                containerPath=mount)])

        for device in self.devices:
            args.extend(["--device", str(device)])

        for cap in set(self.syscaps):
            args.extend(["--cap-add", cap])

        for ctl in set(self.sysctls):
            args.extend(["--sysctl", ctl])

        for e, v in sorted(self.environ.items()):
            args.extend(["-e", "%s=%s" % (e, v)])

        if self.user:
            args.extend(["--user", "%d" % self.user])

        args.extend(self.uidmaps)

        if self.privileged:
            args.extend(["--privileged"])

        if self.detachKeys:
            args.extend(["--detach-keys", self.detachKeys])

        if self.interactive:
            args.extend(["-it"])

        return args


@dataclass
class DesktopEntry:
    """A desktop file definition"""
    envName: str
    relPath: Path
    terminal: bool
    name: str = field(default="")
    icon: str = field(default="")

    def format(self) -> str:
        if self.icon:
            if (self.relPath / self.icon).exists():
                iconPath = (self.relPath / self.icon).expanduser().resolve()
            elif Path(self.icon).exists():
                iconPath = Path(self.icon).expanduser().resolve()
            else:
                iconPath = Path(self.icon)
            icon = f"Icon={iconPath}"
        else:
            icon = ""
        terminal = "true" if self.terminal else "false"

        return dedent(f"""            # Generated by podenv
            [Desktop Entry]
            Type=Application
            Name={self.name}
            Comment=Podenv launcher for {self.envName}
            Exec=podenv {self.envName}
            Terminal={terminal}
            {icon}
        """)

    def __post_init__(self) -> None:
        if not self.name:
            self.name = f"podenv - {self.envName}"


@dataclass
class Env:
    """The user provided container representation"""
    name: str = field(default="", metadata=dict(
        doc="The name of the environment"))
    description: Optional[str] = field(default="", metadata=dict(
        doc="Environment description"))
    url: Optional[str] = field(default="", metadata=dict(
        doc="Application home page"))
    parent: str = field(default="", metadata=dict(
        doc="A parent environment name to inherit attributes from"))
    abstract: bool = field(default=False, metadata=dict(
        doc="Set to True to indicate the environment can't be used directly"))
    desktop: Optional[DesktopEntry] = field(default=None, metadata=dict(
        doc="A desktop launcher entry file definition"))
    image: str = field(default="", metadata=dict(
        doc="The container image reference"))
    rootfs: str = field(default="", metadata=dict(
        doc="The path of a rootfs"))
    packages: List[str] = field(default_factory=list, metadata=dict(
        doc="List of packages to be installed in the image"))
    imageTasks: List[Task] = field(default_factory=list, metadata=dict(
        doc="List of ansible like command to commit to the image"))
    preTasks: List[Task] = field(default_factory=list, metadata=dict(
        doc="List of ansible like command to run before the command"))
    postTasks: List[Task] = field(default_factory=list, metadata=dict(
        doc="List of ansible like command to run after the pod exited"))
    command: ExecArgs = field(default_factory=list, metadata=dict(
        doc="Container starting command"))
    args: ExecArgs = field(default_factory=list, metadata=dict(
        doc="Optional arguments to append to the command"))
    environ: Dict[str, str] = field(default_factory=dict, metadata=dict(
        doc="User environ(7)"))
    vars: Dict[str, str] = field(default_factory=dict, metadata=dict(
        doc="Extra environ vars to be used for command substitution only"))
    syscaps: List[str] = field(default_factory=list, metadata=dict(
        doc="List of system capabilities(7)"))
    sysctls: List[str] = field(default_factory=list, metadata=dict(
        doc="List of sysctl(8)"))
    volumes: Dict[str, Volume] = field(default_factory=dict, metadata=dict(
        doc="List of volumes"))
    mounts: Dict[str, Optional[str]] = field(
        default_factory=dict, metadata=dict(doc="Extra mountpoints"))
    capabilities: Dict[str, bool] = field(default_factory=dict, metadata=dict(
        doc="List of capabilities"))
    network: str = field(default="", metadata=dict(
        doc="Name of a network to be shared by multiple environment"))
    addHosts: Dict[str, str] = field(default_factory=dict, metadata=dict(
        doc="Custom hostname,ip to configure in the container"))
    requires: StrOrList = field(default_factory=asList, metadata=dict(
        doc="List of required environments"))
    overlays: List[Overlay] = field(default_factory=list, metadata=dict(
        doc="List of overlay to copy in runtime directory"))
    home: str = field(default="", metadata=dict(
        doc="Container home path mount"))
    shmsize: str = field(default="", metadata=dict(
        doc="The shm-size value string"))
    ports: List[str] = field(default_factory=list, metadata=dict(
        doc="List of port to expose on the host"))
    systemType: str = field(default="", metadata=dict(
        doc="Set image system type"))
    dns: str = field(default="", metadata=dict(
        doc="A custom DNS server"))

    # Internal attribute
    envName: str = field(default="", metadata=dict(
        internal=True))
    mountInfos: Dict[str, Path] = field(
        default_factory=dict, metadata=dict(internal=True))
    volumeInfos: Dict[str, VolumeInfo] = field(
        default_factory=dict, metadata=dict(internal=True))
    runtime: Optional[Runtime] = field(default=None, metadata=dict(
        internal=True))
    systemPackages: Set[str] = field(default_factory=set, metadata=dict(
        internal=True))
    pipPackages: Set[str] = field(default_factory=set, metadata=dict(
        internal=True))
    ctx: ExecContext = field(default_factory=ExecContext, metadata=dict(
        internal=True))
    runDir: Optional[Path] = field(default=None, metadata=dict(
        internal=True))
    overlaysDir: Optional[Path] = field(default=None, metadata=dict(
        internal=True))
    manageImage: bool = field(default=True, metadata=dict(
        internal=True))
    branchImage: bool = field(default=True, metadata=dict(
        internal=True))
    autoUpdate: bool = field(default=False, metadata=dict(
        internal=True))
    mountCache: bool = field(default=False, metadata=dict(
        internal=True))
    configFile: Optional[Path] = field(default=None, metadata=dict(
        internal=True))
    registryName: str = field(default="", metadata=dict(internal=True))
    registryShortName: str = field(default="", metadata=dict(internal=True))
    # Backward compat attribute
    imageCustomizations: List[str] = field(default_factory=list, metadata=dict(
        itnernal=True))

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
                if attr.name == "preTasks":
                    # Pre-tasks needs to be kept in order
                    value = attrValue + getattr(self, attr.name)
                else:
                    # List are extended
                    value = getattr(self, attr.name) + attrValue
                setattr(self, attr.name, value)
            elif isinstance(attrValue, dict) or isinstance(attrValue, set):
                # Dictionary are updated in reverse
                mergedDict = getattr(parentEnv, attr.name)
                mergedDict.update(getattr(self, attr.name))
                setattr(self, attr.name, mergedDict)

    def __post_init__(self) -> None:
        if not self.configFile:
            raise RuntimeError("No configFile context")
        if self.desktop:
            # Ignoring type here just to convert yaml dict to DesktopEntry
            self.desktop = DesktopEntry(  # type: ignore
                envName=self.name,
                terminal=self.capabilities.get("terminal", False),
                relPath=self.configFile.parent,
                **self.desktop)
        for volumeName, volumeValue in self.volumes.items():
            if not volumeValue:
                volumeValue = volumeName
            if isinstance(volumeValue, str):
                self.volumeInfos[volumeName] = VolumeInfo(
                    name=volumeName,
                    volumeName=volumeValue)
            else:
                # Ignoring type here just to convert yaml dict to VolumeInfo
                self.volumeInfos[volumeName] = VolumeInfo(
                    name=volumeName,
                    volumeName=volumeValue["name"],  # type: ignore
                    readOnly=(volumeValue.get(  # type: ignore
                        "read-only", "no").lower() in ("yes", "true")))
        for mountName, mountValue in self.mounts.items():
            if not mountValue:
                mountValue = mountName
            self.mountInfos[mountName] = Path(mountValue)

        # Support retro cap name written in camelCase
        retroCap = {}
        for cap, value in self.capabilities.items():
            if cap in ("mountCwd", "mountRun", "autoUpdate"):
                cap = re.sub('([CRU])', r'-\1', cap).lower()
                retroCap[cap] = value

            if cap not in ValidCap:
                raise RuntimeError(f"{self.name}: unknown cap {cap}")
        self.capabilities.update(retroCap)
        # Convert str to list
        self.requires = asList(self.requires)
        # Ensure environ is a str,str mapping
        self.environ = dict(map(lambda x: (str(x[0]), str(x[1])),
                                self.environ.items()))
        self.vars = dict(map(lambda x: (str(x[0]), str(x[1])),
                             self.vars.items()))
        # Minify registry name
        self.registryShortName = "/".join(
            self.registryName.rstrip('/').split('/')[-3:])
        # Ensure parent is a str
        if not self.parent:
            self.parent = ""
        # Ensure command is a list
        self.command = asList(self.command)
        # Record envName (e.g. without any variant names)
        self.envName = self.name
        self.systemPackages = packageFilter(set(self.packages))
        self.pipPackages = pipFilter(set(self.packages))


def rootCap(active: bool, ctx: ExecContext, _: Env) -> None:
    "run as root"
    if active:
        ctx.home = Path("/root")
        ctx.xdgDir = Path("/run/user/0")
    else:
        ctx.home = Path("/home/user")
        ctx.xdgDir = Path("/run/user/1000")
        ctx.user = 1000
    ctx.environ["XDG_RUNTIME_DIR"] = str(ctx.xdgDir)
    ctx.environ["HOME"] = str(ctx.home)


def getUidMap(_: Env) -> ExecArgs:
    return ["--uidmap", "1000:0:1", "--uidmap", "0:1:1000",
            "--uidmap", "1001:1001:%s" % (2**16 - 1001)]


def uidmapCap(active: bool, ctx: ExecContext, env: Env) -> None:
    "map host uid"
    if active:
        ctx.uidmaps = getUidMap(env)


def privilegedCap(active: bool, ctx: ExecContext, _: Env) -> None:
    "run as privileged container"
    ctx.privileged = active


def terminalCap(active: bool, ctx: ExecContext, _: Env) -> None:
    "interactive mode"
    if active:
        ctx.interactive = True
        ctx.detachKeys = "ctrl-e,e"


def networkCap(active: bool, ctx: ExecContext, env: Env) -> None:
    "enable network"
    nsName = ""
    if env.network:
        if env.network == "host":
            nsName = "host"
        else:
            nsName = f"container:net-{env.network}"

    if not active and not env.network:
        nsName = "none"

    if nsName:
        ctx.namespaces["network"] = nsName


def manageImageCap(active: bool, ctx: ExecContext, env: Env) -> None:
    "manage the image with buildah"
    env.manageImage = active


def branchImageCap(active: bool, ctx: ExecContext, env: Env) -> None:
    "branch the image for this environment"
    env.branchImage = active


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


def mountHomeCap(active: bool, ctx: ExecContext, env: Env) -> None:
    "mount home to host home"
    if active:
        ctx.mounts[ctx.home] = Path("~/").expanduser()


def mountCacheCap(active: bool, ctx: ExecContext, env: Env) -> None:
    "mount image build cache"
    if active:
        env.mountCache = True


def ipcCap(active: bool, ctx: ExecContext, env: Env) -> None:
    "share host ipc"
    if active:
        ctx.namespaces["ipc"] = "host"


def x11Cap(active: bool, ctx: ExecContext, env: Env) -> None:
    "share x11 socket"
    if active:
        ctx.mounts[Path("/tmp/.X11-unix")] = Path("/tmp/.X11-unix")
        ctx.environ["DISPLAY"] = os.environ["DISPLAY"]


def pulseaudioCap(active: bool, ctx: ExecContext, env: Env) -> None:
    "share pulseaudio socket"
    if active:
        ctx.mounts[Path("/etc/machine-id:ro")] = Path("/etc/machine-id")
        ctx.mounts[ctx.xdgDir / "pulse"] = \
            Path(os.environ["XDG_RUNTIME_DIR"]) / "pulse"
        # Force PULSE_SERVER environment so that when there are more than
        # one running, pulse client aren't confused by different uid
        ctx.environ["PULSE_SERVER"] = str(ctx.xdgDir / "pulse" / "native")


def gitCap(active: bool, ctx: ExecContext, env: Env) -> None:
    "share .gitconfig and excludesfile"
    if active:
        gitconfigDir = Path("~/.config/git").expanduser().resolve()
        gitconfigFile = Path("~/.gitconfig").expanduser().resolve()
        if gitconfigDir.is_dir():
            ctx.mounts[ctx.home / ".config/git"] = gitconfigDir
        elif gitconfigFile.is_file():
            ctx.mounts[ctx.home / ".gitconfig"] = gitconfigFile
            for line in gitconfigFile.read_text().split('\n'):
                line = line.strip()
                if line.startswith("excludesfile"):
                    excludeFileName = line.split('=')[1].strip()
                    excludeFile = Path(
                        excludeFileName).expanduser().resolve()
                    if excludeFile.is_file():
                        ctx.mounts[ctx.home / excludeFileName.replace(
                            '~/', '')] = excludeFile
                # TODO: improve git crential file discovery
                elif "store --file" in line:
                    storeFileName = line.split()[-1]
                    storeFile = Path(storeFileName).expanduser().resolve()
                    if storeFile.is_file():
                        ctx.mounts[ctx.home / storeFileName.replace(
                            '~/', '')] = storeFile


def editorCap(active: bool, ctx: ExecContext, env: Env) -> None:
    "setup editor env"
    if active:
        ctx.environ["EDITOR"] = os.environ.get("EDITOR", "vi")
        env.packages.append("vi")


def sshCap(active: bool, ctx: ExecContext, env: Env) -> None:
    "share ssh agent and keys"
    if active:
        if os.environ.get("SSH_AUTH_SOCK"):
            ctx.environ["SSH_AUTH_SOCK"] = os.environ["SSH_AUTH_SOCK"]
            sshSockPath = Path(os.environ["SSH_AUTH_SOCK"])
            ctx.mounts[Path(sshSockPath)] = sshSockPath
        sshconfigFile = Path("~/.ssh/config").expanduser().resolve()
        if sshconfigFile.is_file():
            for line in sshconfigFile.read_text().split('\n'):
                line = line.strip()
                if line.startswith("ControlPath"):
                    controlPath = Path(line.split()[1].strip().replace(
                        "%i", str(os.getuid()))).parent
                    if controlPath.is_dir() and controlPath != Path('/tmp'):
                        ctx.mounts[controlPath] = controlPath

        ctx.mounts[ctx.home / ".ssh"] = Path("~/.ssh")
        env.systemPackages.add("openssh-clients")


def gpgCap(active: bool, ctx: ExecContext, env: Env) -> None:
    "share gpg agent"
    if active:
        gpgSockDir = Path(os.environ["XDG_RUNTIME_DIR"]) / "gnupg"
        ctx.mounts[ctx.xdgDir / "gnupg"] = gpgSockDir
        ctx.mounts[ctx.home / ".gnupg"] = Path("~/.gnupg")


def webcamCap(active: bool, ctx: ExecContext, env: Env) -> None:
    "share webcam device"
    if active:
        for device in list(filter(lambda x: x.startswith("video"),
                                  os.listdir("/dev"))):
            ctx.devices.append(Path("/dev") / device)


def alsaCap(active: bool, ctx: ExecContext, env: Env) -> None:
    "share alsa device"
    if active:
        ctx.devices.append(Path("/dev/snd"))


def driCap(active: bool, ctx: ExecContext, env: Env) -> None:
    "share graphic device"
    if active:
        ctx.devices.append(Path("/dev/dri"))


def kvmCap(active: bool, ctx: ExecContext, env: Env) -> None:
    "share kvm device"
    if active:
        ctx.devices.append(Path("/dev/kvm"))


def tunCap(active: bool, ctx: ExecContext, env: Env) -> None:
    "share tun device"
    if active:
        ctx.devices.append(Path("/dev/net/tun"))
        env.systemPackages.add("iproute")


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


def foregroundCap(active: bool, ctx: ExecContext, env: Env) -> None:
    "work around application that goes into background"
    if active:
        # TODO: add gui dialog support for terminal-less usage
        env.command = [
            "bash", "-c",
            "set -e; {command}; echo 'press ctrl-c to quit'; sleep Inf".format(
                command=";".join(env.command)
            )]


def autoUpdateCap(active: bool, _: ExecContext, env: Env) -> None:
    "keep environment updated"
    if active:
        env.autoUpdate = True


def camelCaseToHyphen(name: str) -> str:
    return re.sub('([A-Z]+)', r'-\1', name).lower()


Capability = Callable[[bool, ExecContext, Env], None]
Capabilities: List[Tuple[str, Optional[str], Capability]] = [
    (camelCaseToHyphen(func.__name__[:-3]), func.__doc__, func) for func in [
        manageImageCap,
        branchImageCap,
        rootCap,
        privilegedCap,
        terminalCap,
        ipcCap,
        x11Cap,
        pulseaudioCap,
        gitCap,
        editorCap,
        sshCap,
        gpgCap,
        webcamCap,
        alsaCap,
        driCap,
        kvmCap,
        tunCap,
        seccompCap,
        selinuxCap,
        setuidCap,
        ptraceCap,
        networkCap,
        foregroundCap,
        mountCwdCap,
        mountHomeCap,
        mountRunCap,
        mountCacheCap,
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
        for cap in ("x11", "tun", "pulseaudio"):
            if env.capabilities.get(cap):
                warn(
                    f"SELinux is disabled because capability '{cap}' need "
                    "extra type enforcement that are not currently supported.")
                selinuxCap(False, env.ctx, env)
                env.capabilities["selinux"] = False

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
            if isinstance(hostPath, VolumeInfo):
                continue
            hostPath = hostPath.expanduser().resolve()
            if hostPath.exists() and \
               selinux.getfilecon(str(hostPath))[1].split(':')[2] != label:
                warn(f"SELinux is disabled because {hostPath} doesn't have "
                     f"the {label} label. To set the label run: "
                     f"chcon -Rt {label} {hostPath}")
                selinuxCap(False, env.ctx, env)

    # Check mount points permissions
    for hostPath in env.ctx.mounts.values():
        if isinstance(hostPath, VolumeInfo):
            if not hostPath.readOnly and not env.capabilities.get("uidmap"):
                warn("UIDMap is required for rw volume")
                uidmapCap(True, env.ctx, env)
                break
            continue
        hostPath = hostPath.expanduser().resolve()
        if hostPath.exists() and not os.access(str(hostPath), os.R_OK):
            warn(f"{hostPath} is not readable by the current user.")

    # Check for home mount point
    if env.overlays and not env.ctx.mounts.get(env.ctx.home):
        warn(f"overlay needs a home mount point, "
             "mountRun capability is enabled.")
        mountRunCap(True, env.ctx, env)

    if env.capabilities.get("mount-home") and not env.capabilities.get(
            "uidmap"):
        warn("UIDMap is required for mount-home")
        uidmapCap(True, env.ctx, env)

    # Check for image management
    if not env.manageImage:
        if env.packages:
            warn("manage-image capability is required for packages")
            manageImageCap(True, env.ctx, env)
        if env.imageCustomizations or env.imageTasks:
            warn("manage-image capability is required for image tasks")
            manageImageCap(True, env.ctx, env)
        if env.branchImage:
            warn("branch-image capability is incompatible with manage-image")


# The name of the environment
EnvName = str
# The podman run argument
PodmanArgs = ExecArgs
# The environment command
EnvArgs = ExecArgs
# List of pre/post tasks commands to run on the host
HostPreArgs = List[str]
HostPostArgs = List[str]
PrepareEnvResults = Tuple[
    EnvName, PodmanArgs, EnvArgs, HostPreArgs, HostPostArgs]


def prepareEnv(
        env: Env,
        cliArgs: List[str],
        packages: List[str]) -> PrepareEnvResults:
    """Generate podman exec args based on capabilities"""
    # Setup substitution format matp
    vars = env.vars.copy()
    vars.update(env.environ)

    # Apply capabilities
    for name, _, capability in Capabilities:
        capability(env.capabilities.get(name, False), env.ctx, env)

    if packages:
        # Add user provided extra packages
        cliPackages = set(packages)
        env.systemPackages.update(packageFilter(cliPackages))
        env.pipPackages.update(pipFilter(cliPackages))

    # Apply extra settings from the environment definition:
    args = ["--hostname", env.name]
    if env.dns and env.ctx.hasDirectNetwork():
        args.append(f"--dns={env.dns}")
    if env.shmsize:
        args.append(f"--shm-size={env.shmsize}")
    if env.home:
        env.ctx.mounts[env.ctx.home] = Path(env.home).expanduser().resolve()
    if env.ports:
        for port in env.ports:
            port = port.format(**vars)
            args.append(f"--publish={port}")

    env.addHosts = dictFormat(env.addHosts, vars)

    env.ctx.syscaps.extend(env.syscaps)
    env.ctx.sysctls.extend(env.sysctls)
    env.ctx.environ.update(env.environ)
    env.ctx.addHosts.update(env.addHosts)

    for containerPath, hostPath in env.mountInfos.items():
        hostPath = hostPath.expanduser().resolve()
        if containerPath.startswith("~/"):
            env.ctx.mounts[env.ctx.home / containerPath[2:]] = hostPath
        else:
            env.ctx.mounts[Path(containerPath)] = hostPath

    # Inject volume in mounts point
    for volumeName, volumeInfo in env.volumeInfos.items():
        containerPath = str(volumeInfo.getContainerPath())
        if containerPath.startswith("~"):
            containerPath = str(env.ctx.home / containerPath.lstrip('~/'))
        env.ctx.mounts[Path(containerPath)] = volumeInfo

    # Look for file argument requirement
    fileArg: Optional[Path] = None
    if "$1" in env.command:
        if len(cliArgs) > 1:
            raise RuntimeError("Multiple file input %s" % cliArgs)
        if cliArgs:
            fileArg = Path(cliArgs.pop()).expanduser().resolve(strict=True)
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
    if env.ctx.uidmaps and env.ctx.namespaces.get(
            "network", "").startswith("container:"):
        env.ctx.namespaces["userns"] = env.ctx.namespaces["network"]

    if env.pipPackages:
        env.preTasks.append({"name": "Active virtualenv",
                             "shell": "source /venv/bin/activate"})

    envArgs = list(map(lambda x: safeFormat(str(x), vars), commandArgs))
    hostPreTasks: ExecArgs = []
    if env.preTasks:
        if not envArgs:
            raise RuntimeError("Can't use pre-tasks without a command")
        # TODO: create a /tmp/start.sh script when preTasks are too long
        envPreTasks: ExecArgs = []
        for preTask in env.preTasks:
            command = safeFormat(taskToCommand(preTask), vars)
            if command.startswith("run_local_host; "):
                hostPreTasks.append(command.split('; ', 1)[1])
            else:
                envPreTasks.append(command)
        if len(envPreTasks):
            envArgs = ["bash", "-c", "; ".join(["set -e"] + envPreTasks) +
                       "; exec \"" + "\" \"".join(envArgs) + "\""]

    hostPostTasks: ExecArgs = []
    if env.postTasks:
        for postTask in env.postTasks:
            command = safeFormat(taskToCommand(postTask), vars)
            if command.startswith("run_local_host; "):
                hostPostTasks.append(command.split('; ', 1)[1])
            else:
                raise RuntimeError("Only host post-task are supported")

    return env.name, args + env.ctx.getArgs(), envArgs, hostPreTasks, \
        hostPostTasks


def cleanupEnv(env: Env) -> None:
    ...
