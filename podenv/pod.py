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
This module prepares and executes the runtime.
"""

import copy
import fcntl
import logging
import json
import os
import select
import shlex
import time
import urllib.request
from abc import abstractmethod
from contextlib import contextmanager
from hashlib import md5
from pathlib import Path
from typing import Any, ContextManager, Dict, Generator, List, \
    Optional, Set, Tuple, Union
from subprocess import Popen, PIPE
try:
    import selinux  # type: ignore
    HAS_SELINUX = True
except ImportError:
    HAS_SELINUX = False

from podenv.env import DesktopEntry, Env, ExecArgs, Info, Runtime, getUidMap, \
    UserNotif, taskToCommand, pipFilter, packageFilter

log = logging.getLogger("podenv")
BuildId = str
BuildSession = ContextManager[BuildId]
Command = Union[str, List[str]]
TZFormat = "%Y-%m-%dT%H:%M:%S"
DAY = 3600 * 24
HasRoute: Optional[bool] = None
GuixBuilder = "guix-daemon --build-users-group=guix-builder " \
    "--disable-chroot & true"


class AlreadyRunning(Exception):
    pass


def execute(
        args: ExecArgs,
        cwd: Optional[Path] = None,
        textOutput: bool = False) -> Optional[str]:
    log.debug("Running %s" % " ".join(args))
    proc = Popen(
        args, stdout=PIPE if textOutput else None,
        cwd=str(cwd) if cwd else None)
    try:
        stdout, stderr = proc.communicate()
    except KeyboardInterrupt:
        proc.terminate()
        proc.kill()
        raise
    if proc.wait():
        raise RuntimeError("Failed to run %s" % " ".join(args))
    if textOutput:
        output: str = stdout.decode('utf-8')
        return output
    return None


def desktopNotification(msg: str, color: str = "92") -> None:
    if Popen(["notify-send", msg]).wait():
        log.warning("Couldn't send notification")


def pwrite(args: ExecArgs, stdin: bytes) -> None:
    log.debug("Running %s" % " ".join(args))
    proc = Popen(args, stdin=PIPE)
    try:
        proc.stdin.write(stdin)
    except KeyboardInterrupt:
        proc.terminate()
        proc.kill()
    if proc.wait():
        raise RuntimeError("Failed to run %s" % " ".join(args))


def pread(
        args: ExecArgs, live: bool = False, cwd: Optional[Path] = None) -> str:
    if live:
        return preadLive(args, cwd)
    output = execute(args, cwd, True)
    if output:
        return output
    return ""


def preadLive(args: ExecArgs, cwd: Optional[Path] = None) -> str:
    log.debug("Running %s" % " ".join(args))
    proc = Popen(
        args, bufsize=0, start_new_session=True,
        stdout=PIPE, stderr=PIPE, cwd=str(cwd) if cwd else None)
    for f in (proc.stdout, proc.stderr):
        # Make proc output non blocking
        fd = f.fileno()
        fl = fcntl.fcntl(fd, fcntl.F_GETFL)
        fcntl.fcntl(fd, fcntl.F_SETFL, fl | os.O_NONBLOCK)
    output = []
    while True:
        active = False
        r, w, x = select.select([proc.stdout, proc.stderr], [], [], 1)
        for reader in r:
            data = reader.read()
            if data:
                active = True
                data = data.decode('utf-8')
                print(data, end='')
                output.append(data)
        if not active and proc.poll() is not None:
            if proc.poll():
                raise RuntimeError("Failed to run %s" % " ".join(args))
            return "".join(output)


def readProcessJson(args: ExecArgs) -> Any:
    """Silently read a process JSON output"""
    proc = Popen(args, stdout=PIPE, stderr=PIPE, cwd='/')
    stdout, _ = proc.communicate()
    if proc.wait():
        return None
    return json.loads(stdout)


def commandsToExecArgs(commands: Command) -> ExecArgs:
    if isinstance(commands, list):
        return ["bash", "-c", "; ".join(["set -e"] + commands)]
    else:
        return shlex.split(commands)


def podmanInspect(objType: str, name: str) -> Dict[str, Any]:
    infoList = readProcessJson(["podman", objType, "inspect", name])
    if not infoList:
        return {}
    if len(infoList) != 1:
        raise RuntimeError(f"Multiple container match {name}")
    info: Dict[str, Any] = infoList[0]
    return info


def podmanExists(objType: str, name: str) -> bool:
    try:
        execute(["podman", objType, "exists", name])
        return True
    except RuntimeError:
        return False


def hash(message: str) -> str:
    return md5(message.encode('utf-8')).hexdigest()


def now() -> str:
    return time.strftime(TZFormat)


def age(date: str) -> float:
    return time.time() - time.mktime(time.strptime(date, TZFormat))


def canUpdate() -> bool:
    global HasRoute
    if HasRoute is None:
        try:
            HasRoute = "via" in pread(["ip", "route", "get", "1.1.1.1"])
        except RuntimeError:
            log.warning("Skipping update because host is not online")
            HasRoute = False
    return HasRoute


def updated(lastUpdate: str, maxAge: int = DAY) -> bool:
    return age(lastUpdate) < maxAge


def outdated(path: Path, maxAge: int = DAY) -> bool:
    return not path.exists() or os.stat(path).st_mtime < (time.time() - maxAge)


def isSelinux() -> bool:
    return HAS_SELINUX and Path("/sys/fs/selinux/").exists()


def getSelinuxLabel(env: Env) -> str:
    return "system_u:object_r:container_file_t:s0"


@contextmanager
def buildah(fromRef: str) -> Generator[BuildId, None, None]:
    ctr = pread(["buildah", "from", fromRef]).strip()
    try:
        yield ctr
    finally:
        execute(["buildah", "delete", ctr], textOutput=True)


def buildahRunCommand() -> ExecArgs:
    return ["buildah", "run", "--network", "host", "--user", "0:0"]


def buildahConfig(buildId: BuildId, config: str) -> None:
    execute(["buildah", "config"] + shlex.split(config) + [buildId])


def buildahCommit(buildId: BuildId, tagRef: str) -> None:
    execute(["buildah", "commit", buildId, tagRef])


class PodmanRuntime(Runtime):
    System: Dict[str, Dict[str, Any]] = {
        "dnf": {
            "commands": {
                "install": "dnf install -y",
                "update": "dnf update -y",
            },
            "mounts": {
                "/var/cache/dnf": "~/.cache/podenv/dnf"
            }
        },
        "yum": {
            "commands": {
                "install": "yum install -y",
                "update": "yum update -y",
            },
            "mounts": {
                "/var/cache/yum": "~/.cache/podenv/yum"
            }
        },
        "apt-get": {
            "commands": {
                "install": "apt-get install -y",
                "update": "apt-get update -y"
            },
            "mounts": {
                "/var/cache/apt": "~/.cache/podenv/apt"
            },
            "packagesMap": {
                "vi": "vim"
            },
        },
        "emerge": {
            "mounts": {
                "/var/db/repos/gentoo": "~/.cache/podenv/portage",
                "/usr/portage": "~/.cache/podenv/portage",
                "/var/cache/distfiles": "~/.cache/podenv/distfiles",
                "/var/cache/eix": "~/.cache/podenv/eix",
            },
            "commands": {
                "install": "emerge -vDNt --verbose-conflicts",
                "update": "emerge -uvDNt @world",
                "pre-update": "emerge --sync --quiet",
            },
            "packagesMap": {
                "git": "dev-vcs/git",
                "vi": "app-editors/vim",
            }
        },
        "guix": {
            "mounts": {
                "/gnu": "~/.cache/podenv/guix/gnu",
                "/var/guix": "~/.cache/podenv/guix/var/guix",
            },
            "commands": {
                "install": [
                    GuixBuilder,
                    "guix install"],
                "pre-update": [
                    GuixBuilder,
                    "rm -f /var/guix/profiles/default/current-guix",
                    "guix pull"],
                "update": [
                    GuixBuilder,
                    "guix package -u"],
            },
            "environments": {
                "GUIX_PROFILE": "/root/.config/guix/current",
                "PATH": ("/var/guix/profiles/per-user/root/current-guix/bin/:"
                         "/bin:/sbin:/usr/bin:/usr/sbin:"
                         "/usr/local/bin:/usr/local/sbin")
            },
            "packagesMap": {
                "vi": "vim"
            },
        },
    }

    def __init__(self, cacheDir: Path, name: str, fromRef: str):
        self.commands: Dict[str, Command] = {}
        self.mounts: Dict[str, str] = {}
        self.runtimeMounts: Dict[str, str] = {}
        self.packagesMap: Dict[str, str] = {}
        self.environments: Dict[str, str] = {}
        self.extras: Dict[str, ExtraRuntime] = {}
        self.info: Info = {}
        self.systemType: str = ""
        self.fromRef: str = fromRef
        self.name: str = name
        self.metadataPath = Path(
            str(cacheDir / self.name).rstrip('/') + '.json')

    def exists(self, autoUpdate: bool) -> bool:
        return self.metadataPath.exists()

    def loadInfo(self) -> None:
        self.info = json.loads(self.metadataPath.read_text())
        if self.info.get("system") and isinstance(self.info["system"], str):
            self.setSystemType(self.info["system"])

    def getCustomizations(self) -> List[str]:
        customHashes = self.info.get("customHash", [])
        if not isinstance(customHashes, list):
            raise RuntimeError("Invalid customHash metadata")
        return customHashes

    def getInstalledPackages(self) -> List[str]:
        if self.systemType != self.info.get("system"):
            packages = self.info.get(f"{self.systemType}Packages", [])
        else:
            packages = self.info.get("packages", [])
        if not isinstance(packages, list):
            raise RuntimeError("Invalid packages metadata")
        return packages

    def updateInstalledPackages(self, packages: Set[str]) -> None:
        installedPackages = list(packages.union(self.getInstalledPackages()))
        newInfo: Info = {}
        if self.systemType != self.info.get("system"):
            newInfo = {f"{self.systemType}Packages": installedPackages}
        else:
            newInfo = {"packages": installedPackages}
        self.updateInfo(newInfo)

    def needUpdate(self) -> bool:
        key = "updated"
        if self.systemType != self.info.get("system"):
            key = f"{self.systemType}Updated"
        lastUpdate = self.info.get(key, "")
        if not isinstance(lastUpdate, str):
            raise RuntimeError(f"Invalid {key} metadata")
        return not lastUpdate or (not updated(lastUpdate) and canUpdate())

    def setLastUpdated(self) -> None:
        key = "updated"
        if self.systemType != self.info.get("system"):
            key = f"{self.systemType}Updated"
        self.updateInfo({key: now()})

    def updateInfo(self, info: Info) -> None:
        self.info.update(info)
        self.metadataPath.write_text(json.dumps(self.info))

    def __repr__(self) -> str:
        return self.name

    def setSystemType(self, systemType: str) -> None:
        # Backward compat
        if systemType == "rpm":
            systemType = "dnf"
        elif systemType == "apt":
            systemType = "apt-get"
        self.systemType = systemType
        self.commands = self.System[systemType]["commands"]
        self.mounts = self.System[systemType].get("mounts", {})
        self.packagesMap = self.System[systemType].get("packagesMap", {})
        self.environments = self.System[systemType].get("environment", {})

    def getSystemType(self, buildId: BuildId) -> str:
        systemType = self.info.get("system")
        if not systemType:
            for systemType in self.System:
                try:
                    self.runCommand(
                        buildId, ["sh", "-c", f"type {systemType}"])
                    break
                except RuntimeError:
                    pass
            else:
                raise RuntimeError("Couldn't discover system type")
        if not isinstance(systemType, str):
            # Mypy is confused by the type from the for loop
            raise RuntimeError("Invalid systemType")
        self.setSystemType(systemType)
        return systemType

    def getMounts(self, mountsDict: Dict[str, str]) -> ExecArgs:
        mounts = []
        for containerDir, hostDir in mountsDict.items():
            hostPath = Path(hostDir).expanduser().resolve()
            if not hostPath.exists():
                hostPath.mkdir(parents=True, exist_ok=True)
                if isSelinux():
                    selinux.chcon(
                        str(hostPath), "system_u:object_r:container_file_t:s0")
            mounts.extend(["-v", "%s:%s" % (
                hostPath, containerDir)])
        return mounts

    def getSystemMounts(self, withTmp: bool) -> ExecArgs:
        """Mount points needed to manage the runtime image"""
        # Filter out runtimeMounts
        extra = ["--mount=type=tmpfs,destination=/tmp"] if withTmp else []
        return self.getMounts(dict(filter(
            lambda x: x[0] not in self.runtimeMounts,
            self.mounts.items()))) + extra

    def getSystemEnvironments(self) -> ExecArgs:
        envs = []
        for key, value in self.environments:
            envs.extend(["-e", f"{key}={value}"])
        return envs

    def getSystemArgs(self) -> ExecArgs:
        return self.getRuntimeArgs() + self.getSystemMounts(withTmp=True)

    def getRuntimeArgs(self) -> ExecArgs:
        return self.getRuntimeMounts() + self.getSystemEnvironments()

    def create(self) -> None:
        with self.getSession(create=True) as buildId:
            systemType = self.getSystemType(buildId)
            for command in ["useradd -u 1000 -m user",
                            "mkdir -p /run/user/1000",
                            "chown 1000:1000 /run/user/1000",
                            "mkdir -p /run/user/0",
                            "chown 0:0 /run/user/0"]:
                self.runCommand(buildId, command)
            for config in ["--author='%s'" % os.environ["USER"],
                           "--created-by podenv"]:
                self.applyConfig(buildId, config)
            self.commit(buildId)
        self.updateInfo(dict(created=now(),
                             updated="1970-01-01T00:00:00",
                             packages=[],
                             system=systemType,
                             fromRef=self.fromRef))

    def update(self) -> None:
        for retry in range(3):
            try:
                with self.getSession() as buildId:
                    if self.commands.get("pre-update"):
                        self.runCommand(buildId, commandsToExecArgs(
                            self.commands["pre-update"]))

                    updateOutput = pread(
                        self.runCommandArgs(buildId) +
                        commandsToExecArgs(self.commands["update"]),
                        live=True)
                    if "Nothing to do." in updateOutput:
                        break
                    self.commit(buildId)
                    break
            except RuntimeError:
                if retry == 2:
                    raise
                log.warning("Retrying failed update...")
        self.setLastUpdated()

    def install(self, packages: Set[str]) -> None:
        with self.getSession() as buildId:
            packagesMapped = map(lambda x: self.packagesMap.get(x, x),
                                 packages)
            command = copy.copy(self.commands["install"])
            packagesArgs = " " + " ".join(packagesMapped)
            if isinstance(command, list):
                command[-1] += packagesArgs
            else:
                command += packagesArgs
            self.runCommand(
                buildId, commandsToExecArgs(command))
            self.commit(buildId)
        self.updateInstalledPackages(packages)

    def enableExtra(self, extra: str, env: Env) -> None:
        if extra == "pip":
            extraRuntime = PipRuntime(self, env.envName)
        else:
            raise NotImplementedError(f"Unknown extra type {extra}")
        self.extras[extra] = extraRuntime
        extraRuntime.enable(env)

    def getRuntimeMounts(self) -> ExecArgs:
        return self.getMounts(self.runtimeMounts)

    def getInstalledExtra(self, extra: str) -> Set[str]:
        return self.extras[extra].getInstalled()

    def updateExtra(self, extra: str) -> None:
        self.extras[extra].update()

    def installExtra(self, extra: str, packages: Set[str]) -> None:
        self.extras[extra].install(packages)

    def customize(self, commands: List[Tuple[str, str]]) -> None:
        knownHash: Set[str] = set(self.info.get("customHash", []))
        with self.getSession() as buildId:
            for commandHash, command in commands:
                commandArgs: ExecArgs = []
                if not command.startswith("run_local_host; "):
                    commandArgs = self.runCommandArgs(buildId)
                else:
                    command = command.split('; ', 1)[1]
                commandArgs += ["/bin/bash", "-c", command]
                execute(commandArgs)
                knownHash.add(commandHash)
            self.commit(buildId)
        self.updateInfo(dict(customHash=list(knownHash)))

    @abstractmethod
    def getExecName(self) -> ExecArgs:
        ...

    @abstractmethod
    def getSession(self, create: bool = False) -> BuildSession:
        ...

    @abstractmethod
    def runCommandArgs(self, buildId: BuildId) -> ExecArgs:
        ...

    @abstractmethod
    def runCommand(self, buildId: BuildId, command: Command) -> None:
        ...

    @abstractmethod
    def applyConfig(self, buildId: BuildId, config: str) -> None:
        ...

    @abstractmethod
    def commit(self, buildId: BuildId) -> None:
        ...


class ExtraRuntime:
    def __init__(
            self, extra: str, runtime: PodmanRuntime, envName: str) -> None:
        self.extraDir = (Path("~/.cache/podenv") / (
            runtime.name + "-" + envName + "-pip")).expanduser()
        self.metadataPath = Path(str(self.extraDir) + ".json")
        self.packages: Set[str] = set()
        self.runtime: PodmanRuntime = runtime

    def getInstalled(self) -> Set[str]:
        return self.packages

    @abstractmethod
    def update(self) -> None:
        ...

    @abstractmethod
    def install(self, packages: Set[str]) -> None:
        ...

    @abstractmethod
    def enable(self, env: Env) -> None:
        ...


def createContainerDir(path: Path) -> None:
    path.mkdir(parents=True, exist_ok=True)
    if isSelinux():
        selinux.chcon(str(path), "system_u:object_r:container_file_t:s0")


class PipRuntime(ExtraRuntime):
    def __init__(self, runtime: PodmanRuntime, envName: str) -> None:
        super().__init__("pip", runtime, envName)
        self.pipCache = Path("~/.cache/podenv/pip-cache").expanduser()
        if not self.pipCache.exists():
            createContainerDir(self.pipCache)

    def enable(self, env: Env) -> None:
        if self.metadataPath.exists():
            self.packages = set(json.loads(self.metadataPath.read_text())[
                "packages"])
            createContainerDir(self.extraDir)
        self.runtime.mounts["/root/.cache/pip"] = str(self.pipCache)
        self.runtime.mounts["/venv"] = str(self.extraDir)
        self.runtime.runtimeMounts["/venv"] = str(self.extraDir)

    def exists(self) -> bool:
        return (self.extraDir / "bin" / "activate").exists()

    def create(self) -> None:
        with self.runtime.getSession() as buildId:
            self.runtime.runCommand(
                buildId, "python3 -mvenv --system-site-packages /venv")

    def update(self) -> None:
        if not self.packages:
            return
        if not self.exists():
            self.create()
        with self.runtime.getSession() as buildId:
            self.runtime.runCommand(
                buildId, "/venv/bin/pip install --upgrade " + " ".join(
                    self.packages))

    def install(self, packages: Set[str]) -> None:
        if not self.exists():
            self.create()
        with self.runtime.getSession() as buildId:
            self.runtime.runCommand(
                buildId, "/venv/bin/pip install " + " ".join(map(
                    lambda x: x.lstrip('pip:'), packages)))
        self.packages.update(packages)
        self.metadataPath.write_text(json.dumps(dict(
            packages=list(self.packages))))


class ContainerImage(PodmanRuntime):
    def __init__(
            self,
            cacheDir: Path,
            fromRef: str,
            manage: bool,
            localName: str) -> None:
        self.localName: str = fromRef
        self.manage: bool = manage
        if manage:
            if not localName:
                localName = fromRef.split('/', 1)[-1].replace(':', '-')
            self.localName = "localhost/podenv/" + localName
        super().__init__(cacheDir,
                         fromRef.replace(':', '-').replace('/', '_'),
                         fromRef)

    def __repr__(self) -> str:
        return self.localName

    def getExecName(self) -> ExecArgs:
        return [self.localName]

    def create(self) -> None:
        if self.manage:
            super().create()
        else:
            self.pull()

    def update(self) -> None:
        if self.manage:
            super().update()
        else:
            self.pull()

    def pull(self) -> None:
        execute(["podman", "pull", self.fromRef])
        self.updateInfo({"updated": now()})

    def getSession(self, create: bool = False) -> BuildSession:
        return buildah(self.fromRef if create else self.localName)

    def runCommand(self, buildId: BuildId, command: Command) -> None:
        if isinstance(command, list):
            args = command
        else:
            args = shlex.split(command)
        execute(self.runCommandArgs(buildId) + args)

    def runCommandArgs(self, buildId: BuildId) -> ExecArgs:
        return buildahRunCommand() + self.getSystemArgs() + [buildId]

    def applyConfig(self, buildId: BuildId, config: str) -> None:
        buildahConfig(buildId, config)

    def commit(self, buildId: BuildId) -> None:
        tagRef = "%s:%s" % (self.localName, now().replace(':', '-'))
        buildahCommit(buildId, tagRef)
        execute(["buildah", "tag", tagRef, f"{self.localName}:latest"])

    def exists(self, autoUpdate: bool) -> bool:
        if super().exists(autoUpdate):
            if not podmanExists("image", self.localName):
                return False
            if not autoUpdate:
                return True
            # Check if image needs to be rebuilt from scratch
            self.loadInfo()
            if not isinstance(self.info["created"], str):
                raise RuntimeError("Invalid created metadata")
            if age(self.info["created"]) > DAY * 21 and self.needUpdate():
                log.info("Re-creating base layer because it is too old")
                self.pull()
                self.info.clear()
                return False
            return True
        return False


def extractUrl(url: str, target: Path) -> None:
    log.info(f"Downloading {url} to memory...")
    req = urllib.request.urlopen(url)
    target.mkdir(parents=True, exist_ok=True)
    if isSelinux():
        selinux.chcon(str(target), "system_u:object_r:container_file_t:s0")
    pwrite(["tar", "-C", str(target),
            "--exclude", "dev/*", "-xJf", "-"], req.read())


def downloadUrl(url: str, target: Path) -> None:
    log.info(f"Downloading {url} to {target}...")
    target.parent.mkdir(parents=True, exist_ok=True)
    req = urllib.request.urlopen(url)
    target.write_bytes(req.read())


class RootfsDirectory(PodmanRuntime):
    def __init__(self, cacheDir: Path, fromRef: str) -> None:
        if fromRef.startswith("http"):
            self.url = fromRef
            fromRefPath = Path("~/.config/share/podenv/") / os.path.basename(
                fromRef).split(".tar")[0]
        else:
            fromRefPath = Path(fromRef)
            self.url = ""
        self.rootfs = fromRefPath.expanduser().resolve()
        super().__init__(
            cacheDir, os.path.basename(fromRefPath), str(self.rootfs))

    def getExecName(self) -> ExecArgs:
        return ["--rootfs", self.fromRef]

    def getSession(self, create: bool = False) -> BuildSession:
        @contextmanager
        def fakeSession() -> Generator[BuildId, None, None]:
            yield "noop"
        return fakeSession()

    def runCommand(self, _: BuildId, command: Command) -> None:
        if not self.rootfs.exists():
            if self.url:
                extractUrl(self.url, self.rootfs)
            else:
                raise RuntimeError(f"{self.rootfs}: does not exist")
        execute(self.runCommandArgs(_) + commandsToExecArgs(command))

    def runCommandArgs(self, _: BuildId) -> ExecArgs:
        return ["podman", "run", "--rm", "-t"] + \
            self.getSystemArgs() + self.getExecName()

    def applyConfig(self, buildId: BuildId, config: str) -> None:
        # Rootfs doesn't have metadata
        pass

    def commit(self, buildId: BuildId) -> None:
        # Rootfs doesn't have layer
        pass


def setupRuntime(userNotif: UserNotif, env: Env, cacheDir: Path) -> ExecArgs:
    """Ensure image is ready."""
    cacheDir = cacheDir.expanduser()
    if not cacheDir.exists():
        cacheDir.mkdir(parents=True)

    if env.image and env.rootfs:
        raise RuntimeError(
            f"{env.envName}: both image and rootfs can't be defined")

    if env.rootfs:
        env.runtime = RootfsDirectory(cacheDir, env.rootfs)
    elif env.image:
        env.runtime = ContainerImage(
            cacheDir, env.image, env.manageImage,
            env.envName if env.branchImage else "")

    if not env.runtime:
        raise NotImplementedError("No runtime is defined")

    if not env.runtime.exists(env.autoUpdate):
        userNotif(f"Creating {env.runtime}")
        env.runtime.create()
    else:
        env.runtime.loadInfo()

    if env.systemType:
        env.runtime.setSystemType(env.systemType)

    return env.runtime.getExecName()


def configureRuntime(
        userNotif: UserNotif, env: Env, packages: List[str]) -> None:
    if not env.runtime:
        raise RuntimeError("Env has no metadata")

    imageCustomizations = env.runtime.getCustomizations()
    envCustomizations: Dict[str, str] = {}
    for envCustomization in env.imageCustomizations + list(map(
            taskToCommand, env.imageTasks)):
        envCustomizations[hash(envCustomization)] = envCustomization
    needCustomizations: List[Tuple[str, str]] = [
        (hash, command) for hash, command in envCustomizations.items()
        if hash not in imageCustomizations]
    if needCustomizations:
        userNotif(f"Customizing image {needCustomizations}")
        env.runtime.customize(needCustomizations)

    imagePackages = env.runtime.getInstalledPackages()
    envPackages = env.systemPackages
    envPipPackages = env.pipPackages

    if packages:
        # Add user provided extra packages
        cliPackages = set(packages)
        envPackages.update(packageFilter(cliPackages))
        envPipPackages.update(pipFilter(cliPackages))

    if envPipPackages:
        envPackages.add("python3")
        env.runtime.enableExtra("pip", env)

    if env.autoUpdate and env.runtime.needUpdate():
        userNotif(f"Updating {env.runtime}")
        env.runtime.update()
        if envPipPackages:
            env.runtime.updateExtra("pip")

    need = envPackages.difference(imagePackages)
    if need:
        userNotif(f"Installing {need}")
        env.runtime.install(need)

    if envPipPackages:
        pipPackages = env.runtime.getInstalledExtra("pip")
        needPip = envPipPackages.difference(pipPackages)
        if needPip:
            userNotif(f"Installing {needPip}")
            env.runtime.installExtra("pip", needPip)


def setupInfraNetwork(networkName: str, imageName: ExecArgs, env: Env) -> None:
    """Setup persistent infra pod"""
    try:
        args = ["--detach"]
        if env.capabilities.get("uidmap"):
            args.extend(getUidMap(env))
        if env.dns:
            args.append(f"--dns={env.dns}")

        executePod("net-" + networkName, args, imageName, ["sleep", "Inf"])
    except AlreadyRunning:
        pass


def setupDesktopFile(
        desktopEntry: DesktopEntry,
        appDir: Path = Path("~/.local/share/applications")) -> None:
    appDir = appDir.expanduser().resolve()
    if not appDir.exists():
        appDir.mkdir(parents=True)
    desktopFile = appDir / (desktopEntry.envName + ".desktop")
    desktopContent = desktopEntry.format()
    if not desktopFile.exists() or desktopContent != desktopFile.read_text():
        desktopFile.write_text(desktopContent)
        log.info(f"{desktopFile}: wrote entry spec")


def setupRunDir(env: Env) -> None:
    path = env.runDir
    if path and not path.exists():
        if not path.parent.exists():
            path.parent.mkdir(mode=0o700)
        path.mkdir(mode=0o700)
        if isSelinux():
            selinux.chcon(str(path), getSelinuxLabel(env))


def setupPod(
        userNotif: UserNotif,
        env: Env,
        packages: List[str],
        cacheDir: Path = Path("~/.cache/podenv")) -> ExecArgs:
    imageName = setupRuntime(userNotif, env, cacheDir)
    configureRuntime(userNotif, env, packages)
    if env.network:
        setupInfraNetwork(env.network, imageName, env)

    # TODO: check for environment requires list

    if not env.runtime:
        raise RuntimeError("Env has no runtime")
    imageName = env.runtime.getRuntimeArgs() + imageName
    if env.capabilities.get("mount-cache"):
        # Inject runtime volumes
        imageName = env.runtime.getSystemMounts(withTmp=False) + imageName

    if env.desktop:
        setupDesktopFile(env.desktop)

    for containerPath, hostPath in sorted(env.ctx.mounts.items()):
        hostPath = hostPath.expanduser().resolve()
        if not hostPath.exists() and str(hostPath).startswith(str(env.runDir)):
            setupRunDir(env)
            if hostPath.name == "tmp":
                hostPath.mkdir(mode=0o1777)
                # TODO: check why mkdir mode results in 1774
                hostPath.chmod(0o1777)
            else:
                hostPath.mkdir(mode=0o755, parents=True)
        elif not hostPath.exists():
            hostPath.mkdir(mode=0o755, parents=True)
            if isSelinux():
                selinux.chcon(str(hostPath), getSelinuxLabel(env))
        if env.runDir and env.runDir.exists() and \
           str(containerPath).startswith(str(env.ctx.home) + "/") and \
           hostPath.is_dir():
            # Need to create parent dir, otherwise podman creates them as root
            tmpPath = env.runDir / "home" / str(containerPath).replace(
                str(env.ctx.home) + "/", "")
            tmpPath.mkdir(parents=True, exist_ok=True)

    if env.overlaysDir:
        for overlay in env.overlays:
            homeDir = env.ctx.mounts.get(env.ctx.home)
            if not homeDir:
                raise RuntimeError(
                    "Overlays without home mount isn't supported")

            if isinstance(overlay, str):
                overlayDir = env.overlaysDir / overlay
                if not overlayDir.exists():
                    raise RuntimeError(f"{overlay} does not exists")
                execute(["rsync", "--copy-links", "-a", str(overlayDir) + "/",
                         str(homeDir) + "/"])
            else:
                for containerHomePath, content in overlay.items():
                    hostPath = homeDir / containerHomePath
                    if not hostPath.exists():
                        hostPath.write_text(content)
                    elif hostPath.read_text() != content:
                        log.info(
                            f"Overlay skipped for {hostPath}: already exists")

    return imageName


def executeHostTasks(commands: List[str]) -> None:
    for command in commands:
        execute(["bash", "-c", command])


def executePod(
        name: str, args: ExecArgs, image: ExecArgs, envArgs: ExecArgs) -> None:
    podInfo = podmanInspect("container", name)
    if podInfo:
        if podInfo["State"]["Status"].lower() == "running":
            raise AlreadyRunning()
        log.info(f"Cleaning left-over container {name}")
        execute(["podman", "rm", name])
    execute(
        ["podman", "run", "--rm", "--name", name] + args + image + envArgs,
        cwd=Path('/'))


def killPod(name: str) -> None:
    pread(["podman", "kill", name])
