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
    Optional, Set, Tuple
from subprocess import Popen, PIPE
try:
    import selinux  # type: ignore
    HAS_SELINUX = True
except ImportError:
    HAS_SELINUX = False

from podenv.env import DesktopEntry, Env, ExecArgs, Info, Runtime, getUidMap

log = logging.getLogger("podenv")
BuildId = str
BuildSession = ContextManager[BuildId]
TZFormat = "%Y-%m-%dT%H:%M:%S"
DAY = 3600 * 24


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
    try:
        return "via" in pread(["ip", "route", "get", "1.1.1.1"])
    except RuntimeError:
        log.warning("Skipping update because host is not online")
    return False


def updated(lastUpdate: str, maxAge: int = DAY) -> bool:
    return age(lastUpdate) < maxAge


def outdated(path: Path, maxAge: int = DAY * 7) -> bool:
    return os.stat(path).st_mtime > (time.time() - maxAge)


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
    System = {
        "dnf": {
            "commands": {
                "install": "dnf install -y ",
                "update": "dnf update -y ",
            },
            "mounts": {
                "/var/cache/dnf": "~/.cache/podenv/dnf"
            }
        },
        "apt-get": {
            "commands": {
                "install": "apt-get install -y ",
                "update": "apt-get update -y "
            }
        },
        "emerge": {
            "mounts": {
                "/var/db/repos/gentoo": "~/.cache/podenv/portage",
            },
            "commands": {
                "install": "emerge -vaDNt --verbose-conflicts ",
                "update": "emerge -uvaDNt ",
                "pre-update": "emerge --sync --quiet",
            }
        },
    }

    def __init__(self, cacheDir: Path, name: str, fromRef: str):
        self.commands: Dict[str, str] = {}
        self.mounts: Dict[str, str] = {}
        self.info: Info = {}
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
        packages = self.info.get("packages", [])
        if not isinstance(packages, list):
            raise RuntimeError("Invalid packages metadata")
        return packages

    def needUpdate(self) -> bool:
        if self.info.get("updated") and isinstance(self.info["updated"], str):
            return not updated(self.info["updated"]) and canUpdate()
        raise RuntimeError("Invalid last updated metadata")

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
        self.commands = self.System[systemType]["commands"]
        self.mounts = self.System[systemType].get("mounts", {})

    def getSystemType(self, buildId: BuildId) -> str:
        systemType = self.info.get("system")
        if not systemType:
            for systemType in self.System:
                try:
                    self.runCommand(buildId, "sh -c 'type %s'" % systemType)
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

    def getSystemMounts(self) -> ExecArgs:
        """Mount points needed to manage the runtime image"""
        mounts = []
        for containerDir, hostDir in self.mounts.items():
            hostPath = Path(hostDir).expanduser().resolve()
            if not hostPath.exists():
                hostPath.mkdir(parents=True, exist_ok=True)
                if isSelinux():
                    selinux.chcon(
                        str(hostPath), "system_u:object_r:container_file_t:s0")
            mounts.extend(["-v", "%s:%s" % (
                hostPath, Path(containerDir).expanduser().resolve())])
        return mounts

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
                    systemType = self.getSystemType(buildId)
                    if self.commands.get("pre-update"):
                        self.runCommand(buildId, self.commands["pre-update"])
                    updateOutput = pread(
                        self.runCommandArgs(buildId) +
                        shlex.split(self.commands["update"]), live=True)
                    if "Nothing to do." in updateOutput:
                        break
                    self.commit(buildId)
                    break
            except RuntimeError:
                if retry == 2:
                    raise
                log.warning("Retrying failed update...")
        self.updateInfo(dict(updated=now(), system=systemType))

    def install(self, packages: Set[str]) -> None:
        with self.getSession() as buildId:
            systemType = self.getSystemType(buildId)
            self.runCommand(
                buildId, self.commands["install"] + " ".join(packages))
            self.commit(buildId)
        self.updateInfo(dict(system=systemType, packages=list(packages.union(
                self.info["packages"]))))

    def customize(self, commands: List[Tuple[str, str]]) -> None:
        knownHash: Set[str] = set(self.info.get("customHash", []))
        with self.getSession() as buildId:
            for commandHash, command in commands:
                execute(self.runCommandArgs(buildId) +
                        ["/bin/bash", "-c", command])
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
    def runCommand(self, buildId: BuildId, command: str) -> None:
        ...

    @abstractmethod
    def applyConfig(self, buildId: BuildId, config: str) -> None:
        ...

    @abstractmethod
    def commit(self, buildId: BuildId) -> None:
        ...


class ContainerImage(PodmanRuntime):
    def __init__(
            self, cacheDir: Path, fromRef: str, localName: str = "") -> None:
        self.localName: str = "localhost/podenv/" + fromRef.split(
            '/', 1)[-1].replace(':', '-')
        super().__init__(cacheDir,
                         fromRef.replace(':', '-').replace('/', '_'),
                         fromRef)

    def __repr__(self) -> str:
        return self.localName

    def getExecName(self) -> ExecArgs:
        return [self.localName]

    def getSession(self, create: bool = False) -> BuildSession:
        return buildah(self.fromRef if create else self.localName)

    def runCommand(self, buildId: BuildId, command: str) -> None:
        execute(self.runCommandArgs(buildId) + shlex.split(command))

    def runCommandArgs(self, buildId: BuildId) -> ExecArgs:
        return buildahRunCommand() + self.getSystemMounts() + [buildId]

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
                execute(["podman", "pull", self.fromRef])
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

    def runCommand(self, _: BuildId, command: str) -> None:
        if not self.rootfs.exists():
            if self.url:
                extractUrl(self.url, self.rootfs)
            else:
                raise RuntimeError(f"{self.rootfs}: does not exist")
        execute(self.runCommandArgs(_) + shlex.split(command))

    def runCommandArgs(self, _: BuildId) -> ExecArgs:
        return ["podman", "run", "--rm", "-t"] + \
            self.getSystemMounts() + self.getExecName()

    def applyConfig(self, buildId: BuildId, config: str) -> None:
        # Rootfs doesn't have metadata
        pass

    def commit(self, buildId: BuildId) -> None:
        # Rootfs doesn't have layer
        pass


def setupRuntime(env: Env, cacheDir: Path) -> ExecArgs:
    """Ensure image is ready."""
    cacheDir = cacheDir.expanduser()
    if not cacheDir.exists():
        cacheDir.mkdir(parents=True)

    if env.image and env.rootfs:
        raise RuntimeError(
            f"{env.name}: both image and rootfs can't be defined")

    if env.rootfs:
        env.runtime = RootfsDirectory(cacheDir, env.rootfs)
    elif env.image:
        env.runtime = ContainerImage(cacheDir, env.image)

    if not env.runtime:
        raise NotImplementedError()

    if not env.runtime.exists(env.autoUpdate):
        log.info(f"Creating {env.runtime}")
        env.runtime.create()
    else:
        env.runtime.loadInfo()

    return env.runtime.getExecName()


def configureRuntime(env: Env, packages: List[str]) -> None:
    if not env.runtime:
        raise RuntimeError("Env has no metadata")

    needCustomCommands: List[Tuple[str, str]] = []
    for customCommand in env.imageCustomizations:
        customCommandHash = hash(customCommand)
        if customCommandHash not in env.runtime.getCustomizations():
            needCustomCommands.append((customCommandHash, customCommand))
    if needCustomCommands:
        log.info(f"Customizing image {needCustomCommands}")
        env.runtime.customize(needCustomCommands)

    if env.autoUpdate and env.runtime.needUpdate():
        log.info(f"Updating {env.runtime}")
        env.runtime.update()

    imagePackages = env.runtime.getInstalledPackages()
    envPackages = set(env.packages)
    if packages:
        # Add user provided extra packages
        envPackages.update(packages)

    need = envPackages.difference(imagePackages)
    if need:
        log.info(f"Installing {need}")
        env.runtime.install(need)


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
        env: Env,
        packages: List[str],
        cacheDir: Path = Path("~/.cache/podenv")) -> ExecArgs:
    imageName = setupRuntime(env, cacheDir)
    configureRuntime(env, packages)
    if env.provides.get("network"):
        setupInfraNetwork(env.provides["network"], imageName, env)

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
                         str(homeDir)])
            else:
                for containerHomePath, content in overlay.items():
                    hostPath = homeDir / containerHomePath
                    if not hostPath.exists() or \
                       hostPath.read_text() != content:
                        hostPath.write_text(content)

    return imageName


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
