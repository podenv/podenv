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

import logging
import json
import os
import shlex
import time
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

from podenv.env import Env, ExecArgs, Info, Runtime, getUidMap

log = logging.getLogger("podenv")
BuildId = str
BuildSession = ContextManager[BuildId]
TZFormat = "%Y-%m-%dT%H:%M:%S"


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
    if proc.wait():
        raise RuntimeError("Failed to run %s" % " ".join(args))
    if textOutput:
        output: str = stdout.decode('utf-8')
        return output
    return None


def pread(args: ExecArgs, cwd: Optional[Path] = None) -> str:
    output = execute(args, cwd, True)
    if output:
        return output
    return ""


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


def updated(lastUpdate: str, maxAge: int = 3600 * 24) -> bool:
    return age(lastUpdate) < maxAge


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


def buildahRunCommand(buildId: BuildId) -> ExecArgs:
    return ["buildah", "run", "--network", "host", "--user", "0:0", buildId]


def buildahRun(buildId: BuildId, command: str) -> None:
    execute(buildahRunCommand(buildId) + shlex.split(command))


def buildahConfig(buildId: BuildId, config: str) -> None:
    execute(["buildah", "config"] + shlex.split(config) + [buildId])


def buildahCommit(buildId: BuildId, tagRef: str) -> None:
    execute(["buildah", "commit", buildId, tagRef])


class PodmanRuntime(Runtime):
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

    def __init__(self, cacheDir: Path, name: str, fromRef: str):
        self.commands: Dict[str, str] = {}
        self.info: Info = {}
        self.fromRef: str = fromRef
        self.name: str = name
        self.metadataPath = Path(
            str(cacheDir / self.name).rstrip('/') + '.json')

    def exists(self) -> bool:
        return self.metadataPath.exists()

    def loadInfo(self) -> None:
        self.info = json.loads(self.metadataPath.read_text())

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

    def getExecName(self) -> ExecArgs:
        return [self.name]

    def getSystemType(self, buildId: BuildId) -> str:
        systemType = self.info.get("system")
        if not systemType:
            for systemType, checkCommand in (
                    ("rpm", "sh -c 'type dnf'"),
                    ("apt", "sh -c 'type apt'")):
                try:
                    self.runCommand(buildId, checkCommand)
                    break
                except RuntimeError:
                    log.debug("%s failed", checkCommand)
            else:
                raise RuntimeError("Couldn't discover system type")
        if not isinstance(systemType, str):
            # Mypy is confused by the type from the for loop
            raise RuntimeError("Invalid systemType")
        self.commands = self.System[systemType]["commands"]
        return systemType

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
            self.commit(buildId, self.name)
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
                    updateOutput = pread(
                        self.runCommandArgs(buildId) +
                        shlex.split(self.commands["update"]))
                    print(updateOutput)
                    if "Nothing to do." in updateOutput:
                        break
                    self.commit(buildId, self.name)
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
            self.commit(buildId, self.name)
        self.updateInfo(dict(system=systemType, packages=list(packages.union(
                self.info["packages"]))))

    def customize(self, commands: List[Tuple[str, str]]) -> None:
        knownHash: Set[str] = set(self.info.get("customHash", []))
        with self.getSession() as buildId:
            for commandHash, command in commands:
                execute(self.runCommandArgs(buildId) +
                        ["/bin/bash", "-c", command])
                knownHash.add(commandHash)
            self.commit(buildId, self.name)
        self.updateInfo(dict(customHash=list(knownHash)))

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
    def commit(self, buildId: BuildId, name: str) -> None:
        ...


class ContainerImage(PodmanRuntime):
    def __init__(
            self, cacheDir: Path, fromRef: str, localName: str = "") -> None:
        super().__init__(cacheDir,
                         localName if localName else fromRef.replace(
                             ':', '-').replace('/', '_'),
                         fromRef)

    def getSession(self, create: bool = False) -> BuildSession:
        return buildah(self.fromRef if create else self.name)

    def runCommand(self, buildId: BuildId, command: str) -> None:
        buildahRun(buildId, command)

    def runCommandArgs(self, buildId: BuildId) -> ExecArgs:
        return buildahRunCommand(buildId)

    def applyConfig(self, buildId: BuildId, config: str) -> None:
        buildahConfig(buildId, config)

    def commit(self, buildId: BuildId, name: str) -> None:
        tagRef = "%s:%s" % (name, now().replace(':', '-'))
        buildahCommit(buildId, tagRef)
        execute(["podman", "tag", tagRef, f"{name}:latest"])


class RootfsDirectory(PodmanRuntime):
    def __init__(self, cacheDir: Path, fromRef: str) -> None:
        super().__init__(cacheDir, os.path.basename(fromRef), fromRef)

    def getExecName(self) -> ExecArgs:
        return ["--rootfs", self.fromRef]

    def getSession(self, create: bool = False) -> BuildSession:
        @contextmanager
        def fakeSession() -> Generator[BuildId, None, None]:
            yield "noop"
        return fakeSession()

    def runCommand(self, _: BuildId, command: str) -> None:
        execute(self.runCommandArgs(_) + shlex.split(command))

    def runCommandArgs(self, _: BuildId) -> ExecArgs:
        return ["podman", "run", "--rm", "-t", "--rootfs", self.fromRef]

    def applyConfig(self, buildId: BuildId, config: str) -> None:
        # Rootfs doesn't have metadata
        pass

    def commit(self, buildId: BuildId, name: str) -> None:
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

    if not env.runtime.exists():
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

    for containerPath, hostPath in sorted(env.ctx.mounts.items()):
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
           str(containerPath).startswith(str(env.ctx.home) + "/"):
            # Need to create parent dir, otherwise podman creates them as root
            tmpPath = env.runDir / "home" / str(containerPath).replace(
                str(env.ctx.home) + "/", "")
            tmpPath.mkdir(parents=True, exist_ok=True)

    if env.overlaysDir:
        for overlay in env.overlays:
            overlayDir = env.overlaysDir / overlay
            if not overlayDir.exists():
                raise RuntimeError(f"{overlay} does not exists")
            homeDir = env.ctx.mounts.get(env.ctx.home)
            if not homeDir:
                raise RuntimeError(
                    "Overlays without home mount isn't supported")
            execute(["rsync", "--copy-links", "-a", str(overlayDir) + "/",
                     str(homeDir)])
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
