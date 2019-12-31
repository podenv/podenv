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

import fcntl
import logging
import json
from contextlib import contextmanager
from pathlib import Path
from typing import Any, Dict, Generator, List, Optional, Set, Tuple
from subprocess import Popen, PIPE

from podenv.security import selinux, HAS_SELINUX
from podenv.context import UserNotif, BuildContext, DesktopEntry, ExecArgs, \
    Volume, Volumes, ExecContext


log = logging.getLogger("podenv")


class AlreadyRunning(Exception):
    pass


def prettyCmd(argv: ExecArgs) -> str:
    return " ".join(map(
            lambda arg: (lambda x: str(x if arg else "''"))(arg)
            if " " not in arg else f"'{arg}'", argv))


def execute(
        args: ExecArgs,
        cwd: Optional[Path] = None,
        textOutput: bool = False) -> Optional[str]:
    log.debug("Running %s" % prettyCmd(args))
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
        raise RuntimeError("Failed to run %s" % prettyCmd(args))
    if textOutput:
        output: str = stdout.decode('utf-8')
        return output
    return None


def desktopNotification(msg: str, color: str = "92") -> None:
    if Popen(["notify-send", msg]).wait():
        log.warning("Couldn't send notification")


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


def podmanExists(objType: str, name: str) -> bool:
    try:
        execute(["podman", objType, "exists", name])
        return True
    except RuntimeError:
        return False


def isSelinux() -> bool:
    return HAS_SELINUX and Path("/sys/fs/selinux/").exists()


def getSelinuxLabel(ctx: ExecContext) -> str:
    return "system_u:object_r:container_file_t:s0"


@contextmanager
def lock(name: Path) -> Generator[None, None, None]:
    lock_file = open(str(name) + '.lock', "w")
    fcntl.flock(lock_file, fcntl.LOCK_EX)
    try:
        yield None
    finally:
        fcntl.flock(lock_file, fcntl.LOCK_UN)


def setupInfraNetwork(
        networkName: str, imageName: str, ctx: ExecContext) -> None:
    """Setup persistent infra pod"""
    try:
        args = ["--detach"]
        if ctx.uidmaps:
            args.extend(ctx.getUidMaps())
        if ctx.dns:
            args.append(f"--dns={ctx.dns}")
        args.extend(ctx.getHosts())

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


def setupRunDir(ctx: ExecContext) -> None:
    path = ctx.runDir
    if path and not path.exists():
        if not path.parent.exists():
            path.parent.mkdir(mode=0o700)
        path.mkdir(mode=0o700)
        if isSelinux():
            selinux.chcon(str(path), getSelinuxLabel(ctx))


def setupVolumes(volumes: Volumes) -> None:
    """Create volume first to ensure proper owner"""
    volumesList = readProcessJson(
        ["podman", "volume", "ls", "--format", "json"])
    existingVolumes: Set[str] = set()
    if volumesList:
        for volume in volumesList:
            existingVolumes.add(volume['name'])
    for volume in volumes.values():
        if volume.name not in existingVolumes:
            log.info(f"Creating volume {volume.name}")
            execute(["podman", "volume", "create", volume.name])
        if volume.files:
            for file in volume.files:
                path = Path("~/.local/share/containers/storage/volumes/"
                            f"{volume.name}/_data/{file.name}").expanduser()
                if not path.exists():
                    log.info(f"Writting {path}")
                    path.write_text(file.content)


def build(filePath: Path, localName: str, ctx: Optional[BuildContext]) -> None:
    # TODO: add image build mount cache
    buildCommand = ["buildah", "bud"]
    if ctx and ctx.mounts:
        for containerPath, hostPath in ctx.mounts.items():
            hostPath = hostPath.expanduser()
            if not hostPath.exists():
                hostPath.mkdir(parents=True, exist_ok=True)
                if isSelinux():
                    selinux.chcon(
                        str(hostPath), "system_u:object_r:container_file_t:s0")
            if str(containerPath).startswith("~/"):
                containerPath = Path("/root") / str(containerPath)[2:]
            buildCommand.append(f"--volume={hostPath}:{containerPath}")
    with lock(filePath):
        execute(buildCommand +
                ["-f", filePath.name, "-t", localName, str(filePath.parent)])


def getLocalName(cache: Path, image: str, update: bool) -> Tuple[str, Path]:
    localName = image.replace("localhost/", "")
    localFileName = localName.replace("/", "-")
    if not update:
        localFileName = "Containerfile." + localFileName
    else:
        localFileName = "Containerfile." + localFileName + "-update"
    return (localName, (cache / "containerfiles" / localFileName).expanduser())


def setupContainerFile(
        userNotif: UserNotif,
        ctx: ExecContext,
        rebuild: bool,
        inPlace: bool,
        cacheDir: Path) -> None:
    """Ensure the container image is consistent with the containerfile"""
    if not ctx.containerFile:
        raise RuntimeError(f"{ctx.name}: container-file required")

    localName, localFile = getLocalName(cacheDir, ctx.imageName, update=False)
    containerFileCopy: Optional[str] = None

    buildReasons: List[str] = []
    if rebuild:
        buildReasons.append("--rebuild set")
    if not localFile.exists():
        buildReasons.append(f"{localFile} doesn't exists")
    elif localFile.read_text().strip() != ctx.containerFile.strip():
        if inPlace:
            # TODO: generalize this
            containerFileCopy = ctx.containerFile
            ctx.containerFile = "\n".join([
                f"FROM {localName}",
                ctx.containerFile.split("\n")[-1]
            ])
        else:
            # TODO: show diff?
            ...
        buildReasons.append(f"{localFile} content differ")
    if not buildReasons and not podmanExists("image", ctx.imageName):
        buildReasons.append(f"{ctx.imageName} doesn't exist in the store")

    if buildReasons:
        tmpFile = Path(str(localFile) + ".tmp")
        tmpFile.parent.mkdir(parents=True, exist_ok=True)
        tmpFile.write_text(ctx.containerFile)
        userNotif(f"Building {ctx.imageName} with {tmpFile} because: " +
                  ", ".join(buildReasons))
        try:
            build(tmpFile, localName, ctx.imageBuildCtx)
        except RuntimeError as e:
            raise RuntimeError(f"Build of {tmpFile} failed: " + str(e))
        if containerFileCopy:
            localFile.write_text(containerFileCopy)
        else:
            tmpFile.rename(localFile)


def updateContainerFile(
        userNotif: UserNotif,
        ctx: ExecContext,
        cacheDir: Path) -> None:
    (_, containerFile) = getLocalName(cacheDir, ctx.imageName, update=False)
    if not containerFile.exists():
        raise RuntimeError(f"{ctx.name}: hasn't been built yet")
    if not ctx.containerUpdate:
        raise RuntimeError(f"{ctx.name}: doesn't have a container-update")

    localName, localFile = getLocalName(cacheDir, ctx.imageName, update=True)

    if not localFile.exists() \
       or localFile.read_text() != ctx.containerUpdate:
        localFile.parent.mkdir(parents=True, exist_ok=True)
        localFile.write_text(ctx.containerUpdate)
    build(localFile, localName, ctx.imageBuildCtx)


def pullImage(userNotif: UserNotif, imageName: str, force: bool) -> str:
    if force or not podmanExists("image", imageName):
        userNotif(f"Pulling {imageName}")
        execute(["podman", "pull", imageName])
    return imageName


def updateImage(userNotif: UserNotif,
                ctx: ExecContext,
                cacheDir: Path) -> None:
    if ctx.containerUpdate:
        updateContainerFile(userNotif, ctx, cacheDir)
    else:
        try:
            pullImage(userNotif, ctx.imageName, True)
        except RuntimeError:
            userNotif(f"{ctx.imageName} doesn't seem to exist, "
                      "maybe there is a typo in the `image` attribute?")
            raise


def setupImage(userNotif: UserNotif,
               ctx: ExecContext,
               rebuild: bool,
               inPlace: bool,
               cacheDir: Path) -> None:
    if ctx.imageName.startswith("localhost/") and ctx.containerFile:
        setupContainerFile(userNotif, ctx, rebuild, inPlace, cacheDir)
    else:
        try:
            pullImage(userNotif, ctx.imageName, rebuild)
        except RuntimeError:
            userNotif(f"{ctx.imageName} doesn't seem to exist. "
                      "if you provided a `container-file` with a custom "
                      "`image` name, make sure to prefix the name with "
                      "'localhost/' to let podenv build it for you.")
            raise


def setupPod(
        userNotif: UserNotif,
        ctx: ExecContext,
        rebuild: bool) -> None:

    if ctx.network and ctx.network != "host" and \
       not ctx.network.startswith("container:"):
        setupInfraNetwork(ctx.network, ctx.imageName, ctx)

    if ctx.volumes:
        setupVolumes(ctx.volumes)

    if ctx.desktop:
        setupDesktopFile(ctx.desktop)

    for containerPath, hostPath in sorted(ctx.mounts.items()):
        if isinstance(hostPath, Path):
            hostPath = hostPath.expanduser().resolve()
            if not hostPath.exists() and str(hostPath).startswith(
                    str(ctx.runDir)):
                setupRunDir(ctx)
                if hostPath.name == "tmp":
                    hostPath.mkdir(mode=0o1777)
                    # TODO: check why mkdir mode results in 1774
                    hostPath.chmod(0o1777)
                else:
                    hostPath.mkdir(mode=0o755, parents=True)
            elif not hostPath.exists():
                hostPath.mkdir(mode=0o755, parents=True)
                if isSelinux():
                    selinux.chcon(str(hostPath), getSelinuxLabel(ctx))
        if ctx.runDir and ctx.runDir.exists() and \
           str(containerPath).startswith(str(ctx.home) + "/") and (
               (isinstance(hostPath, Path) and hostPath.is_dir()) or
               (isinstance(hostPath, Volume))):
            # Need to create parent dir, otherwise podman creates them as root
            tmpPath = ctx.runDir / "home" / str(containerPath).replace(
                str(ctx.home) + "/", "")
            tmpPath.mkdir(parents=True, exist_ok=True)


def executeHostTasks(commands: List[str]) -> None:
    for command in commands:
        execute(["bash", "-c", command])


def executePod(
        name: str, args: ExecArgs, image: str, envArgs: ExecArgs) -> None:
    podInfo = podmanInspect("container", name)
    if podInfo:
        if podInfo["State"]["Status"].lower() == "running":
            raise AlreadyRunning()
        log.info(f"Cleaning left-over container {name}")
        execute(["podman", "rm", name])
    execute(
        ["podman", "run", "--rm", "--name", name] + args + [image] + envArgs,
        cwd=Path('/'))


def killPod(name: str) -> None:
    pread(["podman", "kill", name])
