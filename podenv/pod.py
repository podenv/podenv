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
from contextlib import contextmanager
from pathlib import Path
from typing import Any, Dict, Generator, List, Optional, Set
from subprocess import Popen, PIPE
try:
    import selinux  # type: ignore
    HAS_SELINUX = True
except ImportError:
    HAS_SELINUX = False

from podenv.env import Env, ExecArgs, Info, Runtime

log = logging.getLogger("podenv")
BuildId = str
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


def now() -> str:
    return time.strftime(TZFormat)


def age(date: str) -> float:
    return time.time() - time.mktime(time.strptime(date, TZFormat))


def updated(info: Info, maxAge: int = 3600 * 24) -> bool:
    lastUpdate = info.get("updated")
    if isinstance(lastUpdate, str):
        return age(lastUpdate) < maxAge
    raise RuntimeError("Info has incorrect updated metadata: %s" % lastUpdate)


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


def buildahRun(buildId: BuildId, command: str) -> None:
    execute(["buildah", "run", "--network", "host", buildId] +
            shlex.split(command))


def buildahConfig(buildId: BuildId, config: str) -> None:
    execute(["buildah", "config"] + shlex.split(config) + [buildId])


def buildahCommit(buildId: BuildId, tagRef: str) -> None:
    execute(["buildah", "commit", buildId, tagRef])


def buildahCommitNow(buildId: BuildId, tagRef: str) -> None:
    nowRef = "%s:%s" % (tagRef, now().replace(':', '-'))
    buildahCommit(buildId, nowRef)
    execute(["podman", "tag", nowRef, "%s:latest" % tagRef])


class ContainerImage(Runtime):
    def __init__(
            self, cacheDir: Path, fromRef: str, localName: str = "") -> None:
        self.fromRef = fromRef
        self.name = localName if localName else fromRef.replace(
            ':', '-').replace('/', '_')
        super().__init__(Path(str(cacheDir / self.name).rstrip('/') + '.json'))

    def __repr__(self) -> str:
        return self.name

    def getExecName(self) -> str:
        return self.name

    def create(self) -> None:
        with buildah(self.fromRef) as buildId:
            for command in [self.commands["update"],
                            "useradd -u 1000 -m user",
                            "mkdir /run/user/1000",
                            "chown 1000:1000 /run/user/1000",
                            "mkdir /run/user/0",
                            "chown 0:0 /run/user/0"]:
                buildahRun(buildId, command)
            for config in ["--author='%s'" % os.environ["USER"],
                           "--created-by podenv"]:
                buildahConfig(buildId, config)
            buildahCommit(buildId, self.name)
        self.updateInfo(dict(created=now(),
                             updated=now(),
                             packages=[],
                             fromRef=self.fromRef))

    def update(self) -> None:
        for retry in range(3):
            try:
                with buildah(self.name) as buildId:
                    updateOutput = pread(
                        ["buildah", "run", "--network", "host", buildId] +
                        shlex.split(self.commands["update"]))
                    print(updateOutput)
                    if "Nothing to do." in updateOutput:
                        break
                    buildahCommitNow(buildId, self.name)
                    break
            except RuntimeError:
                if retry == 2:
                    raise
                log.warning("Retrying failed update...")
        self.updateInfo(dict(updated=now()))

    def install(self, packages: Set[str]) -> None:
        with buildah(self.name) as buildId:
            buildahRun(buildId, self.commands["install"] + " ".join(packages))
            buildahCommitNow(buildId, self.name)
            self.updateInfo(dict(packages=list(packages.union(
                self.info["packages"]))))


def setupRuntime(env: Env, cacheDir: Path) -> str:
    """Ensure image is ready."""
    cacheDir = cacheDir.expanduser()
    if not cacheDir.exists():
        cacheDir.mkdir()

    if env.image and env.rootfs:
        raise RuntimeError(
            f"{env.name}: both image and rootfs can't be defined")

    if env.rootfs:
        raise RuntimeError("rootfs is not implemented")
    elif env.image:
        env.runtime = ContainerImage(cacheDir, env.image)

    if not env.runtime:
        raise NotImplementedError()

    if not env.runtime.exists():
        log.info(f"Creating {env.runtime}")
        env.runtime.create()
    else:
        env.runtime.loadInfo()

    if env.autoUpdate and not updated(env.runtime.info):
        log.info(f"Updating {env.runtime}")
        env.runtime.update()

    return env.runtime.getExecName()


def configureRuntime(env: Env, imageName: str, packages: List[str]) -> None:
    if not env.runtime:
        raise RuntimeError("Env has no metadata")
    imagePackages = env.runtime.info.get("packages")
    if not isinstance(imagePackages, list):
        raise RuntimeError("Invalid packages metadata")
    envPackages = set(env.packages)
    if packages:
        # Add user provided extra packages
        envPackages.update(packages)

    need = envPackages.difference(imagePackages)
    if need:
        log.info(f"Installing {need}")
        env.runtime.install(need)


def setupInfraNetwork(networkName: str, imageName: str, dns: str) -> None:
    """Setup persistent infra pod"""
    try:
        executePod("net-" + networkName,
                   ["--detach"] + [f"--dns={dns}"] if dns else [],
                   imageName, ["sleep", "Inf"])
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
        cacheDir: Path = Path("~/.cache/podenv")) -> str:
    imageName = setupRuntime(env, cacheDir)
    configureRuntime(env, imageName, packages)
    if env.provides.get("network"):
        setupInfraNetwork(env.provides["network"], imageName, env.dns)

    for containerPath, hostPath in sorted(env.ctx.mounts.items()):
        if not hostPath.exists() and str(hostPath).startswith(str(env.runDir)):
            setupRunDir(env)
            if hostPath.name == "tmp":
                hostPath.mkdir(mode=0o1777)
                # TODO: check why mkdir mode results in 1774
                hostPath.chmod(0o1777)
            else:
                hostPath.mkdir(mode=0o755, parents=True)

    return imageName


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
