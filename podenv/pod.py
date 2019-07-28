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
import os
import shlex
import time
from contextlib import contextmanager
from pathlib import Path
from typing import Generator, List, Optional
from subprocess import Popen, PIPE

from podenv.env import Env, ExecArgs, Info, Runtime

log = logging.getLogger("podenv")
BuildId = str
TZFormat = "%Y-%m-%dT%H:%M:%S"


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


def now() -> str:
    return time.strftime(TZFormat)


def age(date: str) -> float:
    return time.time() - time.mktime(time.strptime(date, TZFormat))


def updated(info: Info, maxAge: int = 3600 * 24) -> bool:
    lastUpdate = info.get("updated")
    if isinstance(lastUpdate, str):
        return age(lastUpdate) < maxAge
    raise RuntimeError("Info has incorrect updated metadata: %s" % lastUpdate)


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

    def install(self, packages: List[str]) -> None:
        ...


def setupPod(env: Env, cacheDir: Path = Path("~/.cache/podenv")) -> None:
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

    if not updated(env.runtime.info):
        log.info(f"Updating {env.runtime}")
        env.runtime.update()


def executePod(args: ExecArgs) -> None:
    ...
