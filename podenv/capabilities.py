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
This module defines the capabilities
"""

import os
import re
from pathlib import Path
from typing import Tuple, List, Set, Optional
from podenv.context import ExecContext, Capability


def needUser(capability: str) -> None:
    raise RuntimeError(
        f"The `{capability}` requires an environment with a user attribute."
        "Alternatively uses the `root` capability.")


def rootCap(active: bool, ctx: ExecContext) -> None:
    "run as root"
    if active:
        ctx.home = Path("/root")
        ctx.xdgDir = Path("/run/user/0")
        ctx.username = "root"
    elif ctx.user:
        ctx.home = ctx.user.home
        ctx.xdgDir = Path(f"/run/user/{ctx.user.uid}")
        ctx.username = ctx.user.name


def uidmapCap(active: bool, ctx: ExecContext) -> None:
    "map host uid"
    ctx.uidmaps = active


def hostfilesCap(activate: bool, ctx: ExecContext) -> None:
    "enable host files access"
    # This caps is only used by the prepareEnv function
    ...


def privilegedCap(active: bool, ctx: ExecContext) -> None:
    "run as privileged container"
    ctx.privileged = active


def terminalCap(active: bool, ctx: ExecContext) -> None:
    "interactive mode"
    if active:
        ctx.interactive = True
        ctx.detachKeys = ""


def largeShmCap(active: bool, ctx: ExecContext) -> None:
    "mount a 4gb shm"
    ctx.shmsize = "4g" if active else None


def localNetworkCap(active: bool, ctx: ExecContext) -> None:
    "setup localhost network"
    # A workaround podman not configuring loopback
    if active:
        ctx.network = None
        ctx.syscaps.append("NET_ADMIN")


def networkCap(active: bool, ctx: ExecContext) -> None:
    "enable network"
    nsName = ""
    if ctx.network:
        if ctx.network == "host":
            nsName = "host"
        elif ctx.network.startswith("container:"):
            nsName = ctx.network
        else:
            nsName = f"container:net-{ctx.network}"

    if not active and not ctx.network:
        nsName = "none"

    if nsName:
        ctx.namespaces["network"] = nsName


def mountCwdCap(active: bool, ctx: ExecContext) -> None:
    "mount cwd to /data"
    if active:
        ctx.cwd = Path("/data")
        ctx.mounts[ctx.cwd] = Path()


def mountRunCap(active: bool, ctx: ExecContext) -> None:
    "mount home and tmp to host tmpfs"
    if active:
        if ctx.runDir is None:
            raise RuntimeError("runDir isn't set")
        if ctx.home and not ctx.mounts.get(ctx.home):
            ctx.mounts[ctx.home] = ctx.runDir / "home"
        if not ctx.mounts.get(Path("/tmp")):
            ctx.mounts[Path("/tmp")] = ctx.runDir / "tmp"


def mountHomeCap(active: bool, ctx: ExecContext) -> None:
    "mount home to host home"
    if active:
        if not ctx.home:
            return needUser("mount-home")
        ctx.mounts[ctx.home] = Path("~/").expanduser()


def ipcCap(active: bool, ctx: ExecContext) -> None:
    "share host ipc"
    if active:
        ctx.namespaces["ipc"] = "host"


def x11Cap(active: bool, ctx: ExecContext) -> None:
    "share x11 socket"
    if active:
        ctx.mounts[Path("/tmp/.X11-unix")] = Path("/tmp/.X11-unix")
        ctx.environ["DISPLAY"] = os.environ["DISPLAY"]


def pulseaudioCap(active: bool, ctx: ExecContext) -> None:
    "share pulseaudio socket"
    if active:
        if not ctx.xdgDir:
            return needUser("pulseaudio")
        ctx.mounts[Path("/etc/machine-id:ro")] = Path("/etc/machine-id")
        ctx.mounts[ctx.xdgDir / "pulse"] = \
            Path(os.environ["XDG_RUNTIME_DIR"]) / "pulse"
        # Force PULSE_SERVER environment so that when there are more than
        # one running, pulse client aren't confused by different uid
        ctx.environ["PULSE_SERVER"] = str(ctx.xdgDir / "pulse" / "native")


def gitCap(active: bool, ctx: ExecContext) -> None:
    "share .gitconfig and excludesfile"
    if active:
        if not ctx.home:
            return needUser("git")
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


def editorCap(active: bool, ctx: ExecContext) -> None:
    "setup editor env"
    if active:
        ctx.environ["EDITOR"] = os.environ.get("EDITOR", "vi")


def netrcCap(active: bool, ctx: ExecContext) -> None:
    "share ~/.netrc file"
    if active:
        if not ctx.home:
            return needUser("netrc")
        ctx.mounts[ctx.home / ".netrc"] = Path("~/.netrc")


def sshCap(active: bool, ctx: ExecContext) -> None:
    "share ssh agent and keys"
    if active:
        if not ctx.home:
            return needUser("ssh")
        if os.environ.get("SSH_AUTH_SOCK"):
            ctx.environ["SSH_AUTH_SOCK"] = os.environ["SSH_AUTH_SOCK"]
            sshSockPath = Path(os.environ["SSH_AUTH_SOCK"]).parent
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


def gpgCap(active: bool, ctx: ExecContext) -> None:
    "share gpg agent"
    if active:
        if not ctx.home or not ctx.xdgDir:
            return needUser("gpg")
        gpgSockDir = Path(os.environ["XDG_RUNTIME_DIR"]) / "gnupg"
        ctx.mounts[ctx.xdgDir / "gnupg"] = gpgSockDir
        ctx.mounts[ctx.home / ".gnupg"] = Path("~/.gnupg")


def webcamCap(active: bool, ctx: ExecContext) -> None:
    "share webcam device"
    if active:
        for device in list(filter(lambda x: x.startswith("video"),
                                  os.listdir("/dev"))):
            ctx.devices.append(Path("/dev") / device)


def alsaCap(active: bool, ctx: ExecContext) -> None:
    "share alsa device"
    if active:
        ctx.devices.append(Path("/dev/snd"))


def driCap(active: bool, ctx: ExecContext) -> None:
    "share graphic device"
    if active:
        ctx.devices.append(Path("/dev/dri"))


def nvidiaCap(active: bool, ctx: ExecContext) -> None:
    "share nvidia device"
    if active:
        for nv in ("nvidia0", "nvidiactl", "nvidia-modeset",
                   "nvidia-uvm", "nvidia-uvm-tools"):
            nvPath = Path("/dev") / nv
            if nvPath.exists():
                ctx.devices.append(nvPath)


def kvmCap(active: bool, ctx: ExecContext) -> None:
    "share kvm device"
    if active:
        ctx.devices.append(Path("/dev/kvm"))


def tunCap(active: bool, ctx: ExecContext) -> None:
    "share tun device"
    if active:
        ctx.devices.append(Path("/dev/net/tun"))


def selinuxCap(active: bool, ctx: ExecContext) -> None:
    "enable SELinux"
    if not active:
        ctx.seLinuxLabel = "disable"


def seccompCap(active: bool, ctx: ExecContext) -> None:
    "enable seccomp"
    if not active:
        ctx.seccomp = "unconfined"


def ptraceCap(active: bool, ctx: ExecContext) -> None:
    "enable ptrace"
    if active:
        ctx.syscaps.append("SYS_PTRACE")


def setuidCap(active: bool, ctx: ExecContext) -> None:
    "enable setuid"
    if active:
        for cap in ("SETUID", "SETGID"):
            ctx.syscaps.append(cap)


def foregroundCap(active: bool, ctx: ExecContext) -> None:
    "work around application that goes into background"
    if active:
        # TODO: add gui dialog support for terminal-less usage
        if not ctx.commandArgs:
            raise RuntimeError("foreground capability needs command")
        ctx.commandArgs = [
            "bash", "-c",
            "set -e; {command}; echo 'press ctrl-c to quit'; sleep Inf".format(
                command=";".join(ctx.commandArgs)
            )]


def camelCaseToHyphen(name: str) -> str:
    return re.sub('([A-Z]+)', r'-\1', name).lower()


Capabilities: List[Tuple[str, Optional[str], Capability]] = [
    (camelCaseToHyphen(func.__name__[:-3]), func.__doc__, func) for func in [
        rootCap,
        privilegedCap,
        terminalCap,
        hostfilesCap,
        largeShmCap,
        ipcCap,
        x11Cap,
        pulseaudioCap,
        gitCap,
        editorCap,
        netrcCap,
        sshCap,
        gpgCap,
        webcamCap,
        alsaCap,
        driCap,
        nvidiaCap,
        kvmCap,
        tunCap,
        seccompCap,
        selinuxCap,
        setuidCap,
        ptraceCap,
        localNetworkCap,
        networkCap,
        foregroundCap,
        mountCwdCap,
        mountHomeCap,
        mountRunCap,
        uidmapCap,
    ]]
ValidCap: Set[str] = set([cap[0] for cap in Capabilities])
