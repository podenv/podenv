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

import copy
import os
import shlex
from sys import stderr
from dataclasses import dataclass, field, fields
from pathlib import Path
from typing import Any, Dict, List, Optional, Tuple, Union


import podenv.capabilities as Cap
import podenv.tasks
from podenv.security import selinux, HAS_SELINUX
from podenv.context import BuildContext, DesktopEntry, ExecArgs, ExecContext, \
    File, ContainerPath, HostPath, Mounts, Task, User, Volume, Volumes


HostName = str
HostIP = str
Hosts = Dict[HostName, HostIP]


@dataclass
class Env:
    """The user provided container representation"""
    name: str = field(metadata=dict(
        doc="The name of the environment"))
    description: Optional[str] = field(default="", metadata=dict(
        doc="Environment description"))
    url: Optional[str] = field(default="", metadata=dict(
        doc="Application home page"))

    capabilities: Dict[str, bool] = field(
        default_factory=dict, metadata=dict(doc="List of capabilities"))

    image: str = field(default="", metadata=dict(
        doc="The container image reference"))

    containerFile: Optional[Union[str, List[Task]]] = field(
        default=None, metadata=dict(doc="Containerfile content"))
    containerUpdate: Optional[Union[str, List[Task]]] = field(
        default=None, metadata=dict(doc="Containerfile update content"))

    packages: Optional[List[str]] = field(default=None, metadata=dict(
        doc="List of required packages"))

    user: Optional[User] = field(default=None, metadata=dict(
        doc="Container user information"))
    command: Optional[ExecArgs] = field(default=None, metadata=dict(
        doc="Container starting command"))
    preTasks: Optional[List[Task]] = field(default=None, metadata=dict(
        doc="List of ansible like command to run before the command"))
    postTasks: Optional[List[Task]] = field(default=None, metadata=dict(
        doc="List of ansible like command to run after the pod exited"))
    workDir: Optional[Path] = field(default=None, metadata=dict(
        doc="The container workdir"))

    environ: Optional[Dict[str, str]] = field(default=None, metadata=dict(
        doc="User environ(7)"))
    syscaps: Optional[List[str]] = field(default=None, metadata=dict(
        doc="List of system capabilities(7)"))
    sysctls: Optional[List[str]] = field(default=None, metadata=dict(
        doc="List of sysctl(8)"))
    volumes: Optional[Volumes] = field(default=None, metadata=dict(
        doc="List of volumes"))
    mounts: Optional[Mounts] = field(default=None, metadata=dict(
        doc="Extra mountpoints"))

    network: Optional[str] = field(default=None, metadata=dict(
        doc="Name of a shared network"))
    addHosts: Optional[Dict[str, str]] = field(default=None, metadata=dict(
        doc="Custom hostname,ip to configure in the container"))
    ports: Optional[List[str]] = field(default=None, metadata=dict(
        doc="List of port to expose on the host"))
    dns: Optional[str] = field(default=None, metadata=dict(
        doc="A custom DNS server"))
    hostname: Optional[str] = field(default=None, metadata=dict(
        doc="Container hostname"))

    home: Optional[str] = field(default=None, metadata=dict(
        doc="Container home path mount"))

    desktop: Optional[DesktopEntry] = field(default=None, metadata=dict(
        doc="A desktop launcher entry file definition"))

    # Internal attribute
    original: Optional[Any] = field(default=None, metadata=dict(internal=True))
    envName: str = field(default="", metadata=dict(internal=True))
    buildCtx: BuildContext = field(default_factory=BuildContext, metadata=dict(
        internal=True))
    fileStr: str = field(default="", metadata=dict(internal=True))
    updateFileStr: str = field(default="", metadata=dict(internal=True))

    def __str__(self) -> str:
        return self.name

    def __repr__(self) -> str:
        # Format object by removing null attribute
        activeFields: List[str] = []
        for f in fields(Env):
            if f.metadata \
               and not f.metadata.get('internal', False):
                value = str(self.__dict__[f.name])
                if value:
                    if '\n' in value:
                        value = f'"""{value}"""'
                    elif ' ' in value:
                        value = f'"{value}"'
                    activeFields.append(f"{f.name}={value}")
        return "Env(%s)" % ", ".join(activeFields)


def loadEnv(schema: Any, debug: bool = False) -> Env:
    """Convert input config to python Env object"""

    # Copy the original schema in debug mode
    if debug:
        schema['original'] = copy.deepcopy(schema)

    # Do early validation first
    if not schema.get('name'):
        raise RuntimeError(
            f"Environment is missing a `name`: {schema}")

    if not schema.get('image') and not schema.get('container-file'):
        raise RuntimeError("{schema['name']}: requires an `image` name or "
                           "a `container-file` content")

    # Ensure default cap
    schema.setdefault('capabilities', {})
    for cap in ("selinux", "seccomp"):
        if schema['capabilities'].get(cap) is None:
            schema['capabilities'][cap] = True

    # Assume image name is env name
    if not schema.get('image'):
        schema['image'] = "localhost/podenv/" + schema['name']

    # Convert container-file tasks to str
    for (key, keyStr) in (("container-file", "fileStr"),
                          ("container-update", "updateFileStr")):
        # Value can be 'str', List[str], Task, List[Task]
        if schema.get(key):
            schema[keyStr] = "\n".join(map(
                lambda elem: (podenv.tasks.containerCommand(elem)
                              if isinstance(elem, dict) else str(elem)),
                schema[key]
                if isinstance(schema[key], list) else [schema[key]]))

    if schema.get('updateFileStr'):
        if any(filter(lambda x: str(x).startswith("FROM "),
                      schema['updateFileStr'].split('\n'))):
            raise RuntimeError("{schema['name']}: container-update "
                               "can't have FROM statement")
        schema['updateFileStr'] = f"FROM {schema['image']}\n" + schema[
            'updateFileStr']

    # Convert to camelCase
    for key in ("pre-tasks", "post-tasks", "work-dir",
                "container-file", "container-update"):
        if schema.get(key):
            x, xs = key.split('-', 1)
            camelKey = x + ''.join(map(str.capitalize, xs.split('-')))
            schema[camelKey] = schema.pop(key)

    # Transform complex types
    def hostsToMap(hosts: List[Dict[str, str]]) -> Hosts:
        def convert(d: Dict[str, str]) -> Tuple[HostName, HostIP]:
            return (d["Name"], d["IP"])
        return dict(map(convert, hosts))

    def mountsToMap(mounts: List[Dict[str, str]]) -> Mounts:
        def convert(d: Dict[str, str]) -> Tuple[ContainerPath, HostPath]:
            return (Path(d["container-path"]), Path(d["host-path"]))
        return dict(map(convert, mounts))

    def strMapToPathsMap(mounts: Dict[str, str]) -> Mounts:
        def convert(item: Tuple[str, str]) -> Tuple[ContainerPath, HostPath]:
            return (Path(item[0]), Path(item[1] if item[1] else item[0]))
        return dict(map(convert, mounts.items()))

    FileSchema = Dict[str, str]
    VolumeSchema = Dict[str, Union[str, List[FileSchema]]]

    def volumesToMap(volumes: List[VolumeSchema]) -> Volumes:
        def convertFiles(d: FileSchema) -> File:
            return File(d['name'], d['content'])

        def convert(d: VolumeSchema) -> Tuple[ContainerPath, Volume]:
            if not isinstance(d['name'], str):
                raise RuntimeError("Invalid volume name")
            if not isinstance(d['container-path'], str):
                raise RuntimeError("Invalid volume path")
            d.setdefault('files', [])
            return (Path(d["container-path"]), Volume(
                d['name'],
                bool(d.get('read-only', False)),
                list(map(convertFiles, d['files']))
                if isinstance(d['files'], list) else []))
        return dict(map(convert, volumes))

    def strMapToVolumesMap(volumes: Dict[str, str]) -> Volumes:
        def convert(item: Tuple[str, str]) -> Tuple[ContainerPath, Volume]:
            return (Path(item[0]), Volume(item[1]))
        return dict(map(convert, volumes.items()))

    def asList(obj: Union[str, List[str]]) -> List[str]:
        if isinstance(obj, str):
            return shlex.split(obj)
        return list(map(str, obj))

    if schema.get("mounts"):
        if isinstance(schema["mounts"], list):
            schema['mounts'] = mountsToMap(schema["mounts"])
        else:
            schema['mounts'] = strMapToPathsMap(schema["mounts"])
    if schema.get("volumes"):
        if isinstance(schema["volumes"], list):
            schema['volumes'] = volumesToMap(schema["volumes"])
        else:
            schema['volumes'] = strMapToVolumesMap(schema["volumes"])

    if schema.get("add-hosts"):
        schema['addHosts'] = hostsToMap(schema.pop("add-hosts"))
    if schema.get('work-dir'):
        schema['workDir'] = Path(schema.pop('work-dir'))

    if schema.get("build-env"):
        schema['buildCtx'] = BuildContext(
            mounts=mountsToMap(schema.pop("build-env")["mounts"]))
    if schema.get("desktop"):
        schema['desktop'] = DesktopEntry(
            envName=schema['name'],
            terminal=schema['capabilities'].get("terminal", False),
            **schema["desktop"])

    if schema.get('user'):
        user = schema.pop('user')
        schema['user'] = User(user['name'], Path(user['home']), user['uid'])
    if schema.get("command"):
        schema['command'] = asList(schema["command"])
    if schema.get("packages"):
        schema['packages'] = asList(schema["packages"])
    schema['envName'] = schema['name']
    return Env(**schema)


def validateEnv(env: Env, ctx: ExecContext) -> None:
    """Sanity check and warn user about missing setting"""
    def warn(msg: str) -> None:
        print(f"\033[93m{msg}\033[m", file=stderr)

    # Check if SELinux will block socket access
    if env.capabilities.get("selinux"):
        for cap in ("x11", "tun", "pulseaudio"):
            if env.capabilities.get(cap):
                warn(
                    f"SELinux is disabled because capability '{cap}' need "
                    "extra type enforcement that are not currently supported.")
                Cap.selinuxCap(False, ctx)
                env.capabilities["selinux"] = False

    # Check for uid permissions
    if not env.capabilities.get("root") and not env.capabilities.get("uidmap"):
        for cap in ("x11", "pulseaudio", "ssh", "gpg"):
            if env.capabilities.get(cap):
                warn(
                    f"UIDMap is required because '{cap}' need "
                    "DAC access to the host file")
                Cap.uidmapCap(True, ctx)
                break

    # Check for system capabilities
    if env.capabilities.get("tun") and "NET_ADMIN" not in ctx.syscaps:
        warn(f"NET_ADMIN capability is needed by the tun device")
        ctx.syscaps.append("NET_ADMIN")

    # Check mount points labels
    if env.capabilities.get("selinux") and HAS_SELINUX:
        label = "container_file_t"
        for hostPath in ctx.mounts.values():
            if isinstance(hostPath, Volume):
                continue
            hostPath = hostPath.expanduser().resolve()
            if hostPath.exists() and \
               selinux.getfilecon(str(hostPath))[1].split(':')[2] != label:
                warn(f"SELinux is disabled because {hostPath} doesn't have "
                     f"the {label} label. To set the label run: "
                     f"chcon -Rt {label} {hostPath}")
                Cap.selinuxCap(False, ctx)

    # Check mount points permissions
    for hostPath in ctx.mounts.values():
        if isinstance(hostPath, Volume):
            if not hostPath.readOnly and not env.capabilities.get("uidmap"):
                warn("UIDMap is required for rw volume")
                Cap.uidmapCap(True, ctx)
                break
            continue
        hostPath = hostPath.expanduser().resolve()
        if hostPath.exists() and not os.access(str(hostPath), os.R_OK):
            warn(f"{hostPath} is not readable by the current user.")

    if env.capabilities.get("mount-home") and not env.capabilities.get(
            "uidmap"):
        warn("UIDMap is required for mount-home")
        Cap.uidmapCap(True, ctx)


def prepareEnv(env: Env, cliArgs: List[str]) -> ExecContext:
    """Generate podman exec args based on capabilities"""
    ctx = ExecContext(
        name=env.name,
        imageName=env.image,
        volumes=env.volumes,
        desktop=env.desktop,
        imageBuildCtx=env.buildCtx,
        network=env.network,
        dns=env.dns,
        hostname=env.hostname,
        cwd=env.workDir,
        user=env.user,
        runDir=Path("/tmp/podenv") / env.name,
        commandArgs=env.command if env.command else [],
        containerFile=env.fileStr,
        containerUpdate=env.updateFileStr,
    )
    # Apply capabilities
    for name, _, capability in Cap.Capabilities:
        capability(env.capabilities.get(name, False), ctx)

    # Add local env attribute to the execution context
    if env.ports:
        for port in env.ports:
            ctx.podmanArgs.append(f"--publish={port}")
    if env.syscaps:
        ctx.syscaps.extend(env.syscaps)
    if env.sysctls:
        ctx.sysctls.extend(env.sysctls)
    if env.environ:
        ctx.environ.update(env.environ)
    if env.addHosts:
        ctx.addHosts.update(env.addHosts)

    # Convert env relative path to absolute path
    if env.home:
        if not ctx.home:
            raise RuntimeError("home attribute needs a user defintion")
        ctx.mounts[ctx.home] = Path(env.home).expanduser().resolve()
    if env.mounts:
        for containerPath, hostPath in env.mounts.items():
            hostPath = hostPath.expanduser().resolve()
            if str(containerPath).startswith("~/"):
                if not ctx.home:
                    raise RuntimeError(
                        "mounts with ~/ path need a user defintion")
                ctx.mounts[
                    ctx.home / str(containerPath)[2:]] = hostPath
            else:
                ctx.mounts[containerPath] = hostPath
    if env.volumes:
        for containerPath, volume in env.volumes.items():
            if str(containerPath).startswith("~/"):
                if not ctx.home:
                    raise RuntimeError(
                        "volumes with ~/ path need a user defintion")
                ctx.mounts[ctx.home / str(containerPath)[2:]] = volume
            else:
                ctx.mounts[containerPath] = volume

    # Extend command args with args from the cli
    if env.capabilities.get('hostfiles'):
        # Replace any file argument in cliArgs
        newArgs: List[str] = []
        for arg in cliArgs:
            argPath = Path(arg).expanduser()
            if argPath.exists():
                if argPath.is_dir():
                    ctx.cwd = Path("/data")
                    ctx.mounts[ctx.cwd] = argPath
                else:
                    containerPath = Path("/tmp") / argPath.name
                    ctx.mounts[containerPath] = argPath
                    newArgs.append(str(containerPath))
                continue
            newArgs.append(arg)
        cliArgs = newArgs

    # Workaround unconfigured loopback
    if env.capabilities.get('local-network'):
        task: Task = dict(
            command="ip a a 127.0.0.1/32 dev lo; ip link set lo up")
        if not env.preTasks:
            env.preTasks = [task]
        else:
            env.preTasks = [task] + env.preTasks

    # Only use cli args when env isn't a shell
    if not ctx.commandArgs or ctx.commandArgs[-1] != "/bin/bash":
        for arg in cliArgs:
            ctx.commandArgs.append(arg)

    # Manage pre/post tasks
    if env.preTasks:
        # TODO: create a /tmp/start.sh script when preTasks are too long
        envPreTasks: ExecArgs = []
        for preTask in env.preTasks:
            command = podenv.tasks.taskToCommand(preTask)
            if command.startswith("run_local_host; "):
                ctx.hostPreTasks.append(command.split('; ', 1)[1])
            else:
                envPreTasks.append(command)
        if len(envPreTasks):
            ctx.commandArgs = [
                "bash", "-c", "; ".join(["set -e"] + envPreTasks) +
                "; exec \"" + "\" \"".join(ctx.commandArgs) + "\""]
    if env.postTasks:
        for postTask in env.postTasks:
            command = podenv.tasks.taskToCommand(postTask)
            if command.startswith("run_local_host; "):
                ctx.hostPostTasks.append(command.split('; ', 1)[1])
            else:
                raise RuntimeError("Only host post-task are supported")

    # Final sanity checks
    validateEnv(env, ctx)

    # OCI doesn't let you join a netns without the userns when using uidmap...
    if ctx.uidmaps and ctx.namespaces.get(
            "network", "").startswith("container:"):
        ctx.namespaces["userns"] = ctx.namespaces["network"]

    return ctx
