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
This module is the command line interface entrypoint.
"""

import argparse
import logging
import sys
from os import environ
from pathlib import Path
from typing import Dict

from podenv.config import loadConfig, loadEnv
from podenv.pod import killPod, setupPod, executeHostTasks, executePod, \
    desktopNotification, podmanExists
from podenv.env import Capabilities, Env, ExecArgs, UserNotif, prepareEnv, \
    cleanupEnv

log = logging.getLogger("podenv")


def usageParser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description="podenv - a podman wrapper")
    parser.add_argument("--verbose", action='store_true')
    parser.add_argument("--list", action='store_true',
                        help="List available environments")
    parser.add_argument("--shell", action='store_true',
                        help="Run bash instead of the profile command")
    parser.add_argument("--net", help="Set the network (host or env name)")
    parser.add_argument("-p", "--package", action='append',
                        help="Add a package to the environment")
    parser.add_argument("-e", "--environ", action='append',
                        help="Set an environ variable")
    parser.add_argument("-i", "--image",
                        help="Override the image name")
    parser.add_argument("-b", "--base",
                        help="Override the base environment name")
    parser.add_argument("-t", "--tag",
                        help="Set the image tag")
    for name, doc, _ in Capabilities:
        parser.add_argument(f"--{name}", action='store_true',
                            help=f"Enable capability: {doc}")
        parser.add_argument(f"--no-{name}", action='store_true',
                            help=f"Disable {name} capibility")
    parser.add_argument("env", nargs='?')
    parser.add_argument("args", nargs='*')
    return parser


def usage(args: ExecArgs) -> argparse.Namespace:
    return usageParser().parse_args(args)


def applyCommandLineOverride(args: argparse.Namespace, env: Env) -> None:
    """Mutate the environment with the command line override"""
    for name, _, _ in Capabilities:
        argName = name.replace('-', '_')
        if getattr(args, f"{argName}"):
            env.capabilities[name] = True
        if getattr(args, f"no_{argName}"):
            env.capabilities[name] = False
    if args.environ:
        for argsEnviron in args.environ:
            key, val = argsEnviron.split("=", 1)
            env.environ[key] = val
    if args.shell:
        env.capabilities["terminal"] = True
        env.command = ["/bin/bash"]
    if args.image:
        env.image = args.image
    if args.network:
        env.network = args.network


def setupLogging(debug: bool) -> None:
    loglevel = logging.DEBUG if debug else logging.INFO
    logging.basicConfig(
        format="%(asctime)s %(levelname)-5.5s %(name)s - "
               "\033[92m%(message)s\033[m",
        level=loglevel)


def fail(userNotif: UserNotif, msg: str, code: int = 1) -> None:
    if userNotif == desktopNotification:
        userNotif(msg)
    else:
        print(f"\033[91m{msg}\033[m")
    exit(code)


def getUserNotificationProc(verbose: bool) -> UserNotif:
    """Return a callable to notify the user"""
    if not sys.stdout.isatty():
        if environ.get("DBUS_SESSION_BUS_ADDRESS") or (
                environ.get("XDG_RUNTIME_DIR") and (
                    Path(environ["XDG_RUNTIME_DIR"]) / "bus").exists()):
            return desktopNotification
    elif verbose:
        return lambda msg: log.info(msg)
    return lambda msg: print(
        f"\033[92m{msg}\033[m", file=sys.stderr)


def listEnv(envs: Dict[str, Env]) -> None:
    maxEnvNameLen = max(map(len, envs.keys())) + 3
    maxParentNameLen = max(map(lambda x: len(x.parent), envs.values())) + 3
    maxRegistryNameLen = max(
        map(lambda x: len(x.registryShortName), envs.values())) + 3
    lineFmt = "{:<%ds}{:<%ds}{:<%ds}{}" % (
        maxEnvNameLen, maxParentNameLen, maxRegistryNameLen)
    print(lineFmt.format("NAME", "PARENT", "REGISTRY", "DESCRIPTION"))
    for _, env in sorted(envs.items()):
        print(lineFmt.format(
            env.envName,
            env.parent if env.parent else "",
            env.registryShortName,
            env.description))


def run(argv: ExecArgs = sys.argv[1:]) -> None:
    args = usage(argv)
    setupLogging(args.verbose)
    notifyUserProc = getUserNotificationProc(args.verbose)

    try:
        # Load config and prepare the environment, no IO are performed here
        conf = loadConfig(notifyUserProc, skipLocal=args.list or args.env)
        if args.list:
            return listEnv(conf.envs)
        env = loadEnv(conf, args.env, args.base)
        applyCommandLineOverride(args, env)
        containerName, containerArgs, envArgs, \
            hostPreArgs, hostPostArgs = prepareEnv(env, args.args)
    except RuntimeError as e:
        fail(notifyUserProc, str(e))

    try:
        # Prepare the image and create needed host directories
        imageName = setupPod(notifyUserProc, env, args.package)
    except RuntimeError as e:
        fail(notifyUserProc, str(e))

    try:
        # Run the environment
        if args.tag:
            # This is a bit of a hack, tag should be passed to the env and used
            # by the runtime.getExecName procedure
            imageName[-1] += ":" + args.tag
            if not podmanExists("image", imageName[-1]):
                fail(notifyUserProc, f"Unknown tag {imageName[-1]}")
        executeHostTasks(hostPreArgs)
        executePod(containerName, containerArgs, imageName, envArgs)
        podResult = 0
    except KeyboardInterrupt:
        try:
            killPod(containerName)
        except RuntimeError:
            pass
        podResult = 1
    except RuntimeError:
        podResult = 1

    try:
        # Perform post tasks
        executeHostTasks(hostPostArgs)
    except RuntimeError as e:
        print(e)

    try:
        # Cleanup left-over
        cleanupEnv(env)
    except RuntimeError as e:
        fail(notifyUserProc, str(e))

    log.debug("Complete")
    exit(podResult)


if __name__ == "__main__":
    run()
