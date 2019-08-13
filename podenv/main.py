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

import argparse
import logging
import sys

from podenv.config import loadConfig, loadEnv
from podenv.pod import killPod, setupPod, executePod
from podenv.env import Capabilities, Env, prepareEnv, cleanupEnv

log = logging.getLogger("podenv")


def usageParser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description="podenv - a podman wrapper")
    parser.add_argument("--verbose", action='store_true')
    parser.add_argument("--shell", action='store_true',
                        help="Run bash instead of the profile command")
    parser.add_argument("-p", "--package", action='append',
                        help="Add a package to the environment")
    for name, doc, _ in Capabilities:
        parser.add_argument(f"--{name}", action='store_true',
                            help=f"Enable capability: {doc}")
        parser.add_argument(f"--no-{name}", action='store_true',
                            help=f"Disable {name} capibility")
    parser.add_argument("env", nargs='?')
    parser.add_argument("args", nargs='*')
    return parser


def usage() -> argparse.Namespace:
    return usageParser().parse_args()


def applyCommandLineOverride(args: argparse.Namespace, env: Env) -> None:
    for name, _, _ in Capabilities:
        if getattr(args, f"{name}"):
            env.capabilities[name] = True
        if getattr(args, f"no_{name}"):
            env.capabilities[name] = False
    if args.shell:
        env.capabilities["terminal"] = True
        env.command = ["/bin/bash"]


def setupLogging(debug: bool) -> None:
    loglevel = logging.DEBUG if debug else logging.INFO
    logging.basicConfig(
        format="%(asctime)s %(levelname)-5.5s %(name)s - "
               "\033[92m%(message)s\033[m",
        level=loglevel)


def fail(msg: str, code: int = 1) -> None:
    print(f"\33[91m{msg}\033[m", file=sys.stderr)
    exit(code)


def run() -> None:
    args = usage()
    setupLogging(args.verbose)

    try:
        # Load config and prepare the environment, no IO are performed here
        conf = loadConfig()
        env = loadEnv(conf, args.env)
        applyCommandLineOverride(args, env)
        containerName, containerArgs, envArgs = prepareEnv(env, args.args)
    except RuntimeError as e:
        fail(str(e))

    try:
        # Prepare the image and create needed host directories
        imageName = setupPod(env, args.package)
    except RuntimeError as e:
        fail(str(e))

    try:
        # Run the environment
        executePod(containerName, containerArgs, imageName, envArgs)
        podResult = 0
    except KeyboardInterrupt:
        killPod(containerName)
        podResult = 1
    except RuntimeError:
        podResult = 1

    try:
        # Cleanup left-over
        cleanupEnv(env)
    except RuntimeError as e:
        fail(str(e))

    log.debug("Complete")
    exit(podResult)


if __name__ == "__main__":
    run()
