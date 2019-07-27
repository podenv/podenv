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
from podenv.pod import setupPod, executePod
from podenv.env import prepareEnv, cleanupEnv

log = logging.getLogger("podenv")


def usage() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="podenv - a podman wrapper")
    parser.add_argument("--verbose", action='store_true')
    parser.add_argument("env", nargs='?')
    parser.add_argument("args", nargs='*')
    return parser.parse_args()


def setupLogging(debug: bool):
    loglevel = logging.DEBUG if debug else logging.INFO
    logging.basicConfig(
        format="%(asctime)s %(levelname)-5.5s %(name)s - "
               "\033[92m%(message)s\033[m",
        level=loglevel)


def fail(msg: str, code=1):
    print(f"\33[91m{msg}\033[m", file=sys.stderr)
    exit(code)


def run():
    args = usage()
    setupLogging(args.verbose)

    try:
        conf = loadConfig()
        env = loadEnv(conf, args.env)
        setupPod(env)
        podmanArgs = prepareEnv(env)
    except RuntimeError as e:
        fail(str(e))

    try:
        executePod(podmanArgs)
        podResult = 0
    except RuntimeError:
        podResult = 1

    try:
        cleanupEnv(env)
    except RuntimeError as e:
        fail(str(e))

    log.debug("Complete")
    exit(podResult)


if __name__ == "__main__":
    run()
