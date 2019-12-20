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
from yaml import safe_dump
from typing import Dict, List
from textwrap import wrap

from podenv.capabilities import Capabilities
from podenv.config import loadConfig, getEnv
from podenv.pod import killPod, updateImage, setupImage, setupPod, \
    executeHostTasks, executePod, desktopNotification, prettyCmd
from podenv.context import ExecArgs, ExecContext, UserNotif
from podenv.env import Env, prepareEnv

log = logging.getLogger("podenv")


def formatUsage() -> str:
    stableModifiers = ["--verbose", "--debug", "--config FILE", "--expr EXPR"]
    stableCommand = ["--show", "--list", "--list-caps",
                     "--rebuild ENV", "--update ENV"]
    stableOptions = [
        "--shell", "--net NAME", "--home PATH", "--image IMAGE",
        "--environ KEY=VALUE"
    ]

    def opt(l: List[str]) -> List[str]:
        return list(map(lambda x: f"[{x}]", l))

    prefix = len("usage: podenv ")
    return ("\n" + " " * prefix).join(
        ["podenv " + " ".join(opt(stableModifiers))] +
        [" ".join(opt(stableCommand))] +
        wrap(" ".join(opt(stableOptions)), width=120 - prefix))


def formatHelp() -> str:
    return "\n".join([
        "usage: " + formatUsage(),
        "",
        "podenv - a podman wrapper",
        "",
        "commands:",
        "  --list          List the environments",
        "  --list-caps     List available capabilities",
        "  --show          Print the environments",
        "  --update ENV    Update environment image",
        "  --rebuild ENV   Rebuild environment image",
        "  ENV [ARGS]      Execute an environment",
        "",
        "environment execution arguments:",
        "  --home PATH     Set environment home directory",
        "  --net  NAME     Set environment network",
        "  --image IMAGE   Set environment image",
        "  --shell         Run a shell instead of the default command",
        "  --environ K=V   Add an environ(5) variable",
        "  --CAP           Activate a capability",
        "  --no-CAP        Disable a capability",
        "",
        "flags:",
        "  --verbose       Prints logs such as exec argv",
        "  --debug         Prints even more log",
        "  --config PATH   Set the config file path,",
        "                  (defaults to PODENV_CONFIG or "
        "~/.config/podenv/config.dhall)",
        "  --expr EXPR     Evaluate a config expression "
        "from the command line",
        "",
    ])


def formatCaps() -> str:
    return "\n".join([
        "capabilities:",
    ] + [f"  {name:15s} {doc.capitalize()}"
         for name, doc, _ in Capabilities if doc])


def usageParser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        prog="podenv",
        description="podenv - a podman wrapper",
        usage=formatUsage(),
        add_help=False,
    )
    parser.add_argument("--help", action='store_true')
    parser.add_argument("--list-caps", action='store_true')
    parser.add_argument("--verbose", action='store_true')
    parser.add_argument("--debug", action='store_true')
    parser.add_argument("-c", "--config",
                        default="~/.config/podenv/config.dhall")
    parser.add_argument("-E", "--expr")
    parser.add_argument("--show", action='store_true')
    parser.add_argument("--show-containerfile", action='store_true')
    parser.add_argument("--list", action='store_true')
    parser.add_argument("--shell", action='store_true')
    parser.add_argument("--dry", action='store_true',
                        help="Do not execute the environment")
    parser.add_argument("--net")
    parser.add_argument("--home")
    parser.add_argument("-e", "--environ", action='append')
    parser.add_argument("-i", "--image")
    parser.add_argument("--rebuild", default=False, action='store_true')
    parser.add_argument("--in-place", default=False, action='store_true')
    parser.add_argument("--update", default=False, action='store_true')
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


def applyEnvironOverride(args: argparse.Namespace) -> None:
    if environ.get("PODENV_CONFIG"):
        if args.config != "~/.config/podenv/config.dhall" \
           and args.config != environ.get("PODENV_CONFIG"):
            print(
                f"{args.config} is overriden by environ "
                f"PODENV_CONFIG={environ['PODENV_CONFIG']}")
        args.config = environ["PODENV_CONFIG"]


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
            if not env.environ:
                env.environ = {}
            env.environ[key] = val
    if args.shell:
        env.capabilities["terminal"] = True
        env.command = ["/bin/bash"]
    if args.image:
        env.image = args.image
    if args.net:
        env.network = args.net
    if args.home:
        env.home = str(Path(args.home).expanduser().resolve(strict=True))


def setupLogging(debug: bool) -> None:
    loglevel = logging.DEBUG if debug else logging.INFO
    logging.basicConfig(
        format="[+] \033[92m%(message)s\033[m",
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
        f"[+] \033[92m{msg}\033[m", file=sys.stderr)


def listEnv(envs: Dict[str, Env]) -> None:
    maxEnvNameLen = max(map(len, envs.keys())) + 3
    lineFmt = "{:<%ds}{}" % maxEnvNameLen
    print(lineFmt.format("NAME", "DESCRIPTION"))
    for _, env in sorted(envs.items()):
        print(lineFmt.format(env.envName, env.description))


def showEnv(verbose: bool, debug: bool, env: Env, ctx: ExecContext) -> None:
    containerFile = ctx.containerFile
    localImage = env.image.startswith('localhost/')
    if debug:
        log.debug("Schema:")
        print(safe_dump(env.original))
        env.original = None
    if verbose:
        log.debug("Environment:")
        env.containerFile = None
        print(f"{env.__repr__()}\n")
    if localImage and containerFile:
        log.info("Containerfile:")
        print(f"{containerFile.strip()}\n")
    if verbose and localImage and ctx.containerUpdate:
        log.info("Containerfile for update:")
        print(f"{ctx.containerUpdate.strip()}\n")
    log.info("Capabilities:")
    print("%s\n" % " ".join(sorted(list(map(lambda x: x[0], filter(
        lambda x: x[1], env.capabilities.items()))))))
    log.info("Command line:")
    print("podman run " + prettyCmd(
        ctx.getArgs() + [ctx.imageName] + ctx.commandArgs))


def run(argv: ExecArgs = sys.argv[1:]) -> None:
    args = usage(argv)
    if args.help:
        return print(formatHelp())
    if args.list_caps:
        return print(formatCaps())
    if args.debug:
        args.verbose = True
    setupLogging(args.verbose)
    notifyUserProc = getUserNotificationProc(args.verbose)
    cacheDir = Path("~/.cache/podenv").expanduser()
    applyEnvironOverride(args)

    try:
        # Load config and prepare the environment, no IO are performed here
        conf = loadConfig(skipLocal=args.list or args.env,
                          configStr=args.expr,
                          configFile=Path(args.config),
                          debug=args.debug)
        if args.list and not args.show:
            return listEnv(conf)

        if not args.env:
            if len(conf) != 1:
                print(usageParser().format_help())
                exit(1)
            args.env = list(conf.keys())[0]

        env = getEnv(conf, args.env)
        applyCommandLineOverride(args, env)
        ctx = prepareEnv(env, args.args)
    except RuntimeError as e:
        if args.debug:
            raise
        fail(notifyUserProc, str(e))

    if args.show_containerfile:
        return print(ctx.containerFile)

    if args.show:
        return showEnv(args.verbose, args.debug, env, ctx)

    try:
        # Update the image
        if args.update:
            if args.rebuild:
                raise RuntimeError("Invalid action --update --rebuild")
            return updateImage(notifyUserProc, ctx, cacheDir)
    except RuntimeError as e:
        if args.debug:
            raise
        fail(notifyUserProc, str(e))

    try:
        setupImage(notifyUserProc, ctx, args.rebuild, args.in_place, cacheDir)
        if args.dry or args.rebuild:
            return log.info("Done.")
        # Prepare the image and create required host directories
        setupPod(notifyUserProc, ctx, args.rebuild)
    except RuntimeError as e:
        if args.debug:
            raise
        fail(notifyUserProc, str(e))

    try:
        executeHostTasks(ctx.hostPreTasks)
        executePod(ctx.name, ctx.getArgs(), ctx.imageName, ctx.commandArgs)
        podResult = 0
    except KeyboardInterrupt:
        try:
            killPod(ctx.name)
        except RuntimeError:
            pass
        podResult = 1
    except RuntimeError:
        podResult = 1

    try:
        # Perform post tasks
        executeHostTasks(ctx.hostPostTasks)
    except RuntimeError as e:
        if args.debug:
            raise
        print(e)

    try:
        # Cleanup left-over
        ...
    except RuntimeError as e:
        fail(notifyUserProc, str(e))

    log.debug("Complete")
    exit(podResult)


if __name__ == "__main__":
    run()
