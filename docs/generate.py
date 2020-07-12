#!/bin/env python3
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
A script to generate docs and types
"""
import re
from dataclasses import fields
from pathlib import Path
from podenv.env import Env
from podenv.capabilities import Capabilities
from podenv.main import formatHelp

dirty = False


def update(path: Path, newContent: str) -> None:
    if newContent[-1] != "\n":
        newContent += "\n"
    if path.read_text() != newContent:
        global dirty
        path.write_text(newContent)
        print(f"{path}: updated!")
        dirty = True


def writeDhallCapabilities() -> None:
    capNames = [c[0] for c in Capabilities]
    defTrue = ("selinux", "seccomp")
    for f, c in (
            ("types/Capabilities.dhall",
             "{ " + "\n, ".join(map(
                 lambda c: c + " : Optional Bool", capNames)) + "\n}"),
            ("defaults/Capabilities.dhall",
             "{ " + "\n, ".join(map(
                 lambda c: c + " = %s" % (
                     "Some True" if c in defTrue else "None Bool"),
                 capNames)) + "\n}")):
        update(Path("podenv") / "dhall" / f, c)


def writeReferences() -> None:
    path = Path("docs/references/configuration.md")
    doc = path.read_text().split('\n')

    envblockstart = doc.index(
        'Name                 | Type            | '
        'Doc                                      |') + 2
    envblockend = doc[envblockstart:].index('') + envblockstart
    capblockstart = doc.index(
        'Name                 | Doc               '
        '                                         |') + 2
    capblockend = doc[capblockstart:].index('') + capblockstart

    def typeToStr(type):
        typeStr = str(type)
        typeStr = typeStr.replace('podenv.context.', '')
        typeStr = typeStr.replace('pathlib.Path', 'Path')
        if typeStr == "<class 'str'>":
            typeStr = 'str'
        elif typeStr.startswith('typing.'):
            typeStr = typeStr.replace('typing.', '')
            typeStr = typeStr.replace(
                'Dict[str, Union[str, Dict[str, str]]]', 'Task')
            typeStr = typeStr.replace('Dict[Path, Volume]', 'Volumes')
            typeStr = typeStr.replace('Dict[Path, Path]', 'Mounts')
            typeStr = re.sub(
                r'Union\[(.*), NoneType\]', r'Optional[\1]', typeStr)
            if ', List[' in typeStr:
                typeStr = typeStr.replace('Optional', 'Optional[Union') + ']'
        else:
            raise RuntimeError("Unknown type: %s" % typeStr)
        return typeStr

    envblock = ['{name:20s} | {type:15s} | {doc:40s} |'.format(
        name=(lambda s: re.sub('([A-Z]+)', r'-\1', s).lower())(f.name),
        type=typeToStr(f.type), doc=f.metadata.get('doc', ''))  # type: ignore
                for f in fields(Env)
                if not f.metadata.get('internal', False)]  # type: ignore
    capblock = ['{name:20s} | {doc:60s} |'.format(
        name=c[0], doc=c[1]) for c in Capabilities]
    newdoc = doc[:envblockstart] + envblock + doc[
        envblockend:capblockstart] + capblock + doc[capblockend:]
    update(path, "\n".join(newdoc))


def writeCommandLine() -> None:
    path = Path("docs/references/command-line.md")
    doc = path.read_text().split('\n')
    clistart = doc.index('```bash')
    newdoc = doc[:clistart + 1] + (formatHelp().split('\n')[:-1]) + ["```"]
    update(path, "\n".join(newdoc))


if __name__ == "__main__":
    writeDhallCapabilities()
    writeReferences()
    writeCommandLine()
    exit(dirty)
