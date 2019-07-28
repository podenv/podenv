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

from __future__ import annotations
import json
from abc import ABC, abstractmethod
from dataclasses import dataclass, field, fields
from pathlib import Path
from typing import Dict, List, Optional, Union


ExecArgs = List[str]
Requirements = List[str]
Info = Dict[str, Union[str, Requirements]]


class Runtime(ABC):
    System = {
        "rpm": {
            "commands": {
                "install": "dnf install -y",
                "update": "dnf update -y",
            }
        }
    }

    def __init__(self, metadataPath: Path):
        self.metadataPath = metadataPath
        # TODO: discover system type
        self.commands = self.System["rpm"]["commands"]
        self.info: Info = {}

    def updateInfo(self, info: Info) -> None:
        self.info.update(info)
        self.metadataPath.write_text(json.dumps(self.info))

    def loadInfo(self) -> None:
        self.info = json.loads(self.metadataPath.read_text())

    def exists(self) -> bool:
        return self.metadataPath.exists()

    @abstractmethod
    def create(self) -> None:
        ...

    @abstractmethod
    def update(self) -> None:
        ...

    def install(self, packages: List[str]) -> None:
        ...


@dataclass
class Env:
    name: str
    image: str = ""
    rootfs: str = ""
    command: ExecArgs = field(default_factory=list)
    parent: str = ""
    environment: Dict[str, str] = field(default_factory=dict)

    # Internal attribute
    runtime: Optional[Runtime] = None

    def applyParent(self, parentEnv: Env) -> None:
        for attr in fields(Env):
            if attr.name in ('name', 'parent'):
                continue
            if getattr(self, attr.name) in (None, ""):
                setattr(self, attr.name, getattr(parentEnv, attr.name))
            elif (attr.type == List[str]):
                # List are extended
                setattr(self, attr.name, getattr(self, attr.name) +
                        getattr(parentEnv, attr.name))
            elif (attr.type == Dict[str, str]):
                # Dictionary are updated in reverse
                mergedDict = getattr(parentEnv, attr.name)
                mergedDict.update(getattr(self, attr.name))
                setattr(self, attr.name, mergedDict)


def prepareEnv(env: Env) -> ExecArgs:
    return []


def cleanupEnv(env: Env) -> None:
    ...
