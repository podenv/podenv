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
from dataclasses import dataclass, field, fields
from typing import Dict, List


ExecArgs = List[str]


@dataclass
class Env:
    name: str
    image: str = ""
    command: ExecArgs = field(default_factory=list)
    parent: str = ""
    environment: Dict[str, str] = field(default_factory=dict)

    def applyParent(self, parentEnv: Env) -> None:
        for attr in fields(Env):
            if attr.name in ('name', 'parent'):
                continue
            if not getattr(self, attr.name):
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
