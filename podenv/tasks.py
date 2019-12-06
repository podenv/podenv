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
This module handles ansible like task definitions
"""

import base64
import copy
from typing import Union, Dict

from podenv.context import Task


def taskToCommand(task: Task) -> str:
    """Convert ansible like task to shell string"""
    task = copy.copy(task)
    command = []

    def defined(key: str) -> bool:
        return task.get(key) is not None

    whenCondition: Union[None, str, Dict[str, str]] = None
    if defined("delegate_to"):
        if task["delegate_to"] != "host":
            raise RuntimeError(f"Task delegate_to is incorrect: {task}")
        command.append("run_local_%s" % task.pop("delegate_to"))
    if defined("when") or defined("unless"):
        if task.get("unless"):
            whenCondition = "! %s" % task.pop("unless")
        else:
            whenCondition = task.pop("when")
        if not isinstance(whenCondition, str):
            raise RuntimeError(f"Invalid when condition {whenCondition}")
        command.append(f"if {whenCondition}; then true")

    if defined("name"):
        if "'" in task["name"]:
            raise RuntimeError(f"Task name can't have ': {task['name']}")
        command.append("echo '%s'" % task.pop("name"))
    if defined("block"):
        block = task.pop("block")
        if isinstance(block, list):
            for blockTask in map(taskToCommand, block):
                command.append(blockTask)
        else:
            raise RuntimeError(f"Invalid block task {block}")
    elif defined("command") or defined("shell"):
        if defined("command"):
            cmd = task.pop("command")
        else:
            cmd = task.pop("shell")
        command.append(str(cmd).rstrip('\n'))
    elif defined("copy"):
        copyTask = task.pop("copy")
        if not isinstance(copyTask, dict):
            raise RuntimeError(f"Invalid copy task {copyTask}")
        copyContent = copyTask.pop("content")
        copyDest = copyTask.pop("dest")
        if copyTask:
            raise RuntimeError(f"Unsupported copy attribute: {copyTask}")
        command.append("echo {b64content} | base64 -d > {dest}".format(
            b64content=base64.b64encode(
                copyContent.encode('utf-8')).decode('ascii'),
            dest=copyDest))
    else:
        raise RuntimeError(f"Unsupported task: {task}")
    if task:
        raise RuntimeError(f"Unsupported task attribute: {task}")
    if whenCondition:
        command.append("fi")
    return "; ".join(command)


def containerCommand(task: Task) -> str:
    if task.get('shell') or task.get('copy'):
        prefix = "RUN "
    else:
        prefix = ""
    command = []
    if task.get('name'):
        command.append(f"# {task.pop('name')}")
    command.append(prefix + taskToCommand(task))
    return "\n".join(command)
