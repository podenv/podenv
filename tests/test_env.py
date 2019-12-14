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

from pathlib import Path
from unittest import TestCase

from podenv.context import User
import podenv.env


def fakeEnv(envName, envSchema):
    envSchema['name'] = envName
    envSchema['image'] = envName
    return podenv.env.loadEnv(envSchema)


class TestConfig(TestCase):
    def test_mounts(self):
        env = fakeEnv("test-env",
                      dict(capabilities=dict(uidmap=True),
                           mounts={"~/git": None}))
        with self.assertRaises(RuntimeError):
            podenv.env.prepareEnv(env, [])
        env.user = User("user", Path("/home/user"), 1000)
        ctx = podenv.env.prepareEnv(env, [])
        execCommand = " ".join(ctx.getArgs())
        self.assertIn("-v /home/user/git:/home/user/git", execCommand)

    def test_volumes(self):
        env = fakeEnv("gertty", dict(
            capabilities=dict(uidmap=True),
            volumes=[{"ContainerPath": "~/git", "name": "git"}]))
        with self.assertRaises(RuntimeError):
            podenv.env.prepareEnv(env, [])
        env.user = User("user", Path("/home/user"), 1000)
        ctx = podenv.env.prepareEnv(env, [])
        execCommand = " ".join(ctx.getArgs())
        self.assertIn("-v git:/home/user/git", execCommand)
