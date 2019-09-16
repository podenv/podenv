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

from unittest import TestCase
from pathlib import Path

from podenv.config import attributesToCamelCase
import podenv.env


def fakeEnv(envName, envSchema):
    return podenv.env.Env(
        envName,
        configFile=Path("test"),
        registryName="test",
        **attributesToCamelCase(envSchema))


class TestConfig(TestCase):
    def test_package_filters(self):
        packages = set(("python3-numpy", "pip:triangle"))
        self.assertEqual(podenv.env.packageFilter(packages),
                         set(("python3-numpy",)))
        self.assertEqual(podenv.env.pipFilter(packages),
                         set(("pip:triangle",)))

    def test_mounts(self):
        env = fakeEnv("test-env",
                      dict(capabilities=dict(uidmap=True),
                           mounts={"~/git": None}))
        preps = podenv.env.prepareEnv(env, [], [])
        execCommand = " ".join(preps[1])
        self.assertIn("-v /home/user/git:/home/user/git", execCommand)

    def test_volumes(self):
        env = fakeEnv("gertty",
                      dict(capabilities=dict(uidmap=True),
                           volumes=dict(git=None)))
        preps = podenv.env.prepareEnv(env, [], [])
        execCommand = " ".join(preps[1])
        self.assertIn("-v git:/home/user/git", execCommand)
