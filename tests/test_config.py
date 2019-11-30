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
from textwrap import dedent
from unittest import skipIf, TestCase

import podenv.config


def fakeConfig(content):
    return type('ConfigFile', (object,), dict(
        read_text=lambda: content, name="<test>"))


def configPath(name: str) -> Path:
    return Path(__file__).parent / "config" / name


class TestConfig(TestCase):
    def test_requires(self):
        config = podenv.config.Config(fakeConfig(dedent("""
          environments:
            test:
              image: fedora:30
            require-list:
              requires:
                - test
            require-str:
              requires: test
        """)))
        self.assertEqual(config.envs["require-list"].requires,
                         config.envs["require-str"].requires)
        self.assertEqual(config.envs["require-str"].requires,
                         ["test"])

    def test_volumes(self):
        config = podenv.config.Config(fakeConfig(dedent("""
          environments:
            gertty:
              volumes:
                git:
              mounts:
                ~/git: ~/git
        """)))
        # TODO: implement path conflict and check for error here.
        self.assertEqual(
            config.envs["gertty"].volumeInfos["git"].name, "git")

    def test_mounts(self):
        config = podenv.config.Config(fakeConfig(dedent("""
          environments:
            test-env:
              mounts:
                ~/git:
        """)))
        # TODO: implement path conflict and check for error here.
        self.assertEqual(
            config.envs["test-env"].mountInfos["~/git"], Path("~/git"))


@skipIf(not Path("~/.local/bin/dhall-to-json").expanduser().exists(),
        "dhall-to-json is missing")
class TestDhallConfig(TestCase):
    @classmethod
    def setUpClass(cls):
        try:
            (Path(configPath(".")) / "podenv").unlink()
        except FileNotFoundError:
            pass

    @classmethod
    def tearDownClass(cls):
        (Path(configPath(".")) / "podenv").unlink()

    def test_schemas(self):
        config = podenv.config.Config(configPath("minimal.dhall"))
        self.assertEqual(
            config.envs["shell"].command, ["/bin/bash"])

    def test_simple_procedure(self):
        config = podenv.config.Config(configPath("common-image.dhall"))
        for env in ("firefox", "emacs"):
            self.assertEqual(
                config.envs[env].command, [env])
            self.assertEqual(config.envs[env].image, "shared-image-name")

    def test_capabilities(self):
        config = podenv.config.Config(configPath("capabilities.dhall"))
        self.assertEqual(config.envs["shell"].capabilities["terminal"], True)
