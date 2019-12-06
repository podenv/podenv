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


def yamlConfig(content: str):
    schema = podenv.config.safe_load(dedent(content))
    return podenv.config.transformSchema(schema)


def dhallConfig(name: str):
    def configPath(name: str) -> Path:
        return Path(__file__).parent / "config" / name

    path = configPath(name)
    schema = podenv.config.loadDhallConfig(path)
    return podenv.config.transformSchema(schema)


class TestConfig(TestCase):
    def test_volumes(self):
        envs = yamlConfig("""
        name: gertty
        image: fedora
        volumes:
          git:
        mounts:
          ~/git: ~/git
        """)
        self.assertIn("gertty", envs)
        # TODO: implement path conflict and check for error here.
        # self.assertEqual(
        #     config.envs["gertty"].volumeInfos["git"].name, "git")

    def test_mounts(self):
        envs = yamlConfig("""
        name: test-env
        image: fedora
        mounts:
          ~/git:
        """)
        self.assertIn("test-env", envs)
        # TODO: implement path conflict and check for error here.
        # self.assertEqual(
        #     config.envs["test-env"].mountInfos["~/git"], Path("~/git"))


@skipIf(not Path("/usr/local/bin/dhall-to-json").expanduser().exists() and
        not Path("~/.local/bin/dhall-to-json").expanduser().exists(),
        "dhall-to-json is missing")
class TestDhallConfig(TestCase):
    def test_schemas(self):
        envs = dhallConfig("minimal.dhall")
        self.assertEqual(
            envs["shell"].command, ["/bin/bash"])

    def test_simple_procedure(self):
        envs = dhallConfig("common-image.dhall")
        for env in ("firefox", "emacs"):
            self.assertEqual(
                envs[env].command, [env])
            self.assertEqual(envs[env].image, "shared-image-name")

    def test_capabilities(self):
        envs = dhallConfig("capabilities.dhall")
        self.assertEqual(envs["shell"].capabilities["terminal"], True)

    def test_hubconfigs(self):
        envs = dhallConfig("hubconfigs.dhall")
        self.assertEqual(
            envs["pavucontrol"].capabilities["pulseaudio"], True)
        self.assertEqual(envs["pavucontrol"].packages, ["pavucontrol"])
        self.assertEqual(envs["xeyes"].packages, ["xorg-x11-apps"])

    def test_container_file(self):
        envs = dhallConfig("container-file.dhall")
        self.assertEqual(envs["firefox"].image, "localhost/podenv/firefox")
