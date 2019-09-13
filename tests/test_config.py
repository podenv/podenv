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

from textwrap import dedent
from unittest import TestCase

import podenv.config


def fakeConfig(content):
    return type('ConfigFile', (object,), dict(
        read_text=lambda: content))


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
