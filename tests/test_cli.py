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

import unittest

import podenv.config
import podenv.main
import podenv.pod
from podenv.defaults import environments


def getEnv(argv):
    args = podenv.main.usage(argv)
    conf = podenv.config.loadConfig(lambda msg: msg)
    env = podenv.config.loadEnv(conf, args.env)
    podenv.main.applyCommandLineOverride(args, env)
    podenv.env.prepareEnv(env, args.args)
    return env


class TestCommandLineInterface(unittest.TestCase):
    def test_env_variant(self):
        env = getEnv(["test.fedora"])
        self.assertEqual(env.image, environments["fedora"]["image"],
                         "fedora env is loaded")


if __name__ == '__main__':
    unittest.main()
