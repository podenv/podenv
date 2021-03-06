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


def getEnv(argv):
    args = podenv.main.usage(argv)
    conf = podenv.config.transformSchema([dict(name="fedora", image="fedora")])
    env = podenv.config.getEnv(conf, args.env)
    podenv.main.applyCommandLineOverride(args, env)
    podenv.env.prepareEnv(env, args.args)
    return env


class TestCommandLineInterface(unittest.TestCase):
    def test_override_environ(self):
        env = getEnv(["-e", "PORT=4242", "fedora"])
        self.assertEqual(env.environ["PORT"], "4242")


if __name__ == '__main__':
    unittest.main()
