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

all: test doc

doc:
	@(python3 -c "'Poor man autodoc generator'; \
'# Get README.md indexes'; \
doc = open('README.md').read().split('\n'); \
envblockstart = doc.index( \
'Name                 | Type            | ' \
'Doc                                      |') + 2; \
envblockend = doc[envblockstart:].index('') + envblockstart; \
capblockstart = doc.index( \
'Name                 | Doc               ' \
'                                         |') + 2; \
capblockend = doc[capblockstart:].index('') + capblockstart; \
usageblockstart = doc.index('$$ podenv --help') + 1; \
usageblockend = doc[usageblockstart:].index('\`\`\`') + usageblockstart; \
'# Generate sections'; \
from dataclasses import fields; \
from podenv.env import Env, Capabilities; \
envblock = ['{name:20s} | {type:15s} | {doc:40s} |'.format( \
                name=f.name, type=f.type, doc=f.metadata.get('doc', '')) \
            for f in fields(Env) \
            if not f.metadata.get('internal', False)]; \
capblock = ['{name:20s} | {doc:60s} |'.format( \
                name=c[0], doc=c[1]) for c in Capabilities]; \
from podenv.main import usageParser; \
import sys; sys.argv = ['podenv']; \
usage = usageParser().format_help().split('\n')[:-1]; \
'# Update README.md if needed'; \
newdoc = doc[:envblockstart] + envblock + doc[envblockend:capblockstart] + \
         capblock + doc[capblockend:usageblockstart] + \
         usage + doc[usageblockend:]; \
exit(0) if newdoc == doc else open('README.md', 'w').write('\n'.join( \
  newdoc)); print('README.md updated!');")

test: test-type test-lint

test-lint:
	flake8

test-type:
	mypy --strict podenv
