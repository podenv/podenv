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
This module provides default environments.

Schema definition is subject to changes. Once the schema is stable,
this should be moved to a separate repo, e.g. podhub.
"""

from os.path import basename
from textwrap import dedent
from typing import List, Tuple


GUIX_URL = "https://ftp.gnu.org/gnu/guix/guix-binary-1.0.1.x86_64-linux.tar.xz"
GUIX_KEY = "3CE464558A84FDC69DB40CFB090B11993D9AEBB5"
Cap = List[Tuple[str, bool]]


# TODO: remove selinux=False when podenv support custom policies
# TODO: remove uidmap=True when podenv support better host uid remap
x11 = [("x11", True), ("selinux", False), ("uidmap", True)]


def on(cap: str) -> Cap:
    return [(cap, True)]


def off(cap: str) -> Cap:
    return [(cap, False)]


environments = {
    "fedora": dict(
        url="https://fedoraproject.org/",
        image="registry.fedoraproject.org/fedora:30",
        capabilities=dict(
            on("manage-image") + on("auto-update")),
    ),
    "fedora-admin": dict(
        parent="fedora",
        description="Fedora admin shell",
        capabilities=dict(
            on("root") + on("network") + on("mount-cache") + on("terminal")),
    ),
    "debian": dict(
        url="https://www.debian.org",
        image="docker.io/debian:stretch",
        capabilities=dict(
            on("manage-image") + on("auto-update")),
        imageTasks=[
            dict(name="Remove docker customization",
                 shell="rm -f /etc/apt/apt.conf.d/docker-*"),
        ]
    ),
    "debian-admin": dict(
        parent="debian",
        description="Debian admin shell",
        capabilities=dict(
            on("root") + on("network") + on("mount-cache") + on("terminal")),
    ),
    "gentoo": dict(
        url="https://www.gentoo.org",
        image="docker.io/gentoo/stage3-x86:latest",
        capabilities=dict(
            on("manage-image") + on("auto-update")),
        imageTasks=[
            dict(name="Fix emerge in rootless",
                 shell='echo FEATURES="-sandbox" >> /etc/portage/make.conf;'
                 'echo USE="-filecaps" >> /etc/portage/make.conf')
        ]
    ),
    "gentoo-admin": dict(
        parent="gentoo",
        description="Gentoo admin shell",
        capabilities=dict(
            on("root") + on("network") + on("mount-cache") + on("terminal")),
    ),
    "guix": {
        "url": "https://guix.gnu.org/",
        # Guix package manager is installed on fedora
        # TODO: create/find a guix image from scratch
        "image": "registry.fedoraproject.org/fedora:30",
        "capabilities": dict(
            on("manage-image") + on("auto-update")),
        "description": "Guix packages",
        "system-type": "guix",
        "mounts": {
            "/gnu": "~/.cache/podenv/guix/gnu",
            "/var/guix": "~/.cache/podenv/guix/var/guix",
        },
        "pre-tasks": [{
            "name": "Loading guix-profile",
            "shell": "source "
            "/var/guix/profiles/default/guix-profile/etc/profile"
        }],
        "image-tasks": [{
            "name": "Installing guix binary",
            "delegate_to": "host",
            "shell": dedent("""
              set -e;
              if [ ! -d ~/.cache/podenv/guix/var/guix/profiles ]; then
              mkdir -p ~/.cache/podenv/guix;
              chcon -R system_u:object_r:container_file_t:s0 \
              ~/.cache/podenv/guix;
              pushd ~/.cache/podenv/guix;
                curl -O {guix_url};
                curl -O {guix_url}.sig;
                gpg --keyserver pool.sks-keyservers.net --recv-keys {guix_key};
                gpg --verify {guix_file}.sig;
                tar xf {guix_file};
              fi
            """.format(guix_url=GUIX_URL,
                       guix_key=GUIX_KEY,
                       guix_file=basename(GUIX_URL)))
        }, {
            "name": "Creating guix users",
            "shell": dedent("""
              groupadd guix-builder;
              for i in $(seq 1 10);
              do adduser -G guix-builder guix-builder$i; done;
              for i in guix guix-daemon;
              do ln -sf /var/guix/profiles/per-user/root/current-guix/bin/$i \
                     /usr/bin/$i; done;
              mkdir -p /root/.config/guix;
              ln -sf /var/guix/profiles/per-user/root/current-guix \
                     /root/.config/guix/current
            """)
        }, {
            "name": "Authorizing guix.gnu.org substitutes",
            "shell": "guix archive --authorize < "
            "/root/.config/guix/current/share/guix/ci.guix.gnu.org.pub"
        }, {
            "name": "Doing initial pull",
            "shell": "rm -f /var/guix/profiles/default/current-guix;"
            "guix-daemon --build-users-group=guix-builder "
            "--disable-chroot & guix pull",
        }]
    },
    "guix-admin": {
        "parent": "guix",
        "description": "Guix packages admin shell",
        "capabilities": dict(
            on("root") + on("network") + on("terminal")),
        "pre-tasks": [{
            "name": "Loading guix root profile",
            "shell": "source /var/guix/profiles/per-user/root/"
            "current-guix/etc/profile"
        }, {
            "name": "Starting guix-daemon",
            "shell": "guix-daemon --build-users-group=guix-builder "
            "--disable-chroot & true"
        }],
        "command": ["/bin/bash"],
    },
}
