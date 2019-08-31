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
    # System
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
        "parent": "fedora",
        "description": "Guix packages",
        "system-type": "guix",
        "mounts": {
            "/gnu": "~/.cache/podenv/guix/gnu",
            "/var/guix": "~/.cache/podenv/guix/var/guix",
        },
        "image-tasks": [{
            "name": "Setup guix binary install",
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
            "name": "Setup guix users",
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
            "name": "Authorize guix.gnu.org substitutes",
            "shell": "guix archive --authorize < "
            "/root/.config/guix/current/share/guix/ci.guix.gnu.org.pub"
        }, {
            "name": "Do initial pull",
            "shell": "rm -f /var/guix/profiles/default/current-guix;"
            "guix-daemon --build-users-group=guix-builder "
            "--disable-chroot & guix pull",
        }]
    },
    "guix-admin": dict(
        parent="guix",
        description="Guix packages admin shell",
        capabilities=dict(
            on("root") + on("network") + on("terminal")),
        command=["/bin/bash", "-c",
                 "guix-daemon --build-users-group=guix-builder "
                 "--disable-chroot & "
                 "source /var/guix/profiles/default/guix-profile/etc/profile;"
                 "source /var/guix/profiles/per-user/root/"
                 "current-guix/etc/profile; exec /bin/bash"]
    ),

    # Basic desktop applications
    "pavucontrol": dict(
        description="Adjust audio volumes",
        parent="fedora",
        capabilities=dict(x11 + on("pulseaudio")),
        desktop=dict(icon="multimedia-volume-control"),
        packages=["pavucontrol"],
        command=["pavucontrol"]),

    "xeyes": dict(
        description="Test graphical setup",
        parent="fedora",
        capabilities=dict(x11),
        packages=["xorg-x11-apps"],
        command=["xeyes"]),

    # File viewer/editor
    "mupdf": dict(
        description="Display a pdf file",
        parent="fedora",
        capabilities=dict(x11),
        packages=["mupdf"],
        command=["mupdf", "$1"]),

    "feh": dict(
        description="Display an image file",
        parent="fedora",
        capabilities=dict(x11),
        packages=["feh"],
        command=["feh", "$1"]),

    "inkscape": dict(
        description="Edit a svg file",
        parent="fedora",
        capabilities=dict(x11),
        packages=["inkscape"],
        command=["inkscape", "$1"]),

    # Network service
    "python-http-server": dict(
        description="Expose current directory over HTTP",
        parent="fedora",
        capabilities=dict(on("network") + on("mount-cwd")),
        packages=["python3"],
        environ=dict(
            PORT="8080",
            PYTHONUNBUFFERED="1"
        ),
        ports=["{PORT}:8000"],
        command=["python3", "-m", "http.server"],
    ),

    # Communication
    "mumble": dict(
        description="VoIP solution",
        parent="fedora",
        # TODO: figure out why host ipc is needed
        capabilities=dict(x11 + on("pulseaudio") + on("ipc") + on("network")),
        # TODO: figure a better system to persist env state
        home="~/.config/podhome/mumble",
        packages=["mumble"],
        command=["mumble"]),

    # Devel
    "git": dict(
        description="Git command line",
        parent="fedora",
        capabilities=dict(on("git") + on("editor") + on("terminal")
                          + on("mount-cwd") + on("uidmap") + off("selinux")),
        packages=["git"],
        command=["git"]),

    "git-pull-request": dict(
        description="Submit github/pagure PR",
        parent="git",
        capabilities=dict(on("ssh") + on("network")),
        # TODO: drop this mounts when fedora update g-p-r version
        mounts={"~/.netrc": "~/.netrc"},
        packages=["git-pull-request"],
        # TODO: drop the no comment argument when fedora update g-p-r version
        command=["git-pull-request", "--no-comment-on-update"]),

    "git-review": dict(
        description="Submit gerrit CR",
        parent="git",
        capabilities=dict(on("ssh") + on("network")),
        packages=["git-review"],
        command=["git-review"]),

    # IDE
    "emacs-nox": dict(
        description="Extensible text editor (terminal mode)",
        parent="fedora",
        capabilities=dict(on("terminal")),
        home="~/.config/podhome/emacs",
        packages=["emacs"],
        command=["emacs"]),

    "emacs": dict(
        description="Extensible text editor",
        parent="emacs-nox",
        capabilities=dict(x11)),

    "codium": dict(
        parent="fedora",
        imageCustomizations=[
            """cat << EOF > /etc/yum.repos.d/vscodium.repo
[gitlab.com_paulcarroty_vscodium_repo]
name=gitlab.com_paulcarroty_vscodium_repo
baseurl=https://gitlab.com/paulcarroty/vscodium-deb-rpm-repo/raw/repos/rpms/
enabled=1
gpgcheck=1
repo_gpgcheck=1
gpgkey=https://gitlab.com/paulcarroty/vscodium-deb-rpm-repo/raw/master/pub.gpg
EOF"""],
        capabilities=dict(x11),
        packages=["which", "codium"],
        home="~/.config/podhome/codium",
        command=[
            # TODO: add support for application which pre-fork
            "bash", "-c", "codium; echo 'press ctrl-c to quit'; sleep Inf"]),

    # Browser
    "firefox-light": dict(
        parent="fedora",
        shmsize="4g",
        capabilities=dict(x11 + on("pulseaudio") + on("network")),
        packages=["firefox"],
        home="~/.config/podhome/firefoxLight",
        command=["firefox"]),

    "firefox": dict(
        parent="firefox-light",
        # Add rpmfusion for ffmpeg
        imageCustomizations=[
            "dnf install -y "
            "https://download1.rpmfusion.org/free/fedora/"
            "rpmfusion-free-release-"
            "$(grep ^VERSION_ID= /etc/os-release | cut -d= -f2).noarch.rpm"],
        # Enable direct rendering
        capabilities=dict(on("dri")),
        packages=["ffmpeg",
                  "libvdpau-va-gl", "mesa-dri-drivers",
                  "libva-intel-driver", "libva-intel-hybrid-driver"],
        home="~/.config/podhome/firefox"),
}
