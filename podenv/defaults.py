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

from typing import List, Tuple


Cap = List[Tuple[str, bool]]


# TODO: remove selinux=False when podenv support custom policies
# TODO: remove uidmap=True when podenv support better host uid remap
x11 = [("x11", True), ("selinux", False), ("uidmap", True)]


def on(cap: str) -> Cap:
    return [(cap, True)]


def off(cap: str) -> Cap:
    return [(cap, False)]


environments = dict(
    fedora=dict(
        image="registry.fedoraproject.org/fedora:30",
    ),

    # Basic desktop applications
    pavucontrol=dict(
        description="Adjust audio volumes",
        parent="fedora",
        capabilities=dict(x11 + on("pulseaudio")),
        desktop=dict(icon="multimedia-volume-control"),
        packages=["pavucontrol"],
        command=["pavucontrol"]),

    xeyes=dict(
        description="Test graphical setup",
        parent="fedora",
        capabilities=dict(x11),
        packages=["xorg-x11-apps"],
        command=["xeyes"]),

    # File viewer/editor
    pdf=dict(
        description="Display a pdf file",
        parent="fedora",
        capabilities=dict(x11),
        packages=["mupdf"],
        command=["mupdf", "$1"]),

    feh=dict(
        description="Display an image file",
        parent="fedora",
        capabilities=dict(x11),
        packages=["feh"],
        command=["feh", "$1"]),

    inkscape=dict(
        description="Edit a svg file",
        parent="fedora",
        capabilities=dict(x11),
        packages=["inkscape"],
        command=["inkscape", "$1"]),

    # Communication
    mumble=dict(
        description="VoIP solution",
        parent="fedora",
        # TODO: figure out why host ipc is needed
        capabilities=dict(x11 + on("pulseaudio") + on("ipc") + on("network")),
        # TODO: figure a better system to persist env state
        home="~/.config/podhome/mumble",
        packages=["mumble"],
        command=["mumble"]),

    # Browser
    firefoxLight=dict(
        parent="fedora",
        shmsize="4g",
        capabilities=dict(x11 + on("pulseaudio") + on("network")),
        packages=["firefox"],
        home="~/.config/podhome/firefoxLight",
        command=["firefox"]),

    firefox=dict(
        parent="firefoxLight",
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
)
