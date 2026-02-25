let Prelude = ../Prelude.dhall

let Podenv = ../Podenv.dhall

let fedora = ../Builders/fedora.dhall

let mkUse =
      \(version : Text) ->
      \(pre-task : Text) ->
      \(post-task : Text) ->
      \(packages : List Text) ->
        Podenv.Container (fedora.image version pre-task post-task packages)

let fusion-repo =
      \(variant : Text) ->
      \(ver : Text) ->
        "https://download1.rpmfusion.org/${variant}/fedora/rpmfusion-${variant}-release-${ver}.noarch.rpm"

let -- | Extra layer to install common graphic packages
    extraGraphic =
      let extra-pkgs =
              [ "mesa-dri-drivers"
              , "mesa-vulkan-drivers"
              , "libglvnd-glx"
              , "glxinfo"
              , "gtk4"
              , "qt5-qtwayland"
              , "xdg-utils"
              , "glx-utils"
              , "libglvnd-gles"
              , "harfbuzz-icu"
              ]
            # [ "dejavu-sans-mono-fonts", "dejavu-sans-mono-fonts" ]
            # [ "xdg-dbus-proxy", "xdg-desktop-portal-gtk" ]
            # [ "which"
              , "findutils"
              , "strace"
              , "procps-ng"
              , "file"
              , "xz"
              , "diffutils"
              ]
            # [ "pipewire-libs", "pulseaudio-libs", "pipewire-pulseaudio" ]

      let repo = fusion-repo "free" "\$(rpm -E %fedora)"

      let erepo = fusion-repo "nonfree" "\$(rpm -E %fedora)"

      let -- | A list of packages that are often required but missing from the requirements
          gfx-pkgs =
            Prelude.Text.concatSep
              " "
              [ "ffmpeg"
              , "libva"
              , "libva-utils"
              , "libva-intel-driver"
              , "libvdpau-va-gl"
              ]

      in  ''
          RUN dnf install -y ${Prelude.Text.concatSep
                                 " "
                                 extra-pkgs} && dnf install -y ${repo}
          RUN dnf install -y ${erepo}
          RUN dnf install -y --best --allowerasing ${gfx-pkgs}
          ''

let base =
      { default = Podenv.Application::{
        , runtime = Podenv.Image (fedora.image-ref ":latest")
        , volumes = fedora.mkVolumes "latest"
        , capabilities = Podenv.Capabilities::{
          , terminal = True
          , interactive = True
          , network = True
          , rw = True
          , root = True
          }
        }
      , latest =
        { use = mkUse "latest" "" ""
        , useGraphic = mkUse "latest" extraGraphic ""
        }
      , useCopr =
          \(name : Text) ->
            "RUN dnf -y install dnf-plugins-core && dnf -y copr enable ${name}"
      }

let useGraphicSimple =
      \(name : Text) ->
      \(desc : Text) ->
        Podenv.Application::{
        , description = Some desc
        , runtime = base.latest.useGraphic [ name ]
        , command = [ name ]
        }

let useGraphic =
      \(desc : Text) ->
      \(pre-task : Text) ->
      \(packages : List Text) ->
        Podenv.Application::{
        , description = Some desc
        , runtime = mkUse "latest" extraGraphic "" packages
        }

let useGraphicRuntime =
      \(version : Text) ->
      \(pre-task : Text) ->
      \(packages : List Text) ->
      \(post-task : Text) ->
        mkUse version (extraGraphic ++ pre-task) post-task packages

in  base // { useGraphicSimple, useGraphic, useGraphicRuntime }
