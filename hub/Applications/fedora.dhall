let Prelude = ../Prelude.dhall

let Podenv = ../Podenv.dhall

let fedora = ../Builders/fedora.dhall

let mkUse =
      \(version : Text) ->
      \(pre-task : Text) ->
      \(packages : List Text) ->
        Podenv.Container (fedora.image version pre-task packages)

let default =
      \(version : Text) ->
        Podenv.Application::{
        , runtime = Podenv.Image (fedora.image-ref (":" ++ version))
        , volumes = fedora.mkVolumes version
        , capabilities = Podenv.Capabilities::{
          , terminal = True
          , interactive = True
          , network = True
          , rw = True
          , root = True
          }
        }

let -- | Create a default app and use function
    useD =
      \(version : Text) -> { default = default version, use = mkUse version "" }

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

      in  ''
          RUN dnf install -y ${Prelude.Text.concatSep " " extra-pkgs}
          ''

let -- | Extra layer to install codec
    -- See: https://fedoraproject.org/wiki/Firefox_Hardware_acceleration
    extraGraphicCodec =
      let -- | A list of packages that are often required but missing from the requirements
          gfx-pkgs =
            [ "ffmpeg"
            , "libva"
            , "libva-utils"
            , "libva-intel-driver"
            , "libvdpau-va-gl"
            ]

      let repo = fusion-repo "free" "\$(rpm -E %fedora)"

      in  ''
          RUN dnf install -y ${repo} && dnf install -y ${Prelude.Text.concatSep
                                                           " "
                                                           gfx-pkgs}
          ''

let -- | Simpler helper where app == package == command
    simples =
      let -- | When the application name is the package and command name
          mkUseSimple =
            \(extra : Text) ->
            \(name : Text) ->
            \(desc : Text) ->
              Podenv.Application::{
              , description = Some desc
              , runtime = mkUse "latest" extra [ name ]
              , command = [ name ]
              }

      in  { useSimple = mkUseSimple ""
          , useGraphicSimple = mkUseSimple extraGraphic
          , useGraphicCodecSimple =
              mkUseSimple (extraGraphic ++ extraGraphicCodec)
          }

let -- | Base image access for extra customization
    images =
      let mkUseImage =
            \(extra : Text) ->
            \(version : Text) ->
            \(pre-task : Text) ->
              fedora.image version (extra ++ pre-task)

      in  { useImage = mkUseImage ""
          , useGraphicImage = mkUseImage extraGraphic
          , useGraphicCodecImage =
              mkUseImage (extraGraphic ++ extraGraphicCodec)
          }

let base =
      { default = default "latest"
      , latest =
        { use = mkUse "latest" ""
        , useGraphic = mkUse "latest" extraGraphic
        , useGraphicCodec = mkUse "latest" (extraGraphic ++ extraGraphicCodec)
        }
      , `34` = useD "34"
      , rawhide = useD "rawhide"
      , fusion =
          let ver = "34"

          let mkVariant =
                \(variant : Text) ->
                \(extra : Text) ->
                  { use =
                      mkUse
                        ver
                        (     extra
                          ++  "RUN dnf install -y "
                          ++  fusion-repo variant ver
                        )
                  }

          in      mkVariant "free" ""
              //  { nonfree =
                      mkVariant "nonfree" (extraGraphic ++ extraGraphicCodec)
                  }
      , useCopr =
          \(name : Text) ->
            "RUN dnf -y install dnf-plugins-core && dnf -y copr enable ${name}"
      }

in  base // simples // images
