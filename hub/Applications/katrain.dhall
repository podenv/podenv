let Podenv = ../Podenv.dhall

in  Podenv.Application::{
    , description = Some " Improve your Baduk skills by training with KataGo!"
    , runtime =
        (./fedora.dhall).useGraphicRuntime
          "40"
          ""
          [ "python3-pip"
          , "xclip"
          , "mesa-libOpenCL"
          , "libzip"
          , "openssl-devel"
          , "gcc"
          , "python3-devel"
          , "mesa-libGL-devel"
          , "sdl2-compat-devel"
          , "ffmpeg-devel"
          ]
          ''
          RUN python3 -mpip install katrain
          ''
    , command = [ "/usr/local/bin/katrain" ]
    , capabilities = Podenv.Capabilities::{
      , x11 = True
      , dri = True
      , pulseaudio = True
      }
    }
