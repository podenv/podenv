let Podenv = ../Podenv.dhall

in  Podenv.Application::{
    , description = Some " Improve your Baduk skills by training with KataGo!"
    , runtime =
        (./fedora.dhall).`40`.useGraphicPost
          [ "python3-pip"
          , "xclip"
          , "mesa-libOpenCL"
          , "libzip"
          , "openssl-devel"
          , "gcc"
          , "python3-devel"
          , "mesa-libGL-devel"
          ]
          ''
          RUN python3 -mpip install katrain kivy==2.3.0
          ''
    , command = [ "/usr/local/bin/katrain" ]
    , capabilities = Podenv.Capabilities::{
      , x11 = True
      , dri = True
      , pulseaudio = True
      }
    }
