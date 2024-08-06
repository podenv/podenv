let Podenv = ../Podenv.dhall

in  Podenv.Application::{
    , description = Some " Improve your Baduk skills by training with KataGo!"
    , runtime =
        (./fedora.dhall).latest.useGraphicPost
          [ "python3-pip", "xclip" ]
          ''
          RUN dnf install -y mesa-libOpenCL libzip openssl-devel
          RUN python3 -mpip install katrain
          ''
    , command = [ "/usr/local/bin/katrain" ]
    , capabilities = Podenv.Capabilities::{
      , x11 = True
      , dri = True
      , pulseaudio = True
      }
    }
