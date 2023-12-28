let Podenv = ../Podenv.dhall

in  \(rootfs : Text) ->
      Podenv.Application::{
      , description = Some "Start a privileged shell with a rootfs"
      , runtime = Podenv.Rootfs rootfs
      , syscaps = [ "CHOWN", "DAC_OVERRIDE" ]
      , command = [ "bash", "--rcfile", "/etc/profile" ]
      , capabilities = Podenv.Capabilities::{
        , terminal = True
        , interactive = True
        , network = True
        , rw = True
        , root = True
        }
      }
