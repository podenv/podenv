let Podenv = env:PODENV

let runtime = Podenv.Hub.fedora.latest.useGraphic [ "sway", "wayvnc" ]

in  { default = Podenv.Application::{
      , runtime
      , command = [ "sway" ]
      , environ =
        [ "WLR_BACKENDS=headless"
        , "WLR_RENDERER=pixman"
        , "WLR_LIBINPUT_NO_DEVICES=1"
        ]
      }
    , vnc = Podenv.Application::{ runtime, command = [ "wayvnc" ] }
    }
