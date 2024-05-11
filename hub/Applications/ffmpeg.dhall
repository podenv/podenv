let Podenv = ../Podenv.dhall

let default =
      Podenv.Application::{
      , description = Some
          "A complete, cross-platform solution to record, convert and stream audio and video"
      , runtime = (./fedora.dhall).latest.useGraphicCodec [ "ffmpeg" ]
      , command = [ "ffmpeg" ]
      , capabilities = Podenv.Capabilities::{ dri = True, hostfile = True }
      }

in  { default, ffprobe = default // { command = [ "ffprobe" ] } }
