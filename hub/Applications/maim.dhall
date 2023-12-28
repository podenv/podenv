let default =
          (./fedora.dhall).useSimple "maim" "Screen capturing application (x11)"
      //  { capabilities = (../Podenv.dhall).Capabilities::{ x11 = True } }

let capture = default // { command = [ "maim", "-s", "-o", "-u" ] }

in  { default, capture }
