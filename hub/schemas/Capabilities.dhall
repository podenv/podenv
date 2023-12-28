{ Type =
    { -- Process i/o
      --
      -- | Attach a terminal, imply interactive
      terminal : Bool
    , -- | Attach stdin
      interactive : Bool
    , --
      -- Process perms
      --
      -- | Run as root
      root : Bool
    , -- | Mount the rootfs as read write
      rw : Bool
    , -- | Run with extra privileges
      privileged : Bool
    , --
      -- Host service
      --
      wayland : Bool
    , pipewire : Bool
    , dbus : Bool
    , pulseaudio : Bool
    , x11 : Bool
    , ssh : Bool
    , gpg : Bool
    , --
      -- Host devices
      --
      video : Bool
    , dri : Bool
    , kvm : Bool
    , alsa : Bool
    , network : Bool
    , tun : Bool
    , --
      -- Host filesystem
      --
      -- | Mount the current working directory
      cwd : Bool
    , -- | Mount host paths given on the cli
      hostfile : Bool
    }
, -- todo: codegen the defaults
  default =
  { terminal = False
  , interactive = False
  , root = False
  , privileged = False
  , rw = False
  , wayland = False
  , pipewire = False
  , dbus = False
  , pulseaudio = False
  , x11 = False
  , ssh = False
  , gpg = False
  , video = False
  , dri = False
  , kvm = False
  , alsa = False
  , network = False
  , tun = False
  , cwd = False
  , hostfile = False
  }
}
