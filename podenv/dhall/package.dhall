let schemas =
      ./schemas.dhall sha256:428c4338199a9c2d000fd30d4df8dc4779f189f5cc0022105d9f6a980ee62e9a

let {- This exports the legacy podenv package structure to keep retro compatibility
    -} backward-compat =
      { Types =
          ./types.dhall sha256:bf646578687aee0233aa847bff2fb1372df67c0e438559f80b5c25cd1786f9e2
      , Defaults =
          ./defaults.dhall sha256:2c4b81e18daa8755b1ce886805cf5ddab86375a5a2c848e6e56302a57839c44e
      , Schemas =
          ./schemas.dhall sha256:428c4338199a9c2d000fd30d4df8dc4779f189f5cc0022105d9f6a980ee62e9a
      }

in  schemas // backward-compat
