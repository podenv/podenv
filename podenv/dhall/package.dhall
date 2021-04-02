let schemas =
      ./schemas.dhall sha256:1c7c3aaa2cd3a8284c428bd3e261e5272647f6a4f9bd13f12d85c6b793f2bcfd

let {- This exports the legacy podenv package structure to keep retro compatibility
    -} backward-compat =
      { Types =
          ./types.dhall sha256:fbe09683b9428a31b6028a466b579b176584e8d33c6f2a7d5c88aa854ffae67c
      , Defaults =
          ./defaults.dhall sha256:dd49df1fa2fb872af658d16e1b6027f50f92e39c988ca3a1094bb8e2529982a2
      , Schemas =
          ./schemas.dhall sha256:1c7c3aaa2cd3a8284c428bd3e261e5272647f6a4f9bd13f12d85c6b793f2bcfd
      }

in  schemas // backward-compat
