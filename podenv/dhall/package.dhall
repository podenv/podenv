let schemas =
      ./schemas.dhall sha256:0958dd2c57b65d298c5ca3f2ed7dc12a21915b5d2bd06d8ba8ce94007a1992db

let {- This exports the legacy podenv package structure to keep retro compatibility
    -} backward-compat =
      { Types =
          ./types.dhall sha256:db87613219e39b57770ca0aa58dd8fc6e54815638bec019d2cdba08b5f80e52c
      , Defaults =
          ./defaults.dhall sha256:af982544d78c7b0a7b227be62184c9072189d82644c97dfcc491770a521e0f29
      , Schemas =
          ./schemas.dhall sha256:0958dd2c57b65d298c5ca3f2ed7dc12a21915b5d2bd06d8ba8ce94007a1992db
      }

in  schemas // backward-compat
