let schemas =
      ./schemas.dhall sha256:bb70b12c9151fa5a51ec3c269838b265e8abf3fee7be49d84ab02ebfac3b50ec

let {- This exports the legacy podenv package structure to keep retro compatibility
    -} backward-compat =
      { Types =
          ./types.dhall sha256:00d6bd8fcc8dbf2e698c40f6d16b005e3ff0ac8fb6d0fd41608b056d8bfbd620
      , Defaults =
          ./defaults.dhall sha256:8ba9acf76e1a8c8bace3e2583a13c238b4a8ec5e8e73064349b37b3d702da7aa
      , Schemas =
          ./schemas.dhall sha256:bb70b12c9151fa5a51ec3c269838b265e8abf3fee7be49d84ab02ebfac3b50ec
      }

in  schemas // backward-compat
