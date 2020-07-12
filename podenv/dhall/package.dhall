let schemas =
      ./schemas.dhall sha256:9cb79ae1afb72c47e7760354c3a4c8f4badf604e29d5dd959e18fade847b7eec

let {- This exports the legacy podenv package structure to keep retro compatibility
    -} backward-compat =
      { Types =
          ./types.dhall sha256:f4a8eb75c7b1413c673e1d4331b5bf56cdb3e3560dfc45b6ac0568555fb9543c
      , Defaults =
          ./defaults.dhall sha256:0d3588bb9899731decbdb0cf8a927e659287774cba1fbd3b63129e5107264d5a
      , Schemas =
          ./schemas.dhall sha256:9cb79ae1afb72c47e7760354c3a4c8f4badf604e29d5dd959e18fade847b7eec
      }

in  schemas // backward-compat
