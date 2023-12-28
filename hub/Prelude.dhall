{- This file provides a central `Prelude` import for the rest of the library to
   use so that the integrity check only needs to be updated in one place
   whenever upgrading the interpreter.

   This allows the user to provide their own Prelude import using the
   `DHALL_PRELUDE` environment variable, like this:

   ```
   $ export DHALL_PRELUDE='https://prelude.dhall-lang.org/package.dhall sha256:...'
   ```

   Note that overriding the Prelude in this way only works if this repository
   is imported locally.  Remote imports do not have access to environment
   variables and any attempt to import one will fall back to the next available
   import.  To learn more, read:

   * https://github.com/dhall-lang/dhall-lang/wiki/Safety-guarantees#cross-site-scripting-xss

   This file also provides an import without the integrity check as a slower
   fallback if the user is using a different version of the Dhall interpreter.

   This pattern is documented in the dhall-nethack repo:

   * https://github.com/dhall-lang/dhall-nethack/blob/master/Prelude.dhall
-}
  env:DHALL_PRELUDE
? { Text.concatSep
    =
      https://prelude.dhall-lang.org/v17.0.0/Text/concatSep
        sha256:e4401d69918c61b92a4c0288f7d60a6560ca99726138ed8ebc58dca2cd205e58
  , List.map
    =
      https://prelude.dhall-lang.org/v17.0.0/List/map
        sha256:dd845ffb4568d40327f2a817eb42d1c6138b929ca758d50bc33112ef3c885680
  }
