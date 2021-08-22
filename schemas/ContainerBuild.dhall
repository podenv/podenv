-- | How to build a local container image
{ Type =
    { containerfile : Text
    , -- TODO: this atttribute should be named `volumes`, but this is not possible until https://github.com/dhall-lang/dhall-haskell/pull/2285
      image_volumes : List Text
    , -- TODO: this attribute should be named `image`
      image_name : Optional Text
    , -- TODO: this attribute should be named `home`
      image_home : Optional Text
    , image_update : Optional Text
    }
, default =
  { image_volumes = [] : List Text
  , image_name = None Text
  , image_home = None Text
  , image_update = None Text
  }
}
