(../Podenv.dhall).Application::{
, description = Some "A simple terminal"
, capabilities = (../Podenv.dhall).Capabilities::{
  , terminal = True
  , interactive = True
  , network = True
  }
, runtime = (../Podenv.dhall).Image "ubi8"
}
