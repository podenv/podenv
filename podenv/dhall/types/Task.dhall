{- Ansible like task -}
{ name : Optional Text
, copy : Optional ./Tasks/Copy.dhall
, shell : Optional Text
, command : Optional Text
, when : Optional Text
, unless : Optional Text
}
