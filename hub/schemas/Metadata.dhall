{ Type =
    { name : Optional Text
    , namespace : Optional Text
    , labels : List { mapKey : Text, mapValue : Text }
    }
, default =
  { name = None Text
  , namespace = None Text
  , labels = [] : List { mapKey : Text, mapValue : Text }
  }
}
