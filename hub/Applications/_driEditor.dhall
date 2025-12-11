\(name : Text) ->
\(desc : Text) ->
  let app = ./_graphicEditor.dhall name desc

  in  app // { capabilities = app.capabilities // { x11 = False, dri = True } }
