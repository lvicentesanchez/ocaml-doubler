open Opium.Std

let double_age_handler req =
  try%lwt
    App.json_of_body_exn req >>| fun json ->
    let source = Ezjsonm.value json |> Person.of_json
    in
    let person = { source with Person.age = source.Person.age * 2 }
    in
    `Json (Person.to_json person) |> respond ~code:`OK
  with
  | Ezjsonm.Parse_error _ | Not_found ->
    `String "Bad request" |> respond' ~code:`Bad_request

let double_age = post "/" double_age_handler

let _ =
  App.empty
  |> double_age
  |> App.run_command
