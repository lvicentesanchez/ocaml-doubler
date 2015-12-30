open Opium.Std

type person = { name: string ; age: int }

let json_of_person { name ; age } =
  `O [ "name", Ezjsonm.string name
     ; "age" , Ezjsonm.int age
     ]

let person_of_json json =
  let name = Ezjsonm.(find json ["name"] |> get_string)
  in
  let age = Ezjsonm.(find json ["age"] |> get_int)
  in
  { name ; age }

let double_age_handle req =
  try%lwt
    App.json_of_body_exn req >>| fun json ->
    let source = Ezjsonm.value json |> person_of_json
    in
    let person = { source with age = source.age * 2 }
    in
    `Json (json_of_person person) |> respond ~code:`OK
  with
  | Ezjsonm.Parse_error _ | Not_found ->
    `String "Bad request" |> respond' ~code:`Bad_request

let double_age = post "/" double_age_handle

let _ =
  App.empty
  |> double_age
  |> App.run_command
