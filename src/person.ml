open Opium.Std

type t = { name: string ; age: int }

let of_json json =
  let name = Ezjsonm.(find json ["name"] |> get_string)
  in
  let age = Ezjsonm.(find json ["age"] |> get_int)
  in
  { name; age }

let to_json { name ; age } =
  `O [ "name", Ezjsonm.string name
     ; "age" , Ezjsonm.int age
     ]
