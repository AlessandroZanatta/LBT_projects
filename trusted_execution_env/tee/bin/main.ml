open Ast

let example_exp =
  Let
    ( "x",
      Eint 11111,
      Let
        ( "add_x_y_z",
          Fun ([ "x"; "y"; "z" ], Op (Sum, Op (Sum, Den "x", Den "y"), Den "z")),
          Call (Den "add_x_y_z", [ Eint 5; Eint 7; Eint 10 ]) ) )

let () =
  let res =
    match Interpreter.eval [] example_exp with
    | Int n -> string_of_int n
    | Bool b -> string_of_bool b
    | Closure _ -> "function"
  in
  Printf.printf "Result: %s" res
