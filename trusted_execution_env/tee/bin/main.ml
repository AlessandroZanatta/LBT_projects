open Ast

let example_exp =
  Let ("add_3", Fun ("n", Op (Sum, Den "n", Eint 3)), Call (Den "add_3", Eint 5))

let () =
  let res =
    match Interpreter.eval [] example_exp with
    | Int n -> string_of_int n
    | Bool b -> string_of_bool b
    | Closure (name, _, _) -> Printf.sprintf "function '%s'" name
  in
  Printf.printf "Result: %s" res
