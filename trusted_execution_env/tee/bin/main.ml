open Ast
open Env
open Interpreter

let simple_tests =
  [
    ( "Double open in trusted code",
      Let
        ( "x",
          Private,
          Fun ([], Open (File "file_1")),
          Let ("y", Private, Call (Den "x", []), Open (File "file_1")) ) );
    ( "Double open in mobile code",
      Execute
        (Let
           ( "x",
             Private,
             Fun ([], Open (File "file_1")),
             Let ("y", Private, Call (Den "x", []), Open (File "file_1")) )) );
    ( "Double open in nested mobile code",
      Execute
        (Let ("x", Private, Open (File "file_1"), Execute (Open (File "file_2"))))
    );
  ]

let policy =
  (fun x ->
    match x with
    | Automata.State "zero_open", Automata.Open _ -> Automata.State "one_open"
    | Automata.State "one_open", Automata.Close _ -> Automata.State "zero_open"
    | _ -> Automata.Failure)
  |> Automata.init_automata (Automata.State "zero_open")

let rec execute_tests tests =
  match tests with
  | [] -> ()
  | (title, code) :: t ->
      let res =
        try
          match eval empty_env { active = None; sandbox = policy } code with
          | Int n -> string_of_int n
          | Bool b -> string_of_bool b
          | Closure _ -> "function"
        with Failure x -> x
      in
      Printf.printf "Test: %s\nResult: %s\n\n" title res;
      execute_tests t

let () = execute_tests simple_tests
