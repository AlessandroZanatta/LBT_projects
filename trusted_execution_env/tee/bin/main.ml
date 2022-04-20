open Ast
open Env
open Interpreter

let simple_tests =
  [
    ( "Using public function in mobile code.\nExpected: 1",
      Let ("myfun", Public, Fun ([], Eint 1), Execute (Call (Den "myfun", [])))
    );
    ( "Using private function in mobile code.\nExpected: error",
      Let ("myfun", Private, Fun ([], Eint 1), Execute (Call (Den "myfun", [])))
    );
    ( "Using public function in nested mobile code.\nExpected: 1",
      Execute
        (Let
           ("myfun", Public, Fun ([], Eint 1), Execute (Call (Den "myfun", []))))
    );
    ( "Using private function in nested mobile code.\nExpected: error",
      Execute
        (Let
           ("myfun", Private, Fun ([], Eint 1), Execute (Call (Den "myfun", []))))
    );
    ( "Double open in trusted code.\nExpected: ok",
      Let
        ( "x",
          Private,
          Fun ([], Open (File "file_1")),
          Let ("y", Private, Call (Den "x", []), Open (File "file_1")) ) );
    ( "Double open of sockets in trusted code.\nExpected: ok",
      Let
        ( "x",
          Private,
          Fun ([], Open (Socket ("127.0.0.1", 1234))),
          Let
            ("y", Private, Call (Den "x", []), Open (Socket ("127.0.0.1", 1234)))
        ) );
    ( "Double open in mobile code.\nExpected: error",
      Execute
        (Let
           ( "x",
             Private,
             Fun ([], Open (File "file_1")),
             Let ("y", Private, Call (Den "x", []), Open (File "file_1")) )) );
    ( "Double open in nested mobile code.\nExpected: error",
      Execute
        (Let ("x", Private, Open (File "file_1"), Execute (Open (File "file_2"))))
    );
  ]

let only_one_open =
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
          (match
             eval empty_env { active = None; sandbox = only_one_open } code
           with
          | Int n -> string_of_int n
          | Bool b -> string_of_bool b
          | Closure _ -> "function"
          | OFile f -> Printf.sprintf "File '%s'" f
          | OSocket (addr, port) -> Printf.sprintf "Socket '%s' %d" addr port
          | String x -> x)
          |> Printf.sprintf "\027[0;32m%s\027[0m"
        with Failure x -> Printf.sprintf "\027[31m%s\027[0m" x
      in
      Printf.printf "Test: %s\nResult: %s\n\n" title res;
      execute_tests t

let () = execute_tests simple_tests
