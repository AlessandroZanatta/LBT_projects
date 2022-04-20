open Ast
open Env
open Interpreter

let only_one_open =
  (function
    | Automaton.State "zero_open", Automaton.Open _ ->
        Automaton.State "one_open"
    | Automaton.State "one_open", Automaton.Close _ ->
        Automaton.State "zero_open"
    | _ -> Automaton.Failure)
  |> Automaton.init_automaton (Automaton.State "zero_open")

let can_only_open_file_txt =
  (function
    | Automaton.State "closed", Automaton.Open (File "file.txt") ->
        Automaton.State "opened"
    | Automaton.State "opened", Automaton.Close (File "file.txt") ->
        Automaton.State "closed"
    | Automaton.State "opened", Automaton.Read (File "file.txt") ->
        Automaton.State "opened"
    | Automaton.State "opened", Automaton.Write (File "file.txt") ->
        Automaton.State "opened"
    | _ -> Failure)
  |> Automaton.init_automaton (Automaton.State "closed")

let no_send_after_read =
  (function
    | Automaton.State "comm", Automaton.Write (Socket (_, _)) ->
        Automaton.State "comm"
    | Automaton.State "comm", Automaton.Read (Socket (_, _)) ->
        Automaton.State "comm"
    | Automaton.State "comm", Automaton.Read (File _) ->
        Automaton.State "reading"
    | _ -> Failure)
  |> Automaton.init_automaton (Automaton.State "comm")

let tests =
  [
    ( only_one_open,
      [
        ( "Using public function in mobile code.\nExpected: 1",
          Let
            ("myfun", Public, Fun ([], Eint 1), Execute (Call (Den "myfun", [])))
        );
        ( "Using private function in mobile code.\nExpected: error",
          Let
            ( "myfun",
              Private,
              Fun ([], Eint 1),
              Execute (Call (Den "myfun", [])) ) );
        ( "Using public function in nested mobile code.\nExpected: 1",
          Execute
            (Let
               ( "myfun",
                 Public,
                 Fun ([], Eint 1),
                 Execute (Call (Den "myfun", [])) )) );
        ( "Using private function in nested mobile code.\nExpected: error",
          Execute
            (Let
               ( "myfun",
                 Private,
                 Fun ([], Eint 1),
                 Execute (Call (Den "myfun", [])) )) );
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
                ( "y",
                  Private,
                  Call (Den "x", []),
                  Open (Socket ("127.0.0.1", 1234)) ) ) );
        ( "Double open in mobile code.\nExpected: error",
          Execute
            (Let
               ( "x",
                 Private,
                 Fun ([], Open (File "file_1")),
                 Let ("y", Private, Call (Den "x", []), Open (File "file_1")) ))
        );
        ( "Double open in nested mobile code.\nExpected: error",
          Execute
            (Let
               ( "x",
                 Private,
                 Open (File "file_1"),
                 Execute (Open (File "file_2")) )) );
      ] );
    ( can_only_open_file_txt,
      [
        ( "Write without open.\nExpected: error",
          Execute (Write (File "file.txt")) );
        ( "Open, read, write and close file.txt",
          Execute
            (Let
               ( "x",
                 Private,
                 Open (File "file.txt"),
                 Let
                   ( "_",
                     Private,
                     Write (File "file.txt"),
                     Read (File "file.txt") ) )) );
        ( "Open another file.\nExpected: error",
          Execute (Open (File "/etc/passwd")) );
      ] );
    ( no_send_after_read,
      [
        ( "Sending and receiving from socket is (the only) allowed.\n\
           Expected: ok",
          Execute
            (Let
               ( "_",
                 Private,
                 Write (Socket ("localhost", 1234)),
                 Read (File "my_file") )) );
        ( "Reading a file then trying to write on a socket.\nExpected: error",
          Execute
            (Let
               ( "_",
                 Private,
                 Read (File "my_file"),
                 Write (Socket ("localhost", 1234)) )) );
      ] );
  ]

let rec execute_test_for_policy (policy, tests) =
  match tests with
  | [] -> ()
  | (title, code) :: t ->
      let res =
        try
          (match
             eval empty_env
               { active = None; sandbox = Utils.copy_automaton policy }
               code
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
      execute_test_for_policy (policy, t)

let rec execute_tests tests =
  match tests with
  | [] -> ()
  | h :: t ->
      execute_test_for_policy h;
      execute_tests t

let () = execute_tests tests
