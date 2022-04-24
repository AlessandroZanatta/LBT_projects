open Ast
open Env
open Interpreter

(********************)
(* EXAMPLE POLICIES *)
(********************)

(* Only a single file can be open at a time *)
let only_one_open =
  (function
    | Automaton.State "zero_open", Automaton.EOpen _ ->
        Automaton.State "one_open"
    | Automaton.State "one_open", Automaton.EClose _ ->
        Automaton.State "zero_open"
    | _ -> Automaton.Failure)
  |> Automaton.init_automaton (Automaton.State "zero_open")

let can_only_open_file_txt =
  (function
    | Automaton.State "closed", Automaton.EOpen (File "file.txt") ->
        Automaton.State "opened"
    | Automaton.State "opened", Automaton.EClose (File "file.txt") ->
        Automaton.State "closed"
    | Automaton.State "opened", Automaton.ERead (File "file.txt") ->
        Automaton.State "opened"
    | Automaton.State "opened", Automaton.EWrite (File "file.txt") ->
        Automaton.State "opened"
    | _ -> Failure)
  |> Automaton.init_automaton (Automaton.State "closed")

let no_send_after_read =
  (function
    | Automaton.State "comm", Automaton.EWrite (Socket (_, _)) ->
        Automaton.State "comm"
    | Automaton.State "comm", Automaton.ERead (Socket (_, _)) ->
        Automaton.State "comm"
    | Automaton.State "comm", Automaton.ERead (File _) ->
        Automaton.State "reading"
    | _ -> Failure)
  |> Automaton.init_automaton (Automaton.State "comm")

(***********************************)
(* EXAMPLES USING DEFINED POLICIES *)
(***********************************)

let examples =
  [
    ( only_one_open,
      [
        ( "Anonymous functions work",
          Call (Fun ([ "x" ], Op (Sum, Den "x", Eint 1)), [ Eint 3 ]),
          Some (Int 4) );
        ( "Non-nested execute are enforced separately",
          Let
            ( "x",
              Private,
              Execute (Open (File "file1")),
              Execute (Open (File "file2")) ),
          Some (OFile "file2") );
        ( "Using public function in mobile code",
          Let
            ("myfun", Public, Fun ([], Eint 1), Execute (Call (Den "myfun", []))),
          Some (Int 1) );
        ( "Using private function in mobile code",
          Let
            ( "myfun",
              Private,
              Fun ([], Eint 1),
              Execute (Call (Den "myfun", [])) ),
          None );
        ( "Using public function in nested mobile code",
          Execute
            (Let
               ( "myfun",
                 Public,
                 Fun ([], Eint 1),
                 Execute (Call (Den "myfun", [])) )),
          Some (Int 1) );
        ( "Using private function in nested mobile code",
          Execute
            (Let
               ( "myfun",
                 Private,
                 Fun ([], Eint 1),
                 Execute (Call (Den "myfun", [])) )),
          None );
        ( "Double open in trusted code",
          Let
            ( "x",
              Private,
              Fun ([], Open (File "file_1")),
              Let ("y", Private, Call (Den "x", []), Open (File "file_1")) ),
          Some (OFile "file_1") );
        ( "Double open of sockets in trusted code",
          Let
            ( "x",
              Private,
              Fun ([], Open (Socket ("127.0.0.1", 1234))),
              Let
                ( "y",
                  Private,
                  Call (Den "x", []),
                  Open (Socket ("127.0.0.1", 1234)) ) ),
          Some (OSocket ("127.0.0.1", 1234)) );
        ( "Double open in mobile code",
          Execute
            (Let
               ( "x",
                 Private,
                 Fun ([], Open (File "file_1")),
                 Let ("y", Private, Call (Den "x", []), Open (File "file_1")) )),
          None );
        ( "Double open in nested mobile code",
          Execute
            (Let
               ( "x",
                 Private,
                 Open (File "file_1"),
                 Execute (Open (File "file_2")) )),
          None );
      ] );
    ( can_only_open_file_txt,
      [
        ("Write without open", Execute (Write (File "file.txt")), None);
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
                     Read (File "file.txt") ) )),
          Some (String "Read from 'file.txt'") );
        ("Open another file", Execute (Open (File "/etc/passwd")), None);
      ] );
    ( no_send_after_read,
      [
        ( "Sending and receiving from socket is (the only) allowed",
          Execute
            (Let
               ( "_",
                 Private,
                 Write (Socket ("localhost", 1234)),
                 Read (File "my_file") )),
          Some (String "Read from 'my_file'") );
        ( "Reading a file then trying to write on a socket",
          Execute
            (Let
               ( "_",
                 Private,
                 Read (File "my_file"),
                 Write (Socket ("localhost", 1234)) )),
          None );
      ] );
  ]

(****************)
(* RUN EXAMPLES *)
(****************)
let rec execute_examples_for_policy (policy, examples) =
  match examples with
  | [] -> ()
  | (title, code, expected_result) :: t ->
      let res =
        try
          let res =
            eval empty_env
              { active = None; sandbox = Utils.copy_automaton policy }
              code
          in
          if Some res = expected_result then "\027[0;32mPassed\027[0m"
          else
            match res with
            | Int n -> string_of_int n
            | Bool b -> string_of_bool b
            | Closure _ -> "function"
            | OFile f -> Printf.sprintf "File '%s'" f
            | OSocket (addr, port) -> Printf.sprintf "Socket '%s' %d" addr port
            | String x ->
                x |> Printf.sprintf "\027[0;31mFailure, expected: %s\027[0m"
        with Failure _ ->
          if Option.is_none expected_result then "\027[0;32mPassed\027[0m"
          else
            Printf.sprintf
              "\027[0;31mCode execution was terminated by EM!\027[0m"
      in
      Printf.printf "%s... %s\n" title res;
      execute_examples_for_policy (policy, t)

let rec execute_examples examples =
  match examples with
  | [] -> ()
  | h :: t ->
      execute_examples_for_policy h;
      execute_examples t

let () = execute_examples examples
