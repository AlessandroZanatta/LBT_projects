open Lib
open Lib.Ast
open Lib.Env
open Lib.Exception

(* Interprets the given expression. This is done by typechecking it and,
   if it typechecks, by evaluating it. *)
let interpret e policy =
  e |> Type_checker.typecheck |> Interpreter.eval [] policy

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
          Call
            ( Fun ([ ("x", TInt) ], TInt, Op (Sum, Den ("x", Private), EInt 1)),
              [ EInt 3 ] ),
          Some (Int 4) );
        ( "Non-nested execute are enforced separately",
          Let
            ( "x",
              TOpeanable,
              Private,
              Execute (Open (File "file1"), TOpeanable),
              Execute (Open (File "file2"), TOpeanable) ),
          Some (OFile "file2") );
        ( "Using public function in mobile code",
          Let
            ( "myfun",
              TClosure,
              Public,
              Fun ([], TInt, EInt 1),
              Execute (Call (Den ("myfun", Public), []), TInt) ),
          Some (Int 1) );
        ( "Using private function in mobile code",
          Let
            ( "myfun",
              TClosure,
              Private,
              Fun ([], TInt, EInt 1),
              Execute (Call (Den ("myfun", Private), []), TInt) ),
          None );
        ( "Using public function in nested mobile code",
          Execute
            ( Let
                ( "myfun",
                  TClosure,
                  Public,
                  Fun ([], TInt, EInt 1),
                  Execute (Call (Den ("myfun", Public), []), TInt) ),
              TInt ),
          Some (Int 1) );
        ( "Using private function in nested mobile code",
          Execute
            ( Let
                ( "myfun",
                  TClosure,
                  Private,
                  Fun ([], TInt, EInt 1),
                  Execute (Call (Den ("myfun", Private), []), TInt) ),
              TInt ),
          None );
        ( "Double open in trusted code",
          Let
            ( "x",
              TClosure,
              Private,
              Fun ([], TOpeanable, Open (File "file_1")),
              Let
                ( "y",
                  TOpeanable,
                  Private,
                  Call (Den ("x", Private), []),
                  Open (File "file_1") ) ),
          Some (OFile "file_1") );
        ( "Double open of sockets in trusted code",
          Let
            ( "x",
              TClosure,
              Private,
              Fun ([], TOpeanable, Open (Socket ("127.0.0.1", 1234))),
              Let
                ( "y",
                  TOpeanable,
                  Private,
                  Call (Den ("x", Private), []),
                  Open (Socket ("127.0.0.1", 1234)) ) ),
          Some (OSocket ("127.0.0.1", 1234)) );
        ( "Double open in mobile code",
          Execute
            ( Let
                ( "x",
                  TClosure,
                  Private,
                  Fun ([], TOpeanable, Open (File "file_1")),
                  Let
                    ( "y",
                      TOpeanable,
                      Private,
                      Call (Den ("x", Private), []),
                      Open (File "file_1") ) ),
              TOpeanable ),
          None );
        ( "Double open in nested mobile code",
          Execute
            ( Let
                ( "_",
                  TOpeanable,
                  Private,
                  Open (File "file_1"),
                  Execute (Open (File "file_2"), TOpeanable) ),
              TOpeanable ),
          None );
      ] );
    ( can_only_open_file_txt,
      [
        ("Write without open", Execute (Write (File "file.txt"), TString), None);
        ( "Open, read, write and close file.txt",
          Execute
            ( Let
                ( "_",
                  TOpeanable,
                  Private,
                  Open (File "file.txt"),
                  Let
                    ( "_",
                      TString,
                      Private,
                      Write (File "file.txt"),
                      Read (File "file.txt") ) ),
              TString ),
          Some (String "Read from 'file.txt'") );
        ( "Open another file",
          Execute (Open (File "/etc/passwd"), TOpeanable),
          None );
      ] );
    ( no_send_after_read,
      [
        ( "Sending and receiving from socket is (the only) allowed",
          Execute
            ( Let
                ( "_",
                  TString,
                  Private,
                  Write (Socket ("localhost", 1234)),
                  Read (File "my_file") ),
              TString ),
          Some (String "Read from 'my_file'") );
        ( "Reading a file then trying to write on a socket",
          Execute
            ( Let
                ( "_",
                  TString,
                  Private,
                  Read (File "my_file"),
                  Write (Socket ("localhost", 1234)) ),
              TString ),
          None );
        ("ciao", Op (Div, EInt 1, EInt 0), None);
      ] );
  ]

(****************)
(* RUN EXAMPLES *)
(****************)
let rec execute_examples_for_policy (policy, examples) =
  match examples with
  | [] -> ()
  | (title, e, expected_result) :: t ->
      let res =
        try
          let res =
            interpret e
              {
                Interpreter.active = None;
                sandbox = Utils.copy_automaton policy;
              }
          in
          if Some res = expected_result then "\027[0;32mPassed\027[0m"
          else
            (match res with
            | Int n -> string_of_int n
            | Bool b -> string_of_bool b
            | Closure _ -> "function"
            | OFile f -> Printf.sprintf "File '%s'" f
            | OSocket (addr, port) -> Printf.sprintf "Socket '%s' %d" addr port
            | String x -> x)
            |> Printf.sprintf "\027[0;31mFailure, got: %s\027[0m"
        with
        | RuntimeError x ->
            if Option.is_none expected_result then "\027[0;32mPassed\027[0m"
            else Printf.sprintf "\027[0;31m[RuntimeError] %s\027[0m" x
        | TypecheckError x ->
            if Option.is_none expected_result then "\027[0;32mPassed\027[0m"
            else Printf.sprintf "\027[0;31m[TypecheckError] %s\027[0m" x
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
