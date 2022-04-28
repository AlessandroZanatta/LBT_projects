open Lib
open Lib.Ast
open Lib.Env
open Lib.Exception

(* Interprets the given expression. This is done by typechecking it and,
   if it typechecks, by evaluating it against the given policy. *)
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
    | Automaton.State "closed", Automaton.EOpen (File ("file.txt", _)) ->
        Automaton.State "opened"
    | Automaton.State "opened", Automaton.EClose (File ("file.txt", _)) ->
        Automaton.State "closed"
    | Automaton.State "opened", Automaton.ERead (File ("file.txt", _)) ->
        Automaton.State "opened"
    | Automaton.State "opened", Automaton.EWrite (File ("file.txt", _)) ->
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

let null_policy =
  (function _ -> Automaton.State "loop")
  |> Automaton.init_automaton (Automaton.State "loop")

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
              TOpenable,
              Private,
              Execute (Open (EOpenable (File ("file1", O_RDWR))), TOpenable),
              Execute (Open (EOpenable (File ("file2", O_RDONLY))), TOpenable)
            ),
          Some (OFile ("file2", O_RDONLY)) );
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
              Fun ([], TOpenable, Open (EOpenable (File ("file_1", O_RDONLY)))),
              Let
                ( "y",
                  TOpenable,
                  Private,
                  Call (Den ("x", Private), []),
                  Open (EOpenable (File ("file_1", O_WRONLY))) ) ),
          Some (OFile ("file_1", O_WRONLY)) );
        ( "Double open of sockets in trusted code",
          Let
            ( "x",
              TClosure,
              Private,
              Fun ([], TOpenable, Open (EOpenable (Socket ("127.0.0.1", 1234)))),
              Let
                ( "y",
                  TOpenable,
                  Private,
                  Call (Den ("x", Private), []),
                  Open (EOpenable (Socket ("127.0.0.1", 1234))) ) ),
          Some (OSocket ("127.0.0.1", 1234)) );
        ( "Double open in mobile code",
          Execute
            ( Let
                ( "x",
                  TClosure,
                  Private,
                  Fun
                    ([], TOpenable, Open (EOpenable (File ("file_1", O_RDONLY)))),
                  Let
                    ( "y",
                      TOpenable,
                      Private,
                      Call (Den ("x", Private), []),
                      Open (EOpenable (File ("file_1", O_RDONLY))) ) ),
              TOpenable ),
          None );
        ( "Double open in nested mobile code",
          Execute
            ( Let
                ( "_",
                  TOpenable,
                  Private,
                  Open (EOpenable (File ("file_1", O_RDONLY))),
                  Execute
                    (Open (EOpenable (File ("file_2", O_RDONLY))), TOpenable) ),
              TOpenable ),
          None );
      ] );
    ( can_only_open_file_txt,
      [
        ( "Write without open",
          Execute
            ( Write (EOpenable (File ("file_1", O_RDWR)), EString "test"),
              TString ),
          None );
        ( "Open, read, write and close file.txt",
          Execute
            ( Let
                ( "_",
                  TOpenable,
                  Private,
                  Open (EOpenable (File ("file.txt", O_RDWR))),
                  Let
                    ( "_",
                      TString,
                      Private,
                      Write
                        ( EOpenable (File ("file.txt", O_RDWR)),
                          Call (Fun ([], TString, EString "test!"), []) ),
                      Read (EOpenable (File ("file.txt", O_RDWR))) ) ),
              TString ),
          Some (String "Read from 'file.txt' (mode: O_RDWR)") );
        ( "Open another file",
          Execute (Open (EOpenable (File ("/etc/passwd", O_RDONLY))), TOpenable),
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
                  Write
                    ( EOpenable (Socket ("localhost", 1234)),
                      EString "test in network" ),
                  Read (EOpenable (File ("my_file", O_RDONLY))) ),
              TString ),
          Some (String "Read from 'my_file' (mode: O_RDONLY)") );
        ( "Reading a file then trying to write on a socket",
          Execute
            ( Let
                ( "_",
                  TString,
                  Private,
                  Read (EOpenable (File ("my_file", O_RDWR))),
                  Write
                    ( EOpenable (Socket ("localhost", 1234)),
                      Call (Fun ([], TString, EString "test!"), []) ) ),
              TString ),
          None );
      ] );
    ( null_policy,
      [
        ("Runtime error when dividing by zero", Op (Div, EInt 1, EInt 0), None);
        ("Typecheck error test #1", Open (EInt 5), None);
        ("Typecheck error test #2", Read (EInt 5), None);
        ("Typecheck error test #3", Write (EInt 5, EString ""), None);
        ( "Typecheck error test #4",
          Write (EOpenable (File ("file", O_RDWR)), EInt 5),
          None );
        ( "Typecheck error test #5",
          Write (EOpenable (File ("file", O_RDONLY)), EString ""),
          None );
        ("Typecheck error test #6", If (EInt 5, EInt 2, EInt 3), None);
        ("Typecheck error test #7", If (EBool true, EInt 2, EString ""), None);
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
            | OFile (f, am) -> (
                match am with
                | Ast.O_RDONLY -> "O_RDONLY"
                | Ast.O_WRONLY -> "O_WRONLY"
                | Ast.O_RDWR ->
                    "O_RDWR" |> Printf.sprintf "File '%s' (mode: %s)" f)
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
