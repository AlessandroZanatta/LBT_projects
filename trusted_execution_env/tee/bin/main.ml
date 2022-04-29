open Lib
open Lib.Ast
open Lib.Env
open Lib.Exception
open Lib.Automaton

(* Interprets the given expression. This is done by typechecking it and,
   if it typechecks, by evaluating it against the given policy. *)
let interpret e policy =
  e |> Type_checker.typecheck |> Interpreter.eval empty_env policy

(********************)
(* EXAMPLE POLICIES *)
(********************)

(* Only a single file can be open at a time *)
let only_one_open =
  (function
    | State "zero_open", EOpen _ -> State "one_open"
    | State "one_open", EClose _ -> State "zero_open"
    | _ -> Failure)
  |> init_automaton (State "zero_open")

let can_only_open_file_txt =
  (function
    | State "closed", EOpen (File "file.txt") -> State "opened"
    | State "opened", EClose (File "file.txt") -> State "closed"
    | State "opened", ERead (File "file.txt") -> State "opened"
    | State "opened", EWrite (File "file.txt") -> State "opened"
    | _ -> Failure)
  |> init_automaton (State "closed")

let no_send_after_read =
  (function
    | State "comm", EWrite (Socket (_, _)) -> State "comm"
    | State "comm", ERead (Socket (_, _)) -> State "comm"
    | State "comm", ERead (File _) -> State "reading"
    | _ -> Failure)
  |> init_automaton (State "comm")

let null_policy =
  (function _ -> State "loop") |> init_automaton (State "loop")

(***********************************)
(* EXAMPLES USING DEFINED POLICIES *)
(***********************************)

let examples =
  [
    ( only_one_open,
      [
        ( "Anonymous functions work",
          Call
            ( Fun
                ( [ ("x", TInt) ],
                  TInt,
                  Op (Sum, Den "x", EInt (1, [ Readable ])),
                  [] ),
              [ EInt (3, [ Readable ]) ] ),
          Some (Int 4) );
        ( "Non-nested execute are enforced separately",
          Let
            ( "x",
              TOpenable,
              Execute (Open (EOpenable (File "file1", [])), TOpenable),
              Execute (Open (EOpenable (File "file2", [])), TOpenable) ),
          Some (OFile "file2") );
        ( "Using public function in mobile code",
          Let
            ( "myfun",
              TClosure,
              Fun ([], TInt, EInt (1, [ Readable ]), [ Readable ]),
              Execute (Call (Den "myfun", []), TInt) ),
          Some (Int 1) );
        ( "Using private function in mobile code",
          Let
            ( "myfun",
              TClosure,
              Fun ([], TInt, EInt (1, [ Readable ]), []),
              Execute (Call (Den "myfun", []), TInt) ),
          None );
        ( "Using public function in nested mobile code",
          Execute
            ( Let
                ( "myfun",
                  TClosure,
                  Fun ([], TInt, EInt (1, []), [ Readable ]),
                  Execute (Call (Den "myfun", []), TInt) ),
              TInt ),
          Some (Int 1) );
        ( "Using private function in nested mobile code",
          Execute
            ( Let
                ( "myfun",
                  TClosure,
                  Fun ([], TInt, EInt (1, []), []),
                  Execute (Call (Den "myfun", []), TInt) ),
              TInt ),
          None );
        ( "Using in mobile code a function declared in mobile code",
          Execute
            ( Let
                ( "myfun",
                  TClosure,
                  Fun ([], TInt, EInt (1, []), []),
                  Call (Den "myfun", []) ),
              TInt ),
          Some (Int 1) );
        ( "Using in nested mobile code a function declared in mobile code",
          Execute
            ( Let
                ( "myfun",
                  TClosure,
                  Fun ([], TInt, EInt (1, []), []),
                  Execute (Call (Den "myfun", []), TInt) ),
              TInt ),
          None );
        ( "Double open in trusted code",
          Let
            ( "open_file1",
              TClosure,
              Fun ([], TOpenable, Open (EOpenable (File "file_1", [])), []),
              Let
                ( "y",
                  TOpenable,
                  Call (Den "open_file1", []),
                  Open (EOpenable (File "file_1", [])) ) ),
          Some (OFile "file_1") );
        ( "Double open of sockets in trusted code",
          Let
            ( "x",
              TClosure,
              Fun
                ( [],
                  TOpenable,
                  Open (EOpenable (Socket ("127.0.0.1", 1234), [])),
                  [] ),
              Let
                ( "y",
                  TOpenable,
                  Call (Den "x", []),
                  Open (EOpenable (Socket ("127.0.0.1", 1234), [])) ) ),
          Some (OSocket ("127.0.0.1", 1234)) );
        ( "Double open in mobile code",
          Execute
            ( Let
                ( "open_file1",
                  TClosure,
                  Fun
                    ( [],
                      TOpenable,
                      Open (EOpenable (File "file_1", [])),
                      [ Readable ] ),
                  Let
                    ( "y",
                      TOpenable,
                      Call (Den "open_file1", []),
                      Open (EOpenable (File "file_1", [])) ) ),
              TOpenable ),
          None );
        ( "Double open in nested mobile code",
          Execute
            ( Let
                ( "_",
                  TOpenable,
                  Open (EOpenable (File "file_1", [])),
                  Execute (Open (EOpenable (File "file_2", [])), TOpenable) ),
              TOpenable ),
          None );
      ] );
    ( can_only_open_file_txt,
      [
        ( "Write without open",
          Execute
            ( Write
                ( EOpenable (File "file_1", []),
                  EString ("test", [ Readable; Sendable ]) ),
              TString ),
          None );
        ( "Open, read, write and close file.txt",
          Execute
            ( Let
                ( "_",
                  TOpenable,
                  Open (EOpenable (File "file.txt", [])),
                  Let
                    ( "_",
                      TString,
                      Write
                        ( EOpenable (File "file.txt", []),
                          Call
                            ( Fun
                                ( [],
                                  TString,
                                  EString ("test!", [ Readable; Sendable ]),
                                  [] ),
                              [] ) ),
                      Read (EOpenable (File "file.txt", [])) ) ),
              TString ),
          Some (String "Read from 'file.txt'") );
        ( "Open another file",
          Execute (Open (EOpenable (File "/etc/passwd", [])), TOpenable),
          None );
      ] );
    ( no_send_after_read,
      [
        ( "Sending and receiving from socket is (the only) allowed action",
          Execute
            ( Let
                ( "_",
                  TString,
                  Write
                    ( EOpenable (Socket ("localhost", 1234), []),
                      EString ("test in network", [ Readable; Sendable ]) ),
                  Read (EOpenable (File "my_file", [])) ),
              TString ),
          Some (String "Read from 'my_file'") );
        ( "Reading a file then trying to write on a socket",
          Execute
            ( Let
                ( "_",
                  TString,
                  Read (EOpenable (File "my_file", [])),
                  Write
                    ( EOpenable (Socket ("localhost", 1234), []),
                      Call
                        ( Fun
                            ( [],
                              TString,
                              EString ("test!", [ Readable; Sendable ]),
                              [] ),
                          [] ) ) ),
              TString ),
          None );
      ] );
    ( null_policy,
      [
        ( "Runtime error when dividing by zero",
          Op (Div, EInt (1, []), EInt (0, [])),
          None );
        ("Typecheck error test #1", Open (EInt (5, [])), None);
        ("Typecheck error test #2", Read (EInt (5, [])), None);
        ("Typecheck error test #3", Write (EInt (5, []), EString ("", [])), None);
        ( "Typecheck error test #4",
          Write (EOpenable (File "file", []), EInt (5, [])),
          None );
        ( "Typecheck error test #5",
          If (EInt (5, []), EInt (0, []), EInt (1, [])),
          None );
        ( "Typecheck error test #6",
          If (EBool (true, []), EInt (5, []), EString ("", [])),
          None );
        ( "Leak test #1",
          Let
            ( "my_pin",
              TString,
              EString ("my_p4$$w0rd", []),
              Let
                ( "my_pin_copy",
                  TString,
                  Den "my_pin",
                  Execute (Den "my_pin_copy", TString) ) ),
          None );
        ( "Leak test #2",
          Let
            ( "my_pin",
              TString,
              EString ("my_p4$$w0rd", []),
              Let
                ( "f",
                  TClosure,
                  Fun ([ ("x", TString) ], TString, Den "x", [ Readable ]),
                  Let
                    ( "my_pin_copy",
                      TString,
                      Call (Den "f", [ Den "my_pin" ]),
                      Execute (Den "my_pin_copy", TString) ) ) ),
          None );
        ( "Leak test #3",
          Let
            ( "my_pin",
              TInt,
              EInt (1234, []),
              Let
                ( "f",
                  TClosure,
                  Fun
                    ( [ ("x", TInt) ],
                      TInt,
                      Op (Sum, Den "x", EInt (1, [ Readable; Sendable ])),
                      [ Readable ] ),
                  Let
                    ( "my_pin_plus_1",
                      TInt,
                      Call (Den "f", [ Den "my_pin" ]),
                      Execute (Den "my_pin_plus_1", TInt) ) ) ),
          None );
        ( "Leak test #4. This test fails, as we execute an untrusted function \
           received from an untrusted mobile code. The function is executed \
           with top-level permissions, leading to leakage. This may be fixed \
           by consider the received function as untrustworthy, and by \
           therefore executing it in a secure environment",
          Let
            ( "my_pin",
              TString,
              EString ("my_p4$$w0rd", []),
              Let
                ( "f",
                  TClosure,
                  Execute
                    ( Fun
                        ( [ ("secret", TString) ],
                          TString,
                          Write
                            ( Open
                                (EOpenable
                                   (Socket ("8.8.8.8", 1234), [ Readable ])),
                              Den "secret" ),
                          [ Readable ] ),
                      TClosure ),
                  Call (Den "f", [ Den "my_pin" ]) ) ),
          None );
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
          let res, p =
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
            |> Printf.sprintf "\027[0;31mFailed, got: %s\027[0m"
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
