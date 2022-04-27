open Automaton

(* Utility to print an opeanble object in a pretty way. *)
let sprintf_openable text openable =
  (match openable with
  | Ast.File f -> Printf.sprintf " '%s'" f
  | Ast.Socket (addr, port) -> Printf.sprintf " '%s' %d" addr port)
  |> Printf.sprintf "%s%s" text

(* Clone of an automata. As we work with refs, we need use fresh automata
 * when running examples/tests. *)
let copy_automaton automaton =
  { transition = automaton.transition; current_state = automaton.current_state }
