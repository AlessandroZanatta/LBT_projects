open Automaton
open Exception

(* [combine3 l1 l2 l3] transform the lists into a list of triples:
   combine3 [a1; ...; an] [b1; ...; bn] [c1; ...; cn] is [(a1,b1,c1); ...; (an,bn,cn)].
   Raises: [RuntimeError] when lists lenght mismatch. *)
let rec combine3 l1 l2 l3 =
  match (l1, l2, l3) with
  | [], [], [] -> []
  | h1 :: t1, h2 :: t2, h3 :: t3 -> (h1, h2, h3) :: combine3 t1 t2 t3
  | _ -> runtime_error "Invalid combine3, mismatch of list lengths"

(* Utility to print an opeanble object in a pretty way. *)
let sprintf_openable text openable =
  (match openable with
  | Ast.File (f, access_mode) ->
      (match access_mode with
      | Ast.O_RDONLY -> "O_RDONLY"
      | Ast.O_WRONLY -> "O_WRONLY"
      | Ast.O_RDWR -> "O_RDWR")
      |> Printf.sprintf " '%s' (mode: %s)" f
  | Ast.Socket (addr, port) -> Printf.sprintf " '%s' %d" addr port)
  |> Printf.sprintf "%s%s" text

(* Clone of an automata. As we work with refs, we need use fresh automata
   when running examples/tests. *)
let copy_automaton automaton =
  { transition = automaton.transition; current_state = automaton.current_state }
