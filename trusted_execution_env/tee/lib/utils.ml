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

(* Utility to pretty print an opeanble . *)
let sprintf_openable text openable =
  (match openable with
  | Ast.File f -> Printf.sprintf "'%s'" f
  | Ast.Socket (addr, port) -> Printf.sprintf " '%s' %d" addr port)
  |> Printf.sprintf "%s%s" text

(* Clone of an automata. As we work with refs, we need use fresh automata
   when running examples/tests. *)
let copy_automaton automaton =
  { transition = automaton.transition; current_state = automaton.current_state }

(* Utility to pretty print an permissions set. *)
let sprintf_permissions perms =
  let rec aux acc = function
    | [] -> acc ^ "]"
    | h :: t ->
        let s =
          match h with Ast.Readable -> "Readable" | Ast.Sendable -> "Sendable"
        in
        aux (acc ^ s ^ " ") t
  in
  aux "[ " (Ast.PermSet.elements perms)
