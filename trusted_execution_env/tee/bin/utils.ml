open Automaton

(* Same as List.combine, but for three lists *)
let rec combine3 l1 l2 l3 =
  match (l1, l2, l3) with
  | [], [], [] -> []
  | h1 :: t1, h2 :: t2, h3 :: t3 -> (h1, h2, h3) :: combine3 t1 t2 t3
  | _ -> failwith "Invalid combine3, mismatch of list lengths"

(* Utility to print an opeanble object in a pretty way *)
let sprintf_openable text openable =
  (match openable with
  | Ast.File f -> Printf.sprintf " '%s'" f
  | Ast.Socket (addr, port) -> Printf.sprintf " '%s' %d" addr port)
  |> Printf.sprintf "%s%s" text

(* 
 * Clone of an automata. As we work with refs, we need use fresh automata
 * when running examples/tests
 *)
let copy_automaton automaton =
  ref
    {
      transition = automaton.transition;
      current_state = automaton.current_state;
    }
