(* State of the automata *)
type state = State of string | Failure

(* Possible security events relevant for the automata *)
type security_event =
  | Open of Ast.openable
  | Close of Ast.openable
  | Read of Ast.openable
  | Write of Ast.openable

(* Security automata definition *)
type security_automata = {
  mutable current_state : state;
  transition : state * security_event -> state;
}

(* Initialize a new automata given its initial state and the transition function *)
let init_automata initial_state transition =
  ref { current_state = initial_state; transition }

(* Transition of the security automata *)
let transition event (automata : security_automata ref) =
  match !automata.transition (!automata.current_state, event) with
  | State new_state -> !automata.current_state <- State new_state
  | Failure -> failwith "*** Violated security policy ***"
