(* State of the automaton. Could also be modeled using an Option *)
type state = State of string | Failure

(* Possible security events relevant for the automaton *)
type security_event =
  | Open of Ast.openable
  | Close of Ast.openable
  | Read of Ast.openable
  | Write of Ast.openable

(* Security automaton definition *)
type security_automaton = {
  mutable current_state : state;
  transition : state * security_event -> state;
}

(* Initialize a new automaton given its initial state and the transition function *)
let init_automaton initial_state transition =
  { current_state = initial_state; transition }

(* Transition of the security automaton *)
let transition event (automaton : security_automaton ref) =
  match !automaton.transition (!automaton.current_state, event) with
  | State new_state -> !automaton.current_state <- State new_state
  | Failure -> failwith "*** Violated security policy ***"
