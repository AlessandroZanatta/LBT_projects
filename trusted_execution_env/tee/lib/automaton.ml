open Exception

(* State of the automaton. Could also be modeled using an Option *)
type state = State of string | Failure

(* Possible security events relevant for the automaton *)
type security_event =
  | EOpen of Ast.openable
  | EClose of Ast.openable
  | ERead of Ast.openable
  | EWrite of Ast.openable

(* Security automaton definition *)
type security_automaton = {
  mutable current_state : state;
  transition : state * security_event -> state;
}

(* Initialize a new automaton given its initial state and the transition function *)
let init_automaton initial_state transition =
  { current_state = initial_state; transition }

(* Transition of the security automaton *)
let ( ==> ) (automaton : security_automaton ref) event =
  match !automaton.transition (!automaton.current_state, event) with
  | State new_state -> !automaton.current_state <- State new_state
  | Failure -> runtime_error "Violated security policy!"
