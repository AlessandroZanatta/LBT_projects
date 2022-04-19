(* State of the automata *)
type state = State of string | Failure
type openable = File of string | Socket of string * int

(* Possible security events relevant for the automata *)
type security_event =
  | Open of openable
  | Close of openable
  | Read of openable
  | Write of openable
  | Send of string
  | Recv of string

(* Security automata definition *)
type security_automata = {
  current_state : state;
  transition : state * security_event -> state;
}

(* Initialize a new automata given its initial state and the transition function *)
let init_automata initial_state transition =
  { current_state = initial_state; transition }

(* Transition of the security automata *)
let transition event automata =
  match automata.transition (automata.current_state, event) with
  | State new_state -> { automata with current_state = State new_state }
  | Failure -> failwith "*** Violated security policy ***"
