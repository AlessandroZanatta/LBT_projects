(* Identifiers type declaration *)
type ide = string

(* Type declaration for operations on integers *)
type ops = Sum | Times | Minus | Equal | Lesser | Greater

(* Supported expressions *)
type exp =
  | Eint of int
  | Ebool of bool
  | Den of ide
  | Op of ops * exp * exp
  | If of exp * exp * exp
  | Let of ide * exp * exp
  | Fun of ide list * exp
  | Call of exp * exp list
  | Execute of exp

(* Runtime values are ints, booleans, and closures *)
type value = Int of int | Bool of bool | Closure of (ide list * exp)
