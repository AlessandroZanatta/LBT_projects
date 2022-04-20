(* Identifiers type declaration *)
type ide = string

(* Type declaration for operations on integers *)
type ops = Sum | Times | Minus | Equal | Lesser | Greater

(* Openable resources *)
type openable = File of string | Socket of string * int

(* Visibility of an identifier or function *)
type visibility = Public | Private

(* Supported expressions *)
type exp =
  | Eint of int
  | Ebool of bool
  | Den of ide
  | Op of ops * exp * exp
  | If of exp * exp * exp
  | Let of ide * visibility * exp * exp
  | Fun of ide list * exp
  | Call of exp * exp list
  | Execute of exp
  | Open of openable
  | Close of openable
  | Read of openable
  | Write of openable

(* Runtime values are ints, booleans, and closures *)
type value =
  | Int of int
  | Bool of bool
  | Closure of (ide list * exp)
  | OFile of string
  | OSocket of string * int
  | String of string
