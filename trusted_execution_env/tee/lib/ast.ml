(* Identifiers type declaration *)
type ide = string

(* Type declaration for operations on integers *)
type ops = Sum | Times | Minus | Div | Mod | Equal | Lesser | Greater | Lesseq | Greateq | Diff | Or | And 

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
(*
 * Note: we do not support Send or Recv directly.
 * Instead, we consider different types of objects on which Read and Write can be
 * called on, including Sockets. Hence, a write on a socket corresponds to a Send operation.   
 *)

(* Runtime values are ints, booleans, closures, open files and sockets, and strings *)
type value =
  | Int of int
  | Bool of bool
  | Closure of (ide list * exp)
  | OFile of string
  | OSocket of string * int
  | String of string
