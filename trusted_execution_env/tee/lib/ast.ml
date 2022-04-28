(* Identifiers type declaration *)
type ide = string

(* Typechecking types definition *)
type 'e typ =
  | TBool
  | TInt
  | TString
  (* We use a dummy type for closures type labels, and one for actual internal logic *)
  | TClosure
  | TClosure' of (ide * 'e typ) list * 'e typ * 'e * 'e typ Env.static_env
  | TOpenable

(* Type declaration for operations on integers *)
type ops =
  | Sum
  | Times
  | Minus
  | Div
  | Mod
  | Equal
  | Lesser
  | Greater
  | Lesseq
  | Greateq
  | Diff
  | Or
  | And

(* Openable resources *)
type access_mode = O_RDONLY | O_WRONLY | O_RDWR
type openable = File of string * access_mode | Socket of string * int

(* Supported expressions *)
type exp =
  | EInt of int
  | EBool of bool
  | EString of string
  | EOpenable of openable
  | Den of ide * Env.visibility
  | Op of ops * exp * exp
  | If of exp * exp * exp
  | Let of ide * exp typ * Env.visibility * exp * exp
  | Fun of (ide * exp typ) list * exp typ * exp
  | Call of exp * exp list
  | Execute of exp * exp typ
  | Open of exp
  | Close of exp
  | Read of exp
  | Write of exp * exp
(*
 * Note: we do not support Send or Recv directly.
 * Instead, we consider different types of objects on which Read and Write can be
 * called on, including Sockets. Hence, a write on a socket corresponds to a Send operation.   
 *)

(* Runtime values are ints, booleans, closures, open files and sockets, and strings *)
type value =
  | Int of int
  | Bool of bool
  | String of string
  | Closure of ((ide * exp typ) list * exp * value Env.t)
  | OFile of string * access_mode
  | OSocket of string * int
