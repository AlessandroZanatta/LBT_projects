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

(* Permissions available for runtime values *)
type permission = Readable | Sendable

(* To handle permissions better, we make use of sets *)
module PermSet = Set.Make (struct
  type t = permission

  let compare = compare
end)

(* Openable resources *)
type openable = File of string | Socket of string * int

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

(* Supported expressions *)
type exp =
  | EInt of int * permission list
  | EBool of bool * permission list
  | EString of string * permission list
  | EOpenable of openable * permission list
  | Fun of (ide * exp typ) list * exp typ * exp * permission list
  | Den of ide
  | Op of ops * exp * exp
  | If of exp * exp * exp
  | Let of ide * exp typ * exp * exp
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
 * Note: we also assume that open, close, read and write are perfect: they always work, even
 * if the file/socket was not opened (simplifying assumption)
 *)

(* Secure runtime values are actually used in the interpreter. These carry around their permissions *)
type 'v secure_runtime_value = 'v * PermSet.t

(* Runtime values are ints, booleans, closures, open files and sockets, and strings *)
type runtime_value =
  | Int of int
  | Bool of bool
  | String of string
  | Closure of
      (ide * exp typ) list * exp * runtime_value secure_runtime_value Env.env
  | OFile of string
  | OSocket of string * int
