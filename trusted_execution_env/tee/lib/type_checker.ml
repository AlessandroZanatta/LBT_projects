open Ast
open Exception
open Env

(* [typeof env e] is the type of [e] in the environment [env].
   That is, it is the [t] such that [env |- e : t].
   Raises: [TypecheckError] if no such [t] exists. *)
let rec typeof env = function
  | EInt _ -> TInt
  | EBool _ -> TBool
  | EString _ -> TString
  | EOpenable _ -> TOpenable
  | Fun (args, ret_type, body, _) -> TClosure' (args, ret_type, body, env)
  | Den x -> lookup_tc env x
  | If (e1, e2, e3) -> typeof_if env e1 e2 e3
  | Let (id, t, e1, e2) -> typeof_let env id t e1 e2
  | Op (op, e1, e2) -> typeof_binop env op e1 e2
  | Call (f, args) -> typeof_call env f args
  | Execute (e, t) -> typeof_execute env e t
  | Open res -> typeof_open env res
  | Close res -> typeof_close env res
  | Read res -> typeof_read env res
  | Write (res, e) -> typeof_write env res e

(* [typeof_if env e1 e2 e3] checks that:
   - [env |- e1 : t1] and t1 = TBool
   - [env |- e2 : t2] and [env |- e3 : t3] and t2 = t3
   Then, it returns t2 (or, equivalently, t3).
   Raises: [TypecheckError] if above typechecking rule fail. *)
and typeof_if env e1 e2 e3 =
  if typeof env e1 = TBool then
    let t2 = typeof env e2 in
    let t3 = typeof env e3 in
    if t2 = t3 then t2 else typecheck_error "If branches types must match"
  else typecheck_error "If guard must evaluate to a Bool"

(* [typeof_let env id t e1 e2] checks that:
   - [env |- e1 : t'] and t = t' or (t = TClosure and t' = TClosure')
   and returns t2, where t2 is the result of [env,(id, t) |- e2 : t1].
   Raises: [TypecheckError] if the type of e1 mismatches with [t] *)
and typeof_let env id t e1 e2 =
  let t' = typeof env e1 in
  if t = t' || check_closure t t' then typeof (bind_tc env (id, t')) e2
  else typecheck_error "Let assignment typecheck failed"

and check_closure t t' =
  match t' with TClosure' _ -> t = TClosure | _ -> false

(* [typeof_binop env op e1 e2] checks that the operation and the
   operands types match.
   Raises: [TypecheckError] if the operations and operands mismatch. *)
and typeof_binop env op e1 e2 =
  match (op, typeof env e1, typeof env e2) with
  | Sum, TInt, TInt -> TInt
  | Times, TInt, TInt -> TInt
  | Minus, TInt, TInt -> TInt
  | Div, TInt, TInt -> TInt
  | Mod, TInt, TInt -> TInt
  | Equal, TBool, TBool -> TBool
  | Lesser, TBool, TBool -> TBool
  | Greater, TBool, TBool -> TBool
  | Lesseq, TBool, TBool -> TBool
  | Greateq, TBool, TBool -> TBool
  | Diff, TBool, TBool -> TBool
  | Or, TBool, TBool -> TBool
  | And, TBool, TBool -> TBool
  | _ -> typecheck_error "Operator and operand typecheck failed"

(* [typeof_call env f args] checks that:
   - [env |- f : t] and t = TClosure'(...)
   - parameters' types match arguments' types
   - return type matches declared type
   Raises: [TypecheckError] if above conditions are not respected. *)
and typeof_call env f args =
  match typeof env f with
  | TClosure' (params, ret_type, body, fun_env) ->
      let ides = List.split params |> fst in
      let types = List.split params |> snd in
      let args_types = List.map (typeof env) args in
      if types = args_types then
        let fun_env' =
          List.combine ides types |> List.fold_left bind_tc fun_env
        in
        if typeof fun_env' body = ret_type then ret_type
        else typecheck_error "Function return value mismatch"
      else
        typecheck_error
          "Function parameters definition and passed arguments type mismatch"
  | _ -> typecheck_error "Call first argument must be of type Closure"

(* [typeof_execute env e t] checks that the return type of [e] matches [t].
   Raises: [TypecheckError] if typecheck fails. *)
and typeof_execute env e t =
  let t' = typeof env e in
  if t = t' || check_closure t t' then t'
  else typecheck_error "Execution of mobile code type mismatch"

(* [typeof_open env res] checks that [env |- res : t] and t = TOpenable.
   Raises: [TypecheckError] if above typechecking fails. *)
and typeof_open env res =
  if typeof env res = TOpenable then TOpenable
  else typecheck_error "Can only open a file or a socket"

(* [typeof_close env res] checks that [env |- res : t] and t = TOpenable.
   Raises: [TypecheckError] if above typechecking fails. *)
and typeof_close env res =
  if typeof env res = TOpenable then TBool
  else typecheck_error "Can only close a file or a socket"

(* [typeof_read env res] checks that [env |- res : t] and t = TOpenable.
   Raises: [TypecheckError] if above typechecking fails. *)
and typeof_read env res =
  if typeof env res = TOpenable then TString
  else typecheck_error "Can only read from a file or a socket"

(* [typeof_write env res e] checks that:
   - [env |- res : t] and t = TOpenable
   - [env |- e : t'] and t' = TString
   Raises: [TypecheckError] if above typechecking fails. *)
and typeof_write env res e =
  if typeof env res = TOpenable && typeof env e = TString then TString
  else typecheck_error "Can only write strings to a file or a socket "

(* [typecheck e] is [e] if [e] typechecks, that is, if it exists a type [t]
   such that [{} |- e : t].
   Raises: [TypecheckError] if [e] does not typecheck. *)
let typecheck e =
  ignore (typeof [] e);
  e
