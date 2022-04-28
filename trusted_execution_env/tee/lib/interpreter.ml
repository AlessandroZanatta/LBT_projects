open Ast
open Env
open Automaton
open Exception

(* 
 * Policy used by the eval. The active policy is enforced (e.g. in an execute).
 * The sandbox policy is the policy that we want to enforce when executing mobile code ("dormient" policy).
 *)
type policy = {
  active : security_automaton ref option;
  sandbox : security_automaton;
}

(* [check_policy sec_aut sec_event] feeds [sec_aut] the new event, hence causing a transition in [sec_aut].
   Raises: [RuntimeError] if [sec_aut] ends up in the Failure state. *)
let check_policy sec_aut sec_event =
  if Option.is_some sec_aut.active then Option.get sec_aut.active ==> sec_event

(* [eval env sec_aut expr] evalues [expr] in the given enviroment [env],
   possibly enforcing [sec_aut] in the case of security relevant actions.
   Raises: [RuntimeError] if dividing by zero, or when a type mismatch occurs
   (i.e. the typechecker returned a false positive). *)
let rec eval env sec_aut expr =
  match expr with
  | EInt n -> Int n
  | EBool b -> Bool b
  | EString x -> String x
  | EOpenable x -> eval_eopenable x
  | Fun (args, _, body) -> Closure (args, body, env)
  | Den (x, visibility) -> Env.lookup env x visibility
  | If (e1, e2, e3) -> eval_if env sec_aut e1 e2 e3
  | Let (id, _, vis, e1, e2) -> eval_let env sec_aut id vis e1 e2
  | Op (op, e1, e2) -> eval_op env sec_aut op e1 e2
  | Call (f, args) -> eval_call env sec_aut f args
  | Execute (e, _) -> eval_execute env sec_aut e
  | Open res -> eval_open env sec_aut res
  | Close res -> eval_close env sec_aut res
  | Read res -> eval_read env sec_aut res
  | Write (res, e) -> eval_write env sec_aut res e

(* [eval_eopenable res] returns an openable based on [res] value *)
and eval_eopenable = function
  | File (f, am) -> OFile (f, am)
  | Socket (ip, port) -> OSocket (ip, port)

(* [eval_if env sec_aut e1 e2 e3] Evaluates [e1].
   If true, returns the evalution of [e2], else the evaluation of [e3]
   Raises: [RuntimeError] if [e1] does not evalute to a Bool *)
and eval_if env sec_aut e1 e2 e3 =
  match eval env sec_aut e1 with
  | Bool true -> eval env sec_aut e2
  | Bool false -> eval env sec_aut e3
  | _ -> runtime_error "If guard must evaluate to a boolean"

(* [eval_let env sec_aut id vis e1 e2]
   Evaluates [e1] and binds it to [id].
   Then, executes [e2] in the new environment. *)
and eval_let env sec_aut id vis e1 e2 =
  let v1 = eval env sec_aut e1 in
  eval (Env.bind env (id, v1, vis)) sec_aut e2

(* [eval_op env sec_aut op e1 e2]
   Evaluates [e1] and [e2] and applies the given binary operation.
   Raises: [RuntimeError] if the operation is invalid or we have divided by zero. *)
and eval_op env sec_aut op e1 e2 =
  let e1 = eval env sec_aut e1 in
  let e2 = eval env sec_aut e2 in
  match (op, e1, e2) with
  | Sum, Int n1, Int n2 -> Int (n1 + n2)
  | Times, Int n1, Int n2 -> Int (n1 * n2)
  | Minus, Int n1, Int n2 -> Int (n1 - n2)
  | Div, Int n1, Int n2 ->
      if n2 = 0 then runtime_error "Division by zero";
      Int (n1 / n2)
  | Mod, Int n1, Int n2 -> Int (n1 mod n2)
  | Equal, Int n1, Int n2 -> Bool (n1 = n2)
  | Lesser, Int n1, Int n2 -> Bool (n1 < n2)
  | Greater, Int n1, Int n2 -> Bool (n1 > n2)
  | Lesseq, Int n1, Int n2 -> Bool (n1 <= n2)
  | Greateq, Int n1, Int n2 -> Bool (n1 >= n2)
  | Diff, Int n1, Int n2 -> Bool (n1 <> n2)
  | Or, Bool b1, Bool b2 -> Bool (b1 || b2)
  | And, Bool b1, Bool b2 -> Bool (b1 && b2)
  | _ -> runtime_error "Invalid expression"

(* [eval_call env sec_aut f args] evaluates the body of [f], if [f] in [env] was found of type Closure.
   To evaluate [f] body, [args] are evaluated. Then, the body of [f] is evaluated in the saved
   environment of [f], augmented with the binding of (params * [args]).
   Raises: [RuntimeError] if [f] was not a closure, or the number of arguments of [f] was mismatched. *)
and eval_call env sec_aut f args =
  match eval env sec_aut f with
  | Closure (params, body, fun_env) ->
      (* Check that the provided number of parameters matches the function declaration *)
      if List.length args <> List.length params then
        runtime_error "Mismatched number of arguments passed to function call"
      else
        let args_val = List.map (eval env sec_aut) args in
        eval
          (Utils.combine3
             (List.split params |> fst)
             args_val
             (List.init (List.length args) (fun _ -> Env.Private))
          |> List.fold_left bind fun_env)
          sec_aut body
  | _ -> runtime_error "Call first argument is not a closure"

(* [eval_execute env sec_aut code] evaluates [code] using the public variables only in [env]
   and enforcing the policy defined in [sec_aut].
   Nested executes are supported and secured by using the same policy with the same current state. *)
and eval_execute env sec_aut code =
  let execute_env = retain_public env in
  match sec_aut.active with
  (* 
   * There is no active policy, therefore we execute the code 
   * with the sandbox policy as active 
   *)
  | None ->
      eval execute_env
        {
          sec_aut with
          (* 
           * We copy the automata, as different execute in the "top-level"
           * code are enforced separately
           *)
          active = Some (Utils.copy_automaton sec_aut.sandbox |> ref);
        }
        code
  (*
   * There is already an active policy (i.e. the mobile code called other mobile code). 
   * We want to keep enforcing the same policy continuing from the current state.
   * This is needed to prevent bypasses of the policy by calling nested mobile codes.
   *)
  | Some _ -> eval execute_env sec_aut code

(* Open, close, read and write operations are considered security events.
 * As such, if a policy is being enforced, we first check that the transition
 * of the automata leads to a valid state.
 * Note: these operations are just stubs: we assume that open, close, read and write are perfect: they 
 * always work, even if the file/socket was not opened (simplifying assumption). *)

(* [eval_open env sec_aut res] checks that the evalution of [res] is either an OFile or an OSocket.
   If so, checks that the policy enforced by [sec_aut] is still valid (if any), and returns the opened resource.
   Raises: [RuntimeError] in the case of type mismatch, or when the action violates the policy of [sec_aut]. *)
and eval_open env sec_aut res =
  let value = eval env sec_aut res in
  let res =
    match value with
    | OFile (f, am) -> File (f, am)
    | OSocket (ip, port) -> Socket (ip, port)
    | _ -> runtime_error "Can only open files or sockets"
  in
  Automaton.EOpen res |> check_policy sec_aut;
  (* Do open *)
  value

(* [eval_close env sec_aut res] checks that the evalution of [res] is either an OFile or an OSocket.
   If so, checks that the policy enforced by [sec_aut] is still valid (if any), and returns Bool true.
   Raises: [RuntimeError] in the case of type mismatch, or when the action violates the policy of [sec_aut]. *)
and eval_close env sec_aut res =
  let res =
    match eval env sec_aut res with
    | OFile (f, am) -> File (f, am)
    | OSocket (ip, port) -> Socket (ip, port)
    | _ -> runtime_error "Can only close files or sockets"
  in
  Automaton.EClose res |> check_policy sec_aut;
  Bool true

(* [eval_read env sec_aut res] checks that the evalution of [res] is either an OFile with valid permissions or an OSocket.
   If so, checks that the policy enforced by [sec_aut] is still valid (if any), and returns a generic string.
   Raises: [RuntimeError] in the case of type mismatch, or if permissions are incorrect, or when the
   action violates the policy of [sec_aut]. *)
and eval_read env sec_aut res =
  let res =
    match eval env sec_aut res with
    | OFile (f, O_RDONLY) -> File (f, O_RDONLY)
    | OFile (f, O_RDWR) -> File (f, O_RDWR)
    | OSocket (ip, port) -> Socket (ip, port)
    | _ ->
        runtime_error
          "Can only read from files or sockets or incorrect permissions"
  in
  Automaton.ERead res |> check_policy sec_aut;
  String (Utils.sprintf_openable "Read from" res)

(* [eval_writeenv sec_aut res e] checks that the evalution of [res] is either an OFile with valid permissions
   or an OSocket, and that [e] evaluates to a String.
   If so, checks that the policy enforced by [sec_aut] is still valid (if any), and returns a generic string.
   Raises: [RuntimeError] in the case of type mismatch, or if permissions are incorrect, or when the
   action violates the policy of [sec_aut]. *)
and eval_write env sec_aut res e =
  let res =
    match eval env sec_aut res with
    | OFile (f, O_WRONLY) -> File (f, O_WRONLY)
    | OFile (f, O_RDWR) -> File (f, O_RDWR)
    | OSocket (ip, port) -> Socket (ip, port)
    | _ ->
        runtime_error
          "Can only write to files or sockets or incorrect permissions"
  in
  let content =
    match eval env sec_aut e with
    | String x -> x
    | _ -> runtime_error "Can only write strings to files or sockets"
  in
  Automaton.EWrite res |> check_policy sec_aut;
  (* Do write *)
  String (Utils.sprintf_openable (Printf.sprintf "Wrote '%s' to" content) res)