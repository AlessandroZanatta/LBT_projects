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
   possibly enforcing [sec_aut.active] in the case of security relevant actions.
   Raises: [RuntimeError] if dividing by zero, or when a type mismatch occurs
   (i.e. the typechecker returned a false positive). *)
let rec eval env sec_aut expr =
  match expr with
  | EInt (n, perms) -> (Int n, PermSet.of_list perms)
  | EBool (b, perms) -> (Bool b, PermSet.of_list perms)
  | EString (x, perms) -> (String x, PermSet.of_list perms)
  | EOpenable (x, perms) -> eval_eopenable (PermSet.of_list perms) x
  | Fun (args, _, body, perms) ->
      (Closure (args, body, env), PermSet.of_list perms)
  | Den x -> eval_den env sec_aut x
  | If (e1, e2, e3) -> eval_if env sec_aut e1 e2 e3
  | Let (id, _, e1, e2) -> eval_let env sec_aut id e1 e2
  | Op (op, e1, e2) -> eval_op env sec_aut op e1 e2
  | Call (f, args) -> eval_call env sec_aut f args
  | Execute (e, _) -> eval_execute env sec_aut e
  | Open res -> eval_open env sec_aut res
  | Close res -> eval_close env sec_aut res
  | Read res -> eval_read env sec_aut res
  | Write (res, e) -> eval_write env sec_aut res e

(* [eval_eopenable res] returns an openable based on [res] value *)
and eval_eopenable perms = function
  | File f -> (OFile f, perms)
  | Socket (ip, port) -> (OSocket (ip, port), perms)

(* [eval_den env sec_aut x] searches for a binding for [x] in [env].
   If the binding was a local one, it is returned. If it was global,
   it is checked that the Readable permission was set before returning it. *)
and eval_den env sec_aut x =
  match Env.lookup env x with
  | v, Local -> v
  | v, Global -> check_permissions sec_aut [ Readable ] v

(* [eval_if env sec_aut e1 e2 e3] Evaluates [e1].
   If true, returns the evalution of [e2], else the evaluation of [e3]
   Raises: [RuntimeError] if [e1] does not evalute to a Bool *)
and eval_if env sec_aut e1 e2 e3 =
  match eval env sec_aut e1 with
  | Bool true, _ -> eval env sec_aut e2
  | Bool false, _ -> eval env sec_aut e3
  | _ -> runtime_error "If guard must evaluate to a boolean"

(* [eval_let env sec_aut id vis e1 e2]
   Evaluates [e1] and binds it to [id].
   Then, executes [e2] in the new environment. *)
and eval_let env sec_aut id e1 e2 =
  let v1 = eval env sec_aut e1 in
  eval (Env.bind env (id, v1)) sec_aut e2

(* [eval_op env sec_aut op e1 e2]
   Evaluates [e1] and [e2] and applies the given binary operation.
   Raises: [RuntimeError] if the operation is invalid, or we have
   divided by zero, or the permissions where insufficient. *)
and eval_op env sec_aut op e1 e2 =
  let v1, p1 = eval env sec_aut e1 |> check_permissions sec_aut [ Readable ] in
  let v2, p2 = eval env sec_aut e2 |> check_permissions sec_aut [ Readable ] in

  (* As we "combine" two values, we also need to "combine" the two permissions sets.
     To do so, we take the intersection of the permissions sets, so that the lowest
     level of permissions is kept, and no new permission is given. *)
  let new_perms = PermSet.inter p1 p2 in
  match (op, v1, v2) with
  | Sum, Int n1, Int n2 -> (Int (n1 + n2), new_perms)
  | Times, Int n1, Int n2 -> (Int (n1 * n2), new_perms)
  | Minus, Int n1, Int n2 -> (Int (n1 - n2), new_perms)
  | Div, Int n1, Int n2 ->
      if n2 = 0 then runtime_error "Division by zero";
      (Int (n1 / n2), new_perms)
  | Mod, Int n1, Int n2 -> (Int (n1 mod n2), new_perms)
  | Equal, Int n1, Int n2 -> (Bool (n1 = n2), new_perms)
  | Lesser, Int n1, Int n2 -> (Bool (n1 < n2), new_perms)
  | Greater, Int n1, Int n2 -> (Bool (n1 > n2), new_perms)
  | Lesseq, Int n1, Int n2 -> (Bool (n1 <= n2), new_perms)
  | Greateq, Int n1, Int n2 -> (Bool (n1 >= n2), new_perms)
  | Diff, Int n1, Int n2 -> (Bool (n1 <> n2), new_perms)
  | Or, Bool b1, Bool b2 -> (Bool (b1 || b2), new_perms)
  | And, Bool b1, Bool b2 -> (Bool (b1 && b2), new_perms)
  | _ -> runtime_error "Invalid expression"

(* [eval_call env sec_aut f args] evaluates the body of [f], if [f] in [env] was found of type Closure.
   To evaluate [f] body, [args] are evaluated. Then, the body of [f] is evaluated in the saved
   environment of [f], augmented with the binding of (params * [args]).
   Notice that we do not need to check the permissions of [f], as it either was an anonymous function,
   or a previous Den x definition evaled to [f], and checked the permission.
   Raises: [RuntimeError] if [f] was not a closure, or the number of arguments of [f] was mismatched. *)
and eval_call env sec_aut f args =
  match eval env sec_aut f with
  | Closure (params, body, fun_env), _ ->
      (* Check that the provided number of parameters matches the function declaration *)
      if List.length args <> List.length params then
        runtime_error "Mismatched number of arguments passed to function call"
      else
        let args_val = List.map (eval env sec_aut) args in
        eval
          (List.combine (List.split params |> fst) args_val
          |> List.fold_left bind fun_env)
          sec_aut body
  | _ -> runtime_error "Call first argument is not a closure"

(* [eval_execute env sec_aut code] evaluates [code]. The environment is "pushed", so
   that the local scope is emptied, and the global scope is prepended by the current [env.local].
   Nested executes are supported by the above mechanism and secured by using the same
   policy with the same current state. *)
and eval_execute env sec_aut e =
  let execute_env = Env.push_scope env in
  match sec_aut.active with
  (* 
   * There is no active policy, therefore we execute the code 
   * with the sandbox policy as active 
   *)
  | None ->
      eval execute_env
        {
          sec_aut with
          (* We copy the automata, as different execute in the "top-level"
           * code are enforced separately *)
          active = Some (Utils.copy_automaton sec_aut.sandbox |> ref);
        }
        e
  (*
   * There is already an active policy (i.e. the mobile code called other mobile code). 
   * We want to keep enforcing the same policy continuing from the current state.
   * This is needed to prevent bypasses of the policy by calling nested mobile codes.
   *)
  | Some _ -> eval execute_env sec_aut e

(* Open, close, read and write operations are considered security events.
 * As such, if a policy is being enforced, we first check that the transition
 * of the automata leads to a valid state.
 * Note: these operations are just stubs: we assume that open, close, read and write are perfect: they 
 * always work, even if the file/socket was not opened (simplifying assumption). *)

(* [eval_open env sec_aut res] checks that the evalution of [res] is either an OFile or an OSocket.
   If so, checks that the policy enforced by [sec_aut] is still valid (if any), and returns the opened resource.
   Raises: [RuntimeError] in the case of type mismatch, or when the action violates the policy of [sec_aut]. *)
and eval_open env sec_aut res =
  let v, p = eval env sec_aut res in
  let res =
    match v with
    | OFile f -> File f
    | OSocket (ip, port) -> Socket (ip, port)
    | _ -> runtime_error "Can only open files or sockets"
  in
  Automaton.EOpen res |> check_policy sec_aut;
  (* Do open *)
  (v, p)

(* [eval_close env sec_aut res] checks that the evalution of [res] is either an OFile or an OSocket.
   If so, checks that the policy enforced by [sec_aut] is still valid (if any), and returns Bool true.
   Raises: [RuntimeError] in the case of type mismatch, or when the action violates the policy of [sec_aut]. *)
and eval_close env sec_aut res =
  let v, p = eval env sec_aut res in
  let res =
    match v with
    | OFile f -> File f
    | OSocket (ip, port) -> Socket (ip, port)
    | _ -> runtime_error "Can only close files or sockets"
  in
  Automaton.EClose res |> check_policy sec_aut;
  (Bool true, p)

(* [eval_read env sec_aut res] checks that the evalution of [res] is either an OFile with valid permissions or an OSocket.
   If so, checks that the policy enforced by [sec_aut] is still valid (if any), and returns a generic string.
   Raises: [RuntimeError] in the case of type mismatch, or if permissions are incorrect, or when the
   action violates the policy of [sec_aut]. *)
and eval_read env sec_aut res =
  let v, p = eval env sec_aut res in
  let res =
    match v with
    | OFile f -> File f
    | OSocket (ip, port) -> Socket (ip, port)
    | _ -> runtime_error "Can only read from files or sockets"
  in
  Automaton.ERead res |> check_policy sec_aut;
  (String (Utils.sprintf_openable "Read from " res), p)

(* [eval_write env sec_aut res e] checks that the evalution of [res] is either an OFile with valid permissions
   or an OSocket, and that [e] evaluates to a String.
   If so, checks that the policy enforced by [sec_aut] is still valid (if any), and returns a generic string.
   Raises: [RuntimeError] in the case of type mismatch, or if permissions are incorrect, or when the
   action violates the policy of [sec_aut]. *)
and eval_write env sec_aut res e =
  let v1, p1 = eval env sec_aut res in
  let v2, p2 =
    eval env sec_aut e |> check_permissions sec_aut [ Readable; Sendable ]
  in
  let res =
    match v1 with
    | OFile f -> File f
    | OSocket (ip, port) -> Socket (ip, port)
    | _ -> runtime_error "Can only write to files or sockets"
  in
  let content =
    match v2 with
    | String x -> x
    | _ -> runtime_error "Can only write strings to files or sockets"
  in
  Automaton.EWrite res |> check_policy sec_aut;
  (* Do write *)
  ( String (Utils.sprintf_openable (Printf.sprintf "Wrote '%s' to" content) res),
    PermSet.empty )

(* [check_permissions sec_aut required_perms v] if we are in an execute,
   we check that [required_perms] is a subset of the permissions defined on [v].
   Then, we simply return [v]. This is done solely to pipeline operations nicely.
   Raises: [RuntimeError] if the above does not hold. *)
and check_permissions sec_aut required_perms v =
  if Option.is_none sec_aut.active then v
  else
    let res, p = v in
    if PermSet.subset (PermSet.of_list required_perms) p then v
    else
      Printf.sprintf "Invalid access. Expected permissions: %s. Got: %s"
        (Utils.sprintf_permissions (PermSet.of_list required_perms))
        (Utils.sprintf_permissions p)
      |> runtime_error
