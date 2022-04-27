open Ast
open Env
open Automaton
open Exception

(* 
 * Policy used by the eval. The active policy is enforced (e.g. in an execute).
 * The sandbox policy is the policy that we want to enforce when executing mobile code.
 *)
type policy = {
  active : security_automaton ref option;
  sandbox : security_automaton;
}

(* 
 * Checks if a policy has to be enforced, and applies 
 * the transition with the given security event.
 * Returns nothing, side-effect only
 *)
let check_policy sec_aut sec_event =
  if Option.is_some sec_aut.active then Option.get sec_aut.active ==> sec_event

let rec eval env sec_aut expr =
  match expr with
  | EInt n -> Int n
  | EBool b -> Bool b
  | EString x -> String x
  | Fun (args, _, body) -> Closure (args, body, env)
  | Den (x, visibility) -> Env.lookup env x visibility
  | If (e1, e2, e3) -> eval_if env sec_aut e1 e2 e3
  | Let (id, _, vis, e1, e2) -> eval_let env sec_aut id vis e1 e2
  | Op (op, e1, e2) -> eval_op env sec_aut op e1 e2
  | Call (f, args) -> eval_call env sec_aut f args
  | Execute (e, _) -> eval_execute env sec_aut e
  | Open res -> eval_open sec_aut res
  | Close res -> eval_close sec_aut res
  | Read res -> eval_read sec_aut res
  | Write res -> eval_write sec_aut res

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

(* Functions support multiple arguments *)
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

(*
 * Execute is used to run mobile code.
 * The public enviroment is kept when evaluating the code, while the
 * private enviroment is emptied.
 *)
and eval_execute env sec_aut code =
  match sec_aut.active with
  (* 
   * There is no active policy, therefore we execute the code 
   * with the sandbox policy as active 
   *)
  | None ->
      eval (retain_public env)
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
  | Some _ -> eval (retain_public env) sec_aut code

(*
 * Open, close, read and write operations are considered security events.
 * As such, if a policy is being enforced, we first check that the transition
 * of the automata leads to a valid state.
 * Note: these operations are just stubs.
 *)
and eval_open sec_aut res =
  Automaton.EOpen res |> check_policy sec_aut;
  (* Do open *)
  match res with
  | File f -> OFile f
  | Socket (addr, port) -> OSocket (addr, port)

and eval_close sec_aut res =
  Automaton.EClose res |> check_policy sec_aut;
  (* Do close *)
  Bool true

and eval_read sec_aut res =
  Automaton.ERead res |> check_policy sec_aut;
  (* Do read *)
  String (Utils.sprintf_openable "Read from" res)

and eval_write sec_aut res =
  Automaton.EWrite res |> check_policy sec_aut;
  (* Do write *)
  String (Utils.sprintf_openable "Wrote to" res)
