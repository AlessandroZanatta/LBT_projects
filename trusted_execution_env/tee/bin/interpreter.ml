open Ast
open Env
open Automaton

(* 
 * Policy used by the eval. The active policy is enforced (e.g. in an execute).
 * The sandbox policy is the policy that we want to enforce when executing mobile code.
 *)
type policy = {
  active : security_automaton ref option;
  sandbox : security_automaton ref;
}

(* Executes the given binary operation *)
let do_binop = function
  | Sum, Int n1, Int n2 -> Int (n1 + n2)
  | Times, Int n1, Int n2 -> Int (n1 * n2)
  | Minus, Int n1, Int n2 -> Int (n1 - n2)
  | Div, Int n1, Int n2 -> Int (n1 / n2)
  | Mod, Int n1, Int n2 -> Int (n1 mod n2)
  | Equal, Int n1, Int n2 -> Bool (n1 = n2)
  | Lesser, Int n1, Int n2 -> Bool (n1 < n2)
  | Greater, Int n1, Int n2 -> Bool (n1 > n2)
  | Lesseq, Int n1, Int n2 -> Bool (n1 <= n2)
  | Greateq, Int n1, Int n2 -> Bool (n1 >= n2)
  | Diff, Int n1, Int n2 -> Bool (n1 <> n2)
  | Or, Bool b1, Bool b2 -> Bool (b1 || b2)
  | And, Bool b1, Bool b2 -> Bool (b1 && b2)
  | _ -> failwith "Invalid expression"

(* Binds the function arguments values to the corresponding parameters *)
let bind_function_arguments params args_val env =
  Utils.combine3 params
    (List.init (List.length params) (fun _ -> Ast.Private))
    args_val
  |> List.fold_left bind env

(* 
 * Checks if a policy has to be enforced, and applies 
 * the transition with the given security event.
 * Returns nothing, side-effect only
 *)
let check_policy sec_aut sec_event =
  if Option.is_some sec_aut.active then Option.get sec_aut.active ==> sec_event

let rec eval env (sec_aut : policy) expr =
  match expr with
  | Eint n -> Int n
  | Ebool b -> Bool b
  | Fun (args, body) -> Closure (args, body)
  | Den x -> Env.lookup env x
  | If (e1, e2, e3) -> (
      match eval env sec_aut e1 with
      | Bool true -> eval env sec_aut e2
      | Bool false -> eval env sec_aut e3
      | _ -> failwith "If guard must evaluate to a boolean")
  (* 
   * Declared identifiers always have a visibility associated to them.
   * In the concrete syntax, we would also assume the Private visibility by default.
   *)
  | Let (id, visibility, e1, e2) ->
      let v1 = eval env sec_aut e1 in
      eval (Env.bind env (id, visibility, v1)) sec_aut e2
  | Op (op, e1, e2) ->
      let e1 = eval env sec_aut e1 in
      let e2 = eval env sec_aut e2 in
      do_binop (op, e1, e2)
  (* Functions support multiple arguments *)
  | Call (f, args) -> (
      match eval env sec_aut f with
      | Closure (params, body) ->
          (* Check that the provided number of parameters matches the function declaration *)
          if List.length args <> List.length params then
            failwith "Mismatched number of arguments passed to function call"
          else
            let args_val = List.map (eval env sec_aut) args in
            eval (bind_function_arguments params args_val env) sec_aut body
      | _ -> failwith "Call first argument is not a closure")
  (*
   * Execute is used to run mobile code.
   * The public enviroment is kept when evaluating the code, while the
   * private enviroment is emptied.
   *)
  | Execute code -> (
      match sec_aut.active with
      (* 
       * There is no active policy, therefore we execute the code 
       * with the sandbox policy as active 
       *)
      | None ->
          eval { env with priv = [] }
            { sec_aut with active = Some sec_aut.sandbox }
            code
      (* 
       * There is already an active policy (i.e. the mobile code called other mobile code). 
       * We want to keep enforcing the same policy continuing from the current state.
       * This is needed to prevent bypasses of the policy by calling nested mobile codes.
       *)
      | Some _ -> eval { env with priv = [] } sec_aut code)
  (* 
   * Open, close, read and write operations are considered security events.
   * As such, if a policy is being enforced, we first check that the transition 
   * of the automata leads to a valid state.
   * Note: these operations are just stubs.
   *)
  | Open res -> (
      Automaton.EOpen res |> check_policy sec_aut;
      (* Do open *)
      match res with
      | File f -> OFile f
      | Socket (addr, port) -> OSocket (addr, port))
  | Close res ->
      Automaton.EClose res |> check_policy sec_aut;
      (* Do close *)
      Bool true
  | Read res ->
      Automaton.ERead res |> check_policy sec_aut;
      (* Do read *)
      String (Utils.sprintf_openable "Read from" res)
  | Write res ->
      Automaton.EWrite res |> check_policy sec_aut;
      (* Do write *)
      String (Utils.sprintf_openable "Wrote to" res)
