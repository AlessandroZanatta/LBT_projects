open Ast
open Env

(* 
 * Policy used by the eval. The active policy is enforced (e.g. in an execute).
 * The sandbox policy is the policy that we want to enforce when executing mobile code.
 *)
type policy = {
  active : Automata.security_automata ref option;
  sandbox : Automata.security_automata ref;
}

let do_binop = function
  | Sum, Int n1, Int n2 -> Int (n1 + n2)
  | Times, Int n1, Int n2 -> Int (n1 * n2)
  | Minus, Int n1, Int n2 -> Int (n1 - n2)
  | Equal, Int n1, Int n2 -> Bool (n1 = n2)
  | Lesser, Int n1, Int n2 -> Bool (n1 < n2)
  | Greater, Int n1, Int n2 -> Bool (n1 > n2)
  | _ -> failwith "Invalid expression"

let bind_function_arguments params args args_val env =
  Utils.combine3 params
    (List.init (List.length args) (fun _ -> Ast.Private))
    args_val
  |> List.fold_left bind env

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
  | Let (id, visibility, e1, e2) ->
      let v1 = eval env sec_aut e1 in
      eval (Env.bind env (id, visibility, v1)) sec_aut e2
  | Op (op, e1, e2) ->
      let e1 = eval env sec_aut e1 in
      let e2 = eval env sec_aut e2 in
      do_binop (op, e1, e2)
  | Call (f, args) -> (
      match eval env sec_aut f with
      | Closure (params, body) ->
          if List.length args <> List.length params then
            failwith "Mismatched number of arguments passed to function call"
          else
            let args_val = List.map (eval env sec_aut) args in
            eval (bind_function_arguments params args args_val env) sec_aut body
      | _ -> failwith "Call first argument is not a closure")
  | Execute code -> (
      match sec_aut.active with
      (* There is no active policy, therefore we execute the code with the policy used for mobile code *)
      | None ->
          eval { env with priv = [] }
            { sec_aut with active = Some sec_aut.sandbox }
            code
      (* There is already an active policy (i.e. the mobile code called other mobile code). We want to keep enforcing the same policy continuing from the current state *)
      | Some _ -> eval { env with priv = [] } sec_aut code)
  | Open res -> (
      if Option.is_some sec_aut.active then
        Option.get sec_aut.active |> Automata.transition (Automata.Open res);
      (* Do open *)
      match res with
      | File f -> OFile f
      | Socket (addr, port) -> OSocket (addr, port))
  | Close res ->
      if Option.is_some sec_aut.active then
        Option.get sec_aut.active |> Automata.transition (Automata.Close res);
      (* Do close *)
      Bool true
  | Read res ->
      if Option.is_some sec_aut.active then
        Option.get sec_aut.active |> Automata.transition (Automata.Read res);
      (* Do read *)
      String (Utils.sprintf_openable "Read from" res)
  | Write res ->
      if Option.is_some sec_aut.active then
        Option.get sec_aut.active |> Automata.transition (Automata.Write res);
      (* Do write *)
      String (Utils.sprintf_openable "Wrote to" res)
