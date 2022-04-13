open Ast
open Env

let rec eval env expr =
  match expr with
  | Eint n -> Int n
  | Ebool b -> Bool b
  | Fun (x, body) -> Closure (x, body, env)
  | Den x -> Env.lookup env x
  | If (e1, e2, e3) -> (
      match eval env e1 with
      | Bool true -> eval env e2
      | Bool false -> eval env e3
      | _ -> failwith "If guard must evaluate to a boolean")
  | Let (id, e1, e2) ->
      let v1 = eval env e1 in
      eval (Env.bind env (id, v1)) e2
  | Op (op, e1, e2) -> (
      let e1 = eval env e1 in
      let e2 = eval env e2 in
      match (op, e1, e2) with
      | Sum, Int n1, Int n2 -> Int (n1 + n2)
      | Times, Int n1, Int n2 -> Int (n1 * n2)
      | Minus, Int n1, Int n2 -> Int (n1 - n2)
      | Equal, Int n1, Int n2 -> Bool (n1 = n2)
      | Lesser, Int n1, Int n2 -> Bool (n1 < n2)
      | Greater, Int n1, Int n2 -> Bool (n1 > n2)
      | _ -> failwith "Invalid expression")
  | Call (f, arg) -> (
      match eval env f with
      | Closure (x, body, env) ->
          let x_val = eval env arg in
          eval (bind env (x, x_val)) body
      | _ -> failwith "Call first argument is not a closure")
  | Execute _ -> failwith "Not implemented!"
