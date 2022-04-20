(* Environment contains variables with a visibility specifier *)
type 'v env = { pub : (string * 'v) list; priv : (string * 'v) list }

let lookup (env : 'v env) x =
  let rec aux = function
    | [] ->
        Printf.sprintf "Binding not found in the environment: %s" x |> failwith
    | (ide, value) :: r -> if x = ide then value else aux r
  in
  env.priv @ env.pub |> aux

let empty_env = { pub = []; priv = [] }

let bind (env : 'v env) (id, visibility, value) =
  match visibility with
  | Ast.Public -> { env with pub = (id, value) :: env.pub }
  | Ast.Private -> { env with priv = (id, value) :: env.priv }
