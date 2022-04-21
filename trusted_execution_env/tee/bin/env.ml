(* Environment contains variables with a visibility specifier *)
type 'v env = { pub : (string * 'v) list; priv : (string * 'v) list }

(* 
 * Lookup searches in the private enviorment first. This is to cope with 
 * the fact that functions arguments are assumed to be private, but this would
 * erranously return the incorrect binding if a public variable with the same name existed
 * This is needed as we miss clearly separated scopes in our language.
 *)
let lookup (env : 'v env) x =
  let rec aux = function
    | [] ->
        Printf.sprintf "Binding not found in the environment: %s" x |> failwith
    | (ide, value) :: r -> if x = ide then value else aux r
  in
  env.priv @ env.pub |> aux

let empty_env = { pub = []; priv = [] }

(* Binds the variable in the correct list based on its visibility *)
let bind (env : 'v env) (id, visibility, value) =
  match visibility with
  | Ast.Public -> { env with pub = (id, value) :: env.pub }
  | Ast.Private -> { env with priv = (id, value) :: env.priv }
