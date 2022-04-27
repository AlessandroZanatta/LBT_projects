open Exception

(* Environment contains variables with a visibility specifier *)
type 'v t = (string * 'v) list

(* Static environment used for typechecking *)
type 'v static_env = (string * 'v) list

(* 
 * Lookup searches in the private enviorment first. This is to cope with 
 * the fact that functions arguments are assumed to be private, but this would
 * erranously return the incorrect binding if a public variable with the same name existed
 * This is needed as we miss clearly separated scopes in our language.
 *)
let rec lookup (env : 'v t) x =
  match env with
  | [] -> Printf.sprintf "Unbound variable: %s" x |> runtime_error
  | (ide, value) :: t -> if x = ide then value else lookup t x

let empty_env = []

(* Binds the variable in the correct list based on its visibility *)
let bind (env : 'v t) (id, value) = (id, value) :: env