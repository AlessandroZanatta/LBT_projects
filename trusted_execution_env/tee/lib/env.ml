open Exception

type visibility = Private | Public

(* Environment contains variables with a visibility specifier *)
type 'v t = (string * ('v * visibility)) list

(* Static environment used for typechecking *)
type 'v static_env = (string * ('v * visibility)) list

(* 
 * Lookup searches in the private enviorment first. This is to cope with 
 * the fact that functions arguments are assumed to be private, but this would
 * erranously return the incorrect binding if a public variable with the same name existed
 * This is needed as we miss clearly separated scopes in our language.
 *)
let rec lookup env ide vis =
  match List.assoc_opt ide env with
  | Some (v, vis') ->
      if vis = vis' then v
      else Printf.sprintf "Unbound variable: %s" ide |> runtime_error
  | None -> Printf.sprintf "Unbound variable: %s" ide |> runtime_error

let retain_public env = List.filter (fun (_, (_, vis)) -> vis = Public) env
let empty_env = []

(* Binds the variable in the correct list based on its visibility *)
let bind env (id, value, vis) =
  if List.mem_assoc id env then (id, (value, vis)) :: List.remove_assoc id env
  else (id, (value, vis)) :: env
