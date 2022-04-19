type 'v env = (string * 'v) list

let rec lookup (env : 'v env) x =
  match env with
  | [] ->
      Printf.sprintf "Binding not found in the environment: %s" x |> failwith
  | (ide, value) :: r -> if x = ide then value else lookup r x

let bind (env : 'v env) (x, v) = (x, v) :: env