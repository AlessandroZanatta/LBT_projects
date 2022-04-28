open Exception

type visibility = Private | Public

(* Environment contains variables with a visibility specifier *)
type 'v t = (string * ('v * visibility)) list

(* Static environment used for typechecking *)
type 'v static_env = (string * ('v * visibility)) list

(* [lookup env ide vis] searches for a binding with identifier [ide]
   and visibility [vis] in [env].
   Raises: [RuntimeError] if a binding with the correct visibility is not found. *)
let rec lookup env ide vis =
  match List.assoc_opt ide env with
  | Some (v, vis') ->
      if vis = vis' then v
      else Printf.sprintf "Unbound variable: %s" ide |> runtime_error
  | None -> Printf.sprintf "Unbound variable: %s" ide |> runtime_error

(* [retain_public env] removes any non-public binding in [env] *)
let retain_public env = List.filter (fun (_, (_, vis)) -> vis = Public) env

(* [bind env (id, value, vis)] binds the given triple or, if present, substitutes it, in [env] *)
let bind env (id, value, vis) =
  if List.mem_assoc id env then (id, (value, vis)) :: List.remove_assoc id env
  else (id, (value, vis)) :: env
