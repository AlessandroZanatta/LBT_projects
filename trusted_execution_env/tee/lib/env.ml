open Exception

(* We keep two environments: a local one for the current "scope"
   (i.e. the current level of nesting of execute calls), and a
   global one for other "scopes" (levels of nesting). *)
type scope = Local | Global

(* Environment contains variables with a set of permissions *)
type 'v env = { local : (string * 'v) list; global : (string * 'v) list }

(* Empty environment for the interpreter *)
let empty_env = { local = []; global = [] }

(* [lookup env ide] searches for a binding with identifier [ide] in [env].
   Raises: [RuntimeError] if the binding does not exist. *)
let lookup env ide =
  match List.assoc_opt ide env.local with
  | Some v -> (v, Local)
  | None -> (
      match List.assoc_opt ide env.global with
      | Some v -> (v, Global)
      | None -> Printf.sprintf "Unbound variable: %s" ide |> runtime_error)

(* [bind env (id, value)] binds the given tuple in [env].
   Binding always happens in the [env.local] scope. *)
let bind env (id, x) = { env with local = (id, x) :: env.local }

(* [push_scope env] pushes the current local scope to the global one by
   prepending the existing one and empties the local scope. *)
let push_scope env = { local = []; global = env.local @ env.global }

(***********************************************************************************************)

(* Static environment used for typechecking *)
type 'v static_env = (string * 'v) list

(* [bind_tc env (id, x)] binds [(id, x)] in [env]. *)
let bind_tc env (id, x) = (id, x) :: env

(* [lookup_tc env ide] searches for a binding with identifier [ide] in [env].
   Raises: [TypecheckError] if the binding does not exist. *)
let lookup_tc env ide =
  match List.assoc_opt ide env with
  | Some v -> v
  | None -> Printf.sprintf "Unbound variable: %s" ide |> typecheck_error
