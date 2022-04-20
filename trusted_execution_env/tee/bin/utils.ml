let rec combine3 l1 l2 l3 =
  match (l1, l2, l3) with
  | [], [], [] -> []
  | h1 :: t1, h2 :: t2, h3 :: t3 -> (h1, h2, h3) :: combine3 t1 t2 t3
  | _ -> failwith "Invalid combine3, mismatch of list lengths"

let sprintf_openable text openable =
  text
  |> Printf.sprintf "%s %s"
       (match openable with
       | Ast.File f -> Printf.sprintf " '%s'" f
       | Ast.Socket (addr, port) -> Printf.sprintf " '%s' %d" addr port)
