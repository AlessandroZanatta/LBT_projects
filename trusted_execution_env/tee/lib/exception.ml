exception TypecheckError of string
exception RuntimeError of string

let runtime_error x = raise (RuntimeError x)
let typecheck_error x = raise (TypecheckError x)