open Core_kernel
  type t = Statement.t list

  open Statement

  let rec to_ocaml = function
    | [] ->
        ""
    | Assign (name, e) :: ss ->
        sprintf "let %s = %s in\n%s" name (Expr.to_ocaml e) (to_ocaml ss)
    | [Return e] ->
        Expr.to_ocaml e
    | Proc_call (name, args) :: ss ->
        sprintf "%s; %s" (Expr.to_ocaml (Fun_call (name, args))) (to_ocaml ss)
    | Return _ :: _ :: _ ->
        failwith "Return should be the last statement"

