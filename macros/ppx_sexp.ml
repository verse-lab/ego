open Ppxlib

let name = "s"

let build_atom ~loc v =
  let str = Ast_helper.(Exp.constant ~loc (Pconst_string (v, loc, None))) in
  [%expr Sexplib0.Sexp.Atom [%e str ]]

let build_list ~loc ls =
  let rec build_ls = function
    | [] -> [%expr []]
    | h :: t -> [%expr [%e h] :: [%e build_ls t]] in
  [%expr Sexplib0.Sexp.List [%e (build_ls ls)]]

let rec convert ~loc expr =
  match expr with
  | { pexp_desc=Pexp_ident { txt=Lident "()"; _ }; pexp_loc=loc; _ } ->
    build_list ~loc []
  | { pexp_desc=Pexp_ident { txt=Lident txt; _ }; pexp_loc=loc; _ }
    when txt.[0] = '(' && txt.[String.length txt - 1] =  ')' ->
    build_atom ~loc (String.sub txt 1 (String.length txt - 2))
  | { pexp_desc=Pexp_ident { txt=Lident txt; _ }; pexp_loc=loc; _ } ->
    build_atom ~loc txt
  | { pexp_desc=Pexp_constant const; pexp_loc=loc; _ } ->
    let const = match const with
      | Pconst_integer (txt, _) -> txt
      | Pconst_char cr -> String.make 1 cr
      | Pconst_string (txt, _, _) -> txt
      | Pconst_float (txt, _) -> txt in
    build_atom ~loc const
  | { pexp_desc=Pexp_apply (expr, args); pexp_loc=loc; _ }
    when List.for_all (function (Nolabel, _) -> true | _ -> false) args ->
    let h = convert ~loc:expr.pexp_loc expr in
    let t = List.map (fun (_, expr) -> convert ~loc:expr.pexp_loc expr) args in
    build_list ~loc (h :: t)
  | [%expr [%e? x] x ] -> x
  | e ->
    let exp = Pprintast.expression Format.str_formatter e; Format.flush_str_formatter () in
    Location.raise_errorf ~loc "use of unsupported syntactic construct %s" exp

let expand ~loc ~path:_ expr = convert ~loc expr

let ext =
  Extension.declare name Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    expand

let () = Driver.register_transformation name ~extensions:[ext]
