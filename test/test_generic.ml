open Ego.Generic

let sexp =
  (module struct
    type t = Sexplib.Sexp.t
    let pp = Sexplib.Sexp.pp_hum
    let equal = Sexplib.Sexp.equal
  end : Alcotest.TESTABLE with type t = Sexplib.Sexp.t)

module L = struct

  type 'a shape = Add of 'a * 'a | Sub of 'a * 'a | Mul of 'a * 'a
                | Div of 'a * 'a | Var of string | Const of int [@@deriving ord, show]

  type op = AddOp | SubOp | MulOp | DivOp | VarOp of string | ConstOp of int [@@deriving eq]

  type t = Mk of t shape  [@@unboxed]

  let rec of_sexp = function [@warning "-8"]
    | Sexplib0.Sexp.Atom s ->
      begin match int_of_string_opt s with
      | Some n -> Mk (Const n)
      | None -> Mk (Var s)
      end
    | List [Atom ("*" | " * "); l; r] -> Mk (Mul (of_sexp l, of_sexp r))
    | List [Atom "-"; l; r] -> Mk (Sub (of_sexp l, of_sexp r))
    | List [Atom "+"; l; r] -> Mk (Add (of_sexp l, of_sexp r))
    | List [Atom "/"; l; r] -> Mk (Div (of_sexp l, of_sexp r))

  let rec to_sexp = function
    | Mk (Add (l, r)) -> Sexplib0.Sexp.List [Atom "+"; to_sexp l; to_sexp r]
    | Mk (Sub (l, r)) -> List [Atom "-"; to_sexp l; to_sexp r]
    | Mk (Mul (l, r)) -> List [Atom "*"; to_sexp l; to_sexp r]      
    | Mk (Div (l, r)) -> List [Atom "/"; to_sexp l; to_sexp r]      
    | Mk (Var s) -> Atom s
    | Mk (Const n) -> Atom (Int.to_string n)

  let op = function
    | Add _ -> AddOp
    | Sub _ -> SubOp
    | Mul _ -> MulOp
    | Div _ -> DivOp
    | Var s -> VarOp s
    | Const i -> ConstOp i

  let op_of_string = function
    | "+" -> AddOp
    | "-" -> SubOp
    | ("*" | " * ") -> MulOp
    | "/" -> DivOp
    | s -> match int_of_string_opt s with
      | None -> VarOp s
      | Some n -> ConstOp n

  let children = function
    | Add (l,r) | Sub (l,r) | Mul (l,r) | Div (l,r) -> [l;r]
    | Var _ | Const _ -> []

  let map_children term f = match term with
    | Add (l,r) -> Add (f l, f r)
    | Sub (l,r) -> Sub (f l, f r)
    | Mul (l,r) -> Mul (f l, f r)
    | Div (l,r) -> Div (f l, f r)
    | Var s -> Var s | Const i -> Const i

  let make op ls =
    match[@warning "-8"] op,ls with
    | AddOp, [l;r] -> Add (l,r)
    | SubOp, [l;r] -> Sub (l,r)
    | MulOp, [l;r] -> Mul (l,r)
    | DivOp, [l;r] -> Div (l,r)
    | VarOp s, [] -> Var s
    | ConstOp i, [] -> Const i

end

module C = struct
  type t = float [@@deriving ord]
  let cost f : Ego.Id.t L.shape -> t = function
    | L.Add (l, r) -> f l +. f r +. 1.0
    | L.Sub (l, r) -> f l +. f r +. 1.5
    | L.Mul (l, r) -> f l +. f r +. 2.0
    | L.Div (l, r) -> f l +. f r +. 2.0
    | L.Var _ -> 1.0
    | L.Const _ -> 1.0
end

module A = struct type t = unit type data = int option [@@deriving eq, show] let default = None end
module MA (S : GRAPH_API
           with type 'p t = (Ego.Id.t L.shape, A.t, A.data, 'p) egraph
            and type 'a shape := 'a L.shape
            and type analysis := A.t
            and type data := A.data
            and type node := L.t)  = struct
  type 'p t = (Ego.Id.t L.shape, A.t, A.data, 'p) egraph

  let eval : A.data L.shape -> A.data =
    function
    | L.Add (Some l, Some r)  -> Some (l + r)
    | L.Sub (Some l, Some r) -> Some (l - r)
    | L.Mul (Some l, Some r) -> Some (l * r)
    | L.Div (Some l, Some r) -> if r <> 0 then Some (l / r) else None
    | L.Const n -> Some n
    | _ -> None

  let make : ro t -> Ego.Id.t L.shape -> A.data =
    fun graph term ->
    eval (L.map_children term (S.get_data graph))

  let merge : A.t -> A.data -> A.data -> A.data * (bool * bool) =
    fun () l r ->  match l,r with
      | Some l, Some r -> assert (l = r); Some l, (false, false)
      | Some l, None -> Some l, (false, true)
      | None, Some r -> Some r, (true, false)
      | _ -> None, (false, false)

  let modify : 'a t -> Ego.Id.t -> unit =
    fun graph cls ->
    match S.get_data (S.freeze graph) cls with
    | None -> ()
    | Some n ->
      let nw_cls = S.add_node graph (L.Mk (Const n)) in
      S.merge graph nw_cls cls

end

module EGraph = Make (L) (A) (MA)
module Extractor = MakeExtractor (L) (C)


let documentation_example () =
  let graph = EGraph.init () in
  let expr = EGraph.add_node graph (L.of_sexp [%s (2 * 2)]) in
  let result = Extractor.extract graph expr in
  Alcotest.(check sexp)
    "extracted expression has been simplified"
    [%s 4]
    (L.to_sexp result)

let simple_constant_folding () =
  let graph = EGraph.init () in
  let expr = EGraph.add_node graph (L.of_sexp [%s (2 + (1 + (3 - 2)))]) in
  let result = Extractor.extract graph expr in
  Alcotest.(check sexp)
    "extracted expression has been simplified"
    [%s 4]
    (L.to_sexp result)


let multiple_terms_constant_folding () =
  let graph = EGraph.init () in
  let expr1 = EGraph.add_node graph (L.of_sexp [%s (3 * (2 - (10 / 5)))]) in
  let expr2 = EGraph.add_node graph (L.of_sexp [%s (3 - 3)]) in
  Alcotest.(check sexp)
    "first extracted expression has been simplified"
    [%s 0]
    (L.to_sexp (Extractor.extract graph expr1));
  Alcotest.(check sexp)
    "second extracted expression has been simplified"
    [%s 0]
    (L.to_sexp (Extractor.extract graph expr2))

let multiple_terms_variable_constant_folding () =
  let graph = EGraph.init () in
  let expr1 = EGraph.add_node graph (L.of_sexp [%s ((2 * x) + (3 * (2 - (10 / 5))))]) in
  let expr2 = EGraph.add_node graph (L.of_sexp [%s ((3 - 3) + (2 * x))]) in
  Alcotest.(check sexp)
    "first extracted expression has been simplified"
    [%s ("+" (( * ) 2 x) 0)]
    (L.to_sexp (Extractor.extract graph expr1));
  Alcotest.(check sexp)
    "second extracted expression has been simplified"
    [%s ("+" 0 (( * ) 2 x))]
    (L.to_sexp (Extractor.extract graph expr2))

let syntactic_rewrite () =
  let graph = EGraph.init () in
  let rewrite =
    EGraph.Rule.make_constant
      ~from:(Query.of_sexp L.op_of_string [%s (2 * "?x")])
      ~into:(Query.of_sexp L.op_of_string [%s ("?x" + "?x")]) in
  let expr = EGraph.add_node graph (L.of_sexp [%s (1 + (3 / (2 * a)))]) in
  Alcotest.(check bool)
    "rewrites reached saturation" true
    @@ EGraph.run_until_saturation graph [rewrite];
  Alcotest.(check sexp)
    "first extracted expression has been simplified"
    [%s ("+" 1 ((/) 3 ("+" a a)))]
    (L.to_sexp (Extractor.extract graph expr))

let conditional_rewrite () =
  let graph = EGraph.init () in
  let rewrite =
    EGraph.Rule.make_conditional
      ~from:(Query.of_sexp L.op_of_string [%s ("?x" / "?x")])
      ~into:(Query.of_sexp L.op_of_string [%s 1])
      ~cond:(fun graph _root env ->
        let x = StringMap.find "x" env in
        match EGraph.get_data graph x with
        | None | Some 0 -> false
        | _ -> true (* only safe to do this rewrite if x isn't 0  *)) in
  let expr_valid = EGraph.add_node graph (L.of_sexp [%s (10 / 10)]) in
  let expr_invalid = EGraph.add_node graph (L.of_sexp [%s (0 / 0)]) in
  let expr_invalid_compl = EGraph.add_node graph (L.of_sexp [%s ((4 * 3 - 6 * 2) / (2 * 3 - 3 * 2))]) in
  let expr_valid_compl = EGraph.add_node graph (L.of_sexp [%s (((4 * 3 - 6 * 2) + 1) / ((2 * 3 - 3 * 2) + 1))]) in
  let expr_x_x = EGraph.add_node graph (L.of_sexp [%s (x / x)]) in
  Alcotest.(check bool)
    "rewrites reached saturation" true
    @@ EGraph.run_until_saturation graph [rewrite];
  Alcotest.(check sexp)
    "basic expression has been simplified"
    [%s 1]
    (L.to_sexp (Extractor.extract graph expr_valid));
  Alcotest.(check sexp)
    "invalid expression has not been simplified"
    [%s ("/" 0 0)]
    (L.to_sexp (Extractor.extract graph expr_invalid));
  Alcotest.(check sexp)
    "complex invalid expression has not been simplified beyond minimal"
    [%s ("/" 0 0)]
    (L.to_sexp (Extractor.extract graph expr_invalid_compl));
  Alcotest.(check sexp)
    "complex valid expression has been simplified"
    [%s 1]
    (L.to_sexp (Extractor.extract graph expr_valid_compl));
  Alcotest.(check sexp)
    "expression of variables has not been simplified"
    [%s ("/" x x)]
    (L.to_sexp (Extractor.extract graph expr_x_x))


let () =
  Alcotest.run "generic" [
    ("documentation", [
       "example given in documentation works as written", `Quick, documentation_example;
       "simple constant folding", `Quick, simple_constant_folding;
       "multiple terms constant folding", `Quick, multiple_terms_constant_folding;
       "multiple terms with variable constant folding", `Quick, multiple_terms_variable_constant_folding;
       "syntactic rewriting", `Quick, syntactic_rewrite;
       "conditional rewriting", `Quick, conditional_rewrite;
     ])
  ]
