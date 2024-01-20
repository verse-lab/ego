open Ego.Generic
let sexp =
  (module struct
    type t = Sexplib.Sexp.t
    let pp = Sexplib.Sexp.pp_hum
    let equal = Sexplib.Sexp.equal
  end : Alcotest.TESTABLE with type t = Sexplib.Sexp.t)

module Symbol : sig
  type t
  val compare : t -> t -> int
  val equal: t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val intern: string -> t
  val to_string: t -> string
end = struct
  type t = int
  let equal = Int.equal
  let compare = Int.compare
  let intern, to_string =
    let tbl = ref @@ StringMap.empty in
    let buf = Array.make 100 "" in
    let limit = ref 0 in
    let intern s =
      match (StringMap.find_opt s !tbl) with
        Some n -> n | None -> let ind = !limit in buf.(ind) <- s; incr limit;
        tbl := (StringMap.add s ind !tbl); ind in
    let to_string n = buf.(n) in
    intern, to_string

  let pp fmt s = Format.pp_print_string fmt (to_string s)
end

module L = struct
  type 'a shape =
    | Diff of 'a * 'a
    | Integral of 'a * 'a
    | Add of 'a * 'a
    | Sub of 'a * 'a
    | Mul of 'a * 'a
    | Div of 'a * 'a
    | Pow of 'a * 'a
    | Ln of 'a
    | Sqrt of 'a
    | Sin of 'a
    | Cos of 'a
    | Constant of float
    | Symbol of Symbol.t
  [@@deriving ord, show]

  (* let float_equal f1 f2 =
   *   print_endline @@ Printf.sprintf "comparing %f eq with %f = %b" f1 f2 (Float.equal f1 f2);
   *   Float.equal f1 f2 *)

  type op =
    | DiffOp | IntegralOp | AddOp | SubOp | MulOp | DivOp | PowOp | LnOp | SqrtOp
    | SinOp | CosOp | ConstantOp of float | SymbolOp of Symbol.t [@@deriving eq]

  type t = Mk of t shape [@@unboxed]

  let rec of_sexp : Sexplib0.Sexp.t -> _ = function [@warning "-8"]
    | Atom s ->
      begin match float_of_string_opt s with
      | Some n -> Mk (Constant n)
      | None ->
        match int_of_string_opt s with
        | Some n -> Mk (Constant (Float.of_int n))
        | None -> Mk (Symbol (Symbol.intern s))
      end
    | List [Atom "d"; l; r] -> Mk (Diff (of_sexp l, of_sexp r))
    | List [Atom "i"; l; r] -> Mk (Integral (of_sexp l, of_sexp r))
    | List [Atom ("*" | " * "); l; r] -> Mk (Mul (of_sexp l, of_sexp r))
    | List [Atom "-"; l; r] -> Mk (Sub (of_sexp l, of_sexp r))
    | List [Atom "+"; l; r] -> Mk (Add (of_sexp l, of_sexp r))
    | List [Atom "/"; l; r] -> Mk (Div (of_sexp l, of_sexp r))
    | List [Atom "pow"; l; r] -> Mk (Pow (of_sexp l, of_sexp r))
    | List [Atom "ln"; l] -> Mk (Ln (of_sexp l))
    | List [Atom "sqrt"; l] -> Mk (Sqrt (of_sexp l))
    | List [Atom "sin"; l] -> Mk (Sin (of_sexp l))
    | List [Atom "cos"; l] -> Mk (Cos (of_sexp l))

  let rec to_sexp : t -> Sexplib0.Sexp.t = function
    | Mk (Diff (l, r)) -> List [Atom "d"; to_sexp l; to_sexp r]
    | Mk (Integral (l, r)) -> List [Atom "i"; to_sexp l; to_sexp r]
    | Mk (Add (l, r)) -> List [Atom "+"; to_sexp l; to_sexp r]
    | Mk (Sub (l, r)) -> List [Atom "-"; to_sexp l; to_sexp r]
    | Mk (Mul (l, r)) -> List [Atom "*"; to_sexp l; to_sexp r]
    | Mk (Div (l, r)) -> List [Atom "/"; to_sexp l; to_sexp r]
    | Mk (Pow (l, r)) -> List [Atom "pow"; to_sexp l; to_sexp r]
    | Mk (Ln l) -> List [Atom "ln"; to_sexp l]
    | Mk (Sqrt l) -> List [Atom "sqrt"; to_sexp l]
    | Mk (Sin l) -> List [Atom "sin"; to_sexp l]
    | Mk (Cos l) -> List [Atom "cos"; to_sexp l]
    | Mk (Constant l) -> Atom (Float.to_string l)
    | Mk (Symbol l) -> Atom (Symbol.to_string l)

  let op = function
    | Diff (_, _) -> DiffOp | Integral (_, _) -> IntegralOp
    | Add (_, _) -> AddOp | Sub (_, _) -> SubOp
    | Mul (_, _) -> MulOp | Div (_, _) -> DivOp
    | Pow (_, _) -> PowOp | Ln _ -> LnOp
    | Sqrt _ -> SqrtOp | Sin _ -> SinOp | Cos _ -> CosOp
    | Constant c -> ConstantOp c | Symbol s -> SymbolOp s

  let op_of_string : string -> op = function [@warning "-8"]
    | "d" -> DiffOp | "i" -> IntegralOp | ("*" | " * ") -> MulOp
    | "-" -> SubOp | "+" -> AddOp | "/" -> DivOp
    | "pow" -> PowOp | "ln" -> LnOp | "sqrt" -> SqrtOp
    | "sin" -> SinOp | "cos" -> CosOp | s ->
      begin match float_of_string_opt s with
      | Some n -> (ConstantOp n)
      | None ->
        match int_of_string_opt s with
        | Some n -> ConstantOp (Float.of_int n)
        | None -> SymbolOp (Symbol.intern s)
      end

  let children = function
    | Diff (l, r) | Integral (l, r) | Add (l, r) | Sub (l, r) | Mul (l, r) 
    | Div (l, r) | Pow (l, r) -> [l;r]
    | Ln l | Sqrt l | Sin l | Cos l -> [l]
    | Constant _ | Symbol _ -> []

  let map_children term f = match term with
    | Diff (l, r) -> Diff (f l, f r) | Integral (l, r) -> Integral (f l, f r)
    | Add (l, r) -> Add (f l, f r) | Sub (l, r) -> Sub (f l, f r)
    | Mul (l, r) -> Mul (f l, f r) | Div (l, r) -> Div (f l, f r)
    | Pow (l, r) -> Pow (f l, f r) | Ln l -> Ln (f l)
    | Sqrt l -> Sqrt (f l) | Sin l -> Sin (f l) | Cos l -> Cos (f l)
    | Constant c -> Constant c | Symbol s -> Symbol s

  let make op ls =
    match[@warning "-8"] op,ls with
    | DiffOp, [l;r] -> Diff (l, r) | IntegralOp, [l;r] -> Integral (l, r)
    | AddOp, [l;r] -> Add (l, r) | SubOp, [l;r] -> Sub (l, r)
    | MulOp, [l;r] -> Mul (l, r) | DivOp, [l;r] -> Div (l, r)
    | PowOp, [l;r] -> Pow (l, r) | LnOp, [l] -> Ln l
    | SqrtOp, [l] -> Sqrt l | SinOp, [l] -> Sin l | CosOp, [l] -> Cos l
    | ConstantOp c, [] -> Constant c | SymbolOp s, [] -> Symbol s

end 

module C = struct
  type t = int [@@deriving ord]
  let cost f : Ego.Id.t L.shape -> t =
    fun term ->
    let base_cost = match term with Diff _  | Integral _ -> 100 | Sub _ -> 20 | _ -> 1 in
    L.children term |> List.fold_left (fun acc vl -> acc + f vl) base_cost
end

module A = struct type t = unit type data = float option [@@deriving eq, show] let default = None end
module MA (S : GRAPH_API
           with type 'p t = (Ego.Id.t L.shape, A.t, A.data, 'p) egraph
            and type 'a shape := 'a L.shape
            and type analysis := A.t
            and type data := A.data
            and type node := L.t)  = struct
  type 'p t = (Ego.Id.t L.shape, A.t, A.data, 'p) egraph

  let eval : A.data L.shape -> A.data =
    function
    | L.Add (Some l, Some r)  -> Some (l +. r)
    | L.Sub (Some l, Some r) -> Some (l -. r)
    | L.Mul (Some l, Some r) -> Some (l *. r)
    | L.Div (Some l, Some r) -> if Containers.Float.equal_precision ~epsilon:0.01 r 0. then Some (l /. r) else None
    | L.Constant n -> Some n
    | _ -> None

  let make : ro t -> Ego.Id.t L.shape -> A.data =
    fun graph term ->
    eval (L.map_children term (S.get_data graph))

  let merge : A.t -> A.data -> A.data -> A.data * (bool * bool)=
    fun () l r ->  match l,r with
      | Some l, Some r ->
        if Float.equal l r
        then Some l, (false,false)
        else failwith @@ Printf.sprintf "merge failed: float values %f <> %f " l r
      | Some l, _ -> Some l, (false, true)
      | _, Some r -> Some r, (true, false)
      | _ -> None, (false,false)

  let modify : 'a t -> Ego.Id.t -> unit =
    fun graph cls ->
    match S.get_data (S.freeze graph) cls with
    | None -> ()
    | Some n ->
      let nw_cls = S.add_node graph (L.Mk (Constant n)) in
      S.merge graph nw_cls cls

end

module EGraph = Make (L) (A) (MA)
module Extractor = MakeExtractor (L) (C)

let is_const_or_distinct_var v w =
  fun graph _root_id env ->
  let v = StringMap.find v env in
  let w = StringMap.find w env in
  (not @@ EGraph.class_equal (EGraph.freeze graph) v w)
  && ((EGraph.get_data graph v |> Option.is_some) ||
      EGraph.iter_children (EGraph.freeze graph) v |> Iter.exists (function L.Symbol _ -> true | _ -> false))

let is_const v =
  fun graph _root_id env ->
  let v = StringMap.find v env in
  EGraph.get_data graph v |> Option.is_some

let is_sym v =
  fun graph _root_id env ->
  let v = StringMap.find v env in
  EGraph.iter_children (EGraph.freeze graph) v |> Iter.exists (function L.Symbol _ -> true | _ -> false)

let is_not_zero v =
  fun graph _root_id env ->
  let v = StringMap.find v env in
  EGraph.get_data graph v |> function Some 0.0 -> false | _ -> true

let qf = Query.of_sexp L.op_of_string 
let (@->) from into = EGraph.Rule.make_constant ~from:(qf from) ~into:(qf into) 
let rewrite from into ~if_ = EGraph.Rule.make_conditional ~from:(qf from) ~into:(qf into) ~cond:if_ 

let rules =
  let[@warning "-26"] (&&) f1 f2 = fun graph root_id env -> (f1 graph root_id env) && (f2 graph root_id env)  in
  [

    [%s ("?a" + "?b")] @-> [%s ("?b" + "?a")]; (* comm-add *)
    [%s ("?a" * "?b")] @-> [%s ("?b" * "?a")]; (* comm-mul *)
    [%s ("?a" + ("?b" + "?c"))] @-> [%s (("?a" + "?b") + "?c")]; (* assoc add *)
    [%s ("?a" * ("?b" * "?c"))] @-> [%s (("?a" * "?b") * "?c")]; (* assoc mul *)

    [%s (("?a" - "?c") + "?b")] @-> [%s ("?a" + ("?b" - "?c"))];

    [%s ("?a" - "?b")] @-> [%s ("?a" + ("-1." * "?b"))]; (* sub canon *)
    rewrite [%s ("?a" / "?b")]  [%s ("?a" * (pow "?b" "-1.0"))] ~if_:(is_not_zero "b"); (* div canon *)

    [%s ("?a" + "0.")] @-> [%s "?a"]; (* zero-add *)
    [%s ("?a" * "0.")] @-> [%s "0."]; (* zero-mul *)
    [%s ("?a" * "1.")] @-> [%s "?a"]; (* one-mul *)

    [%s "?a"] @-> [%s ("?a" + "0.")]; (* add-zero *)
    [%s "?a"] @-> [%s ("?a" * "1.")]; (* mul-one *)

    [%s ("?a" - "?a")] @-> [%s "0."]; (* cancel sub *)
    rewrite [%s ("?a" / "?a")] [%s "1."] ~if_:(is_not_zero "a"); (* cancel div *)
    
    [%s("?a" * ("?b" + "?c"))] @-> [%s (("?a" * "?b") + ("?a" * "?c"))]; (* distribute *)
    [%s (("?a" * "?b") + ("?a" * "?c"))] @-> [%s ("?a" * ("?b" + "?c"))]; (* factor *)
    
    [%s ((pow "?a" "?b") * (pow "?a" "?c"))] @-> [%s (pow "?a" ("?b" + "?c"))]; (* pow-mul *)
    rewrite [%s (pow "?x" "0.")] [%s "1."]  ~if_:(is_not_zero "x"); (* po0 *)
    
    [%s (pow "?x" "1.")] @-> [%s "?x"]; (* pow1 *)
    
    [%s (pow "?x" "2.")] @-> [%s ("?x" * "?x")]; (* po2 *)
    
    rewrite [%s (pow "?x" "-1.")] [%s("1." / "?x")] ~if_:(is_not_zero "x"); (* pow-recip *)
    
    rewrite [%s ("?x" * ("1." / "?x"))]  [%s "1."] ~if_:(is_not_zero "x"); (* recip mul div *)
    
    rewrite [%s (d "?x" "?x")]  [%s "1."] ~if_:(is_sym "x"); (* d variable *)
    
    rewrite [%s (d "?x" "?c")] [%s"0."] ~if_:(is_sym "x" && is_const_or_distinct_var "c" "x");
    (* d constant *)
    
    [%s (d "?x" ("?a" + "?b"))] @-> [%s ((d "?x" "?a") + (d "?x" "?b"))]; (* d-add *)
    [%s (d "?x" ("?a" * "?b"))] @-> [%s (("?a" * (d "?x" "?b")) + ("?b" * (d "?x" "?a")))]; (* d-mul *)
    
    [%s (d "?x" (sin "?x"))] @-> [%s (cos "?x")]; (* d-sin *)
    
    [%s (d "?x" (cos "?x"))] @-> [%s ("-1." * (sin "?x"))]; (* d-cos *)
    
    rewrite [%s (d "?x" (ln "?x"))] [%s (1 / "?x")] ~if_:(is_not_zero "x"); (* d-ln *)
    
    rewrite [%s (d "?x" (pow "?f" "?g"))]
      [%s ((pow "?f" "?g") * (((d "?x" "?f") * ("?g" / "?f")) + ((d "?x" "?g") * (ln "?f"))))]
      ~if_:(is_not_zero "f" && is_not_zero "g");
    [%s (i "1." "?x")] @-> [%s "?x"];
    rewrite
      [%s (i (pow "?x" "?c") "?x")]
      [%s ((pow "?x" ("?c" + "1.")) / ("?c" + "1."))]
      ~if_:(is_const "c");
    [%s (i (cos "?x") "?x")] @-> [%s (sin "?x")];
    [%s (i (sin "?x") "?x")] @-> [%s ("-1." * (cos "?x"))];
    [%s (i ("?f" + "?g") "?x")] @-> [%s ((i "?f" "?x") + (i "?g" "?x"))];
    [%s (i ("?f" - "?g") "?x")] @-> [%s ((i "?f" "?x") - (i "?g" "?x"))];
    [%s (i ("?a" * "?b") "?x")] @-> [%s (("?a" * (i "?b" "?x")) - (i ((d "?x" "?a") * (i "?b" "?x")) "?x"))];
  ]

let run_and_check1 ?node_limit ?fuel rules s1 f () =
  let graph = EGraph.init () in
  let term_1 = EGraph.add_node graph (L.of_sexp s1) in
  let reached_saturation = EGraph.run_until_saturation ?node_limit ?fuel graph rules in
  begin
    match fuel, node_limit with
      _ , Some _ | Some _, _ -> ()
    | None, None -> Alcotest.(check bool) "reaches equality saturation" true reached_saturation
  end;
  f graph term_1

let run_and_check2 ?node_limit ?fuel rules s1 s2 f () =
  let graph = EGraph.init () in
  let term_1 = EGraph.add_node graph (L.of_sexp s1) in
  let term_2 = EGraph.add_node graph (L.of_sexp s2) in
  let reached_saturation = EGraph.run_until_saturation ?node_limit ?fuel graph rules in
  begin
    match fuel, node_limit with
      _ , Some _ | Some _, _ -> ()
    | None, None -> Alcotest.(check bool) "reaches equality saturation" true reached_saturation
  end;
  f graph term_1 term_2

let check_proves_equal ?node_limit ?fuel rules s1 s2 () =
  let graph = EGraph.init () in
  let term_1 = EGraph.add_node graph (L.of_sexp s1) in
  let term_2 = EGraph.add_node graph (L.of_sexp s2) in
  let terms_are_equal graph = EGraph.class_equal (EGraph.freeze graph) term_1 term_2 in
  let _reached_saturation = EGraph.run_until_saturation ~until:terms_are_equal ?node_limit ?fuel graph rules in
  (* begin
   *   match fuel, node_limit with
   *     _ , Some _ | Some _, _ -> ()
   *   | None, None -> Alcotest.(check bool) "reaches equality saturation" true reached_saturation
   * end; *)
  Alcotest.(check bool) "proves terms are equal modulo rewriting"
    true 
    (terms_are_equal graph)

let check_cannot_prove_equal ?node_limit ?fuel rules s1 s2 =
  run_and_check2 ?node_limit ?fuel rules s1 s2 (fun graph term_1 term_2 ->
    Alcotest.(check bool) "must not prove terms are equal modulo rewriting"
      false 
      (EGraph.class_equal (EGraph.freeze graph) term_1 term_2)
  )

let check_extract ?node_limit ?fuel rules s1 s2 =
  run_and_check1 ?node_limit ?fuel rules s1 (fun graph term_1 ->
    Alcotest.(check sexp)
      "extracted expression matches"
      s2
      (L.to_sexp (Extractor.extract graph term_1))
  )

let () =
  Alcotest.run "math"
    [("proving with addition",
     let rules = [
       (* add comm *) [%s ("?a" + "?b")] @-> [%s ("?b" + "?a")];
       (* add assoc *) [%s ("?a" + ("?b" + "?c"))] @-> [%s (("?a" + "?b") + "?c")];
     ] in [
       "constants are simplified", `Quick, check_proves_equal rules
                                             [%s (1 + (2 + (3 + (4 + (5 + (6 + 7))))))]
                                             [%s (7 + (6 + (5 + (4 + (3 + (2 + 1))))))];
       "constants are evaluated", `Quick, check_proves_equal rules
                                            [%s (1 + (2 + (3 + (4 + (5 + (6 + 7))))))]
                                            [%s 28];
       "symbols can be rearranged", `Quick, check_proves_equal rules
                                              [%s (1 + (x + (2 + (3 + (4 + (5 + (6 + 7)))))))]
                                              [%s (x + 28)];
     ]);
     "proving arithmetic with full rule set", [
       "subtraction works with symbols", `Quick,
       check_proves_equal ~node_limit:(`Bounded 10_000) ~fuel:(`Bounded 30) rules
         [%s (x - x)] [%s 0.];
       "subtraction works with non obvious equalities", `Quick,
       check_proves_equal ~node_limit:(`Bounded 10_000) ~fuel:(`Bounded 30) rules
         [%s (x - (x + 0))] [%s 0.];
       "subtraction works with complex expressions", `Quick,
       check_proves_equal ~node_limit:(`Bounded 10_000) ~fuel:(`Bounded 30) rules
         [%s ((sqrt 5.) - (sqrt 5.))] [%s 0.];
       "subtraction works with complex expressions and addition", `Quick,
       check_proves_equal ~node_limit:(`Bounded 10_000) ~fuel:(`Bounded 30) rules
         [%s ((1 + (sqrt 5.)) - ((sqrt 5.) + 1))] [%s 0.];
       "multiplication is rewritten", `Quick,
       check_proves_equal ~node_limit:(`Bounded 75_000) ~fuel:(`Bounded 30) rules
         [%s (1 - x)] [%s (1 + ("-1." * x))];
       "multiplication is propagated", `Quick,
       check_proves_equal ~node_limit:(`Bounded 75_000) ~fuel:(`Bounded 30) rules
         [%s ((1 - x) + x)] [%s (1 + (x + ("-1." * x)))];
       "1subtraction can be reverted", `Quick,
       check_proves_equal ~node_limit:(`Bounded 75_000) ~fuel:(`Bounded 30) rules
         [%s (1 + ("-1." * x))] [%s (1 - x)];
       "subtraction can be cancelled", `Quick,
       check_proves_equal ~node_limit:(`Bounded 75_000) ~fuel:(`Bounded 30) rules
         [%s (x + ("-1." * x))] [%s 0];
       "subtraction can be propagated and cancelled", `Quick,
       check_proves_equal ~node_limit:(`Bounded 100_000_000) ~fuel:(`Bounded 30) rules
         [%s (1 + (x - x))] [%s ((1 - x) + x) ];
       "complex subtraction can be propagated and cancelled", `Quick,
       check_proves_equal ~node_limit:(`Bounded 100_000_000) ~fuel:(`Bounded 30) rules
         [%s (1 + (x - x))] [%s ((1 - x) + x) ];
       "plus minus one can be propagated and cancelled", `Quick,
       check_proves_equal ~node_limit:(`Bounded 100_000_000) ~fuel:(`Bounded 30) rules
         [%s ((1 - x) + (1 + x)) ] [%s 2];
       "division can be simplified", `Quick,
       check_proves_equal ~node_limit:(`Bounded 100_000_000) ~fuel:(`Bounded 30) rules
         [%s (2 / 2) ] [%s 1];
       "division with numerator 0 can be simplified", `Quick,
       check_proves_equal ~node_limit:(`Bounded 100_000_000) ~fuel:(`Bounded 30) rules
         [%s ((x - x) / 2) ] [%s 0];
       "multiplication with 0 is simplified", `Quick,
       check_proves_equal ~node_limit:(`Bounded 100_000_000) ~fuel:(`Bounded 30) rules
         [%s (x * 0.) ] [%s 0];
     ];
     "does not prove invalid equalities", [
       "multiplication and addition are not equal", `Quick,
      check_cannot_prove_equal ~node_limit:(`Bounded 10_000) ~fuel:(`Bounded 30) rules [%s (x + y)] [%s (x / y)]
     ];
     "reasoning about derivatives", [
       "dx/dy of x is 1", `Quick,
       check_proves_equal ~node_limit:(`Bounded 10_000) ~fuel:(`Bounded 30) rules
         [%s (d x x)]  [%s 1 ];
       "dx/dy of y is 0", `Quick,
       check_proves_equal ~node_limit:(`Bounded 10_000) ~fuel:(`Bounded 30) rules
         [%s (d x y)]  [%s 0 ];
       "dx/dy of 1 + 2x is 2", `Quick,
       check_proves_equal ~node_limit:(`Bounded 100_000) ~fuel:(`Bounded 35) rules
         [%s (d x (1 + (2. * x)))]  [%s 2. ];
       "dx/dy of xy + 1 is y", `Quick,
       check_extract ~node_limit:(`Unbounded) ~fuel:(`Bounded 15) rules
         [%s (d x (1. + (y * x)))]  [%s y ];
       "dx/dy of ln x is 1 / x", `Quick,
       check_proves_equal ~node_limit:(`Bounded 100_000) ~fuel:(`Bounded 35) rules
         [%s (d x (ln x))]  [%s 1 / x ];
     ];
    ]
