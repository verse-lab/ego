open Ego.Generic

let sexp =
  (module struct
    type t = Sexplib.Sexp.t
    let pp = Sexplib.Sexp.pp_hum
    let equal = Sexplib.Sexp.equal
  end : Alcotest.TESTABLE with type t = Sexplib.Sexp.t)


module L = struct

  type 'a shape =
    | And of 'a * 'a
    | Or of 'a * 'a
    | Not of 'a
    | Impl of 'a * 'a
    | Bool of bool
    | Symbol of string [@@deriving ord, show]

  type t = Mk of t shape [@@unboxed]

  type op =
    | AndOp
    | OrOp
    | NotOp
    | ImplOp
    | BoolOp of bool
    | SymbolOp of string [@@deriving eq, ord]

  let rec of_sexp = function[@warning "-8"]
    | Sexplib0.Sexp.Atom "true" -> Mk (Bool true)
    | Sexplib0.Sexp.Atom "false" -> Mk (Bool false)
    | Sexplib0.Sexp.Atom s -> Mk (Symbol s)
    | List [Atom "&&"; l; r] -> Mk (And (of_sexp l, of_sexp r))
    | List [Atom "||"; l; r] -> Mk (Or (of_sexp l, of_sexp r))
    | List [Atom "not"; l] -> Mk (Not (of_sexp l))
    | List [Atom "=>"; l; r] -> Mk (Impl (of_sexp l, of_sexp r))

  let op_of_string = function[@warning "-8"]
    | "true" -> (BoolOp true)
    | "false" -> (BoolOp false)
    | "&&" -> AndOp
    | "||" -> OrOp
    | "not" -> NotOp
    | "=>" -> ImplOp
    | s -> (SymbolOp s)


  let rec to_sexp = function[@warning "-8"]
    |  Mk (Bool true) ->Sexplib0.Sexp.Atom "true"
    |  Mk (Bool false) ->Sexplib0.Sexp.Atom "false"
    |  Mk (Symbol s) ->Sexplib0.Sexp.Atom s
    |  Mk (And (l, r)) -> List [Atom "&&"; to_sexp l; to_sexp r]
    |  Mk (Or (l, r)) -> List [Atom "||"; to_sexp l; to_sexp r]
    |  Mk (Not l) -> List [Atom "not"; to_sexp l]
    |  Mk (Impl (l, r)) -> List [Atom "=>"; to_sexp l; to_sexp r]

  let op = function
    | And _ -> AndOp
    | Or _ -> OrOp
    | Not _ -> NotOp
    | Impl _ -> ImplOp
    | Bool b -> BoolOp b
    | Symbol s -> SymbolOp s

  let children = function
    | And (l,r) -> [l;r]
    | Or (l,r) -> [l;r]
    | Not l -> [l]
    | Impl (l,r) -> [l;r]
    | Bool _ | Symbol _ -> []

  let map_children term f = match term with
    | And (l,r) -> And (f l, f r)
    | Or (l,r) -> Or (f l, f r)
    | Not l -> Not (f l)
    | Impl (l,r) -> Impl (f l, f r)
    | Bool b -> Bool b
    | Symbol s -> Symbol s

  let make op children = match[@warning "-8"] op,children with
    | AndOp, [l;r]  -> And (l,r)
    | OrOp, [l;r]  -> Or (l,r)
    | NotOp, [l]  -> Not l
    | ImplOp, [l;r]  -> Impl (l,r)
    | BoolOp b, [] -> Bool b
    | SymbolOp s, [] -> Symbol s

end

module C = struct
  type t = float [@@deriving ord]
  let cost f : Ego.Id.t L.shape -> t = function
    | L.And (l, r) -> f l +. f r +. 3.
    | L.Or (l, r) -> f l +. f r +. 2.0
    | L.Impl (l, r) -> f l +. f r +. 1.0
    | L.Not l -> f l +. 3.0
    | L.Symbol _ -> 1.0
    | L.Bool _ -> 1.0
end

module A = struct type t = unit type data = bool option[@@deriving eq,show] let default = None end
module MA (S : GRAPH_API
           with type 'p t = (Ego.Id.t L.shape, A.t, A.data, 'p) egraph
            and type 'a shape := 'a L.shape
            and type analysis := A.t
            and type data := A.data
            and type node := L.t)  = struct
  type 'p t = (Ego.Id.t L.shape, A.t, A.data, 'p) egraph

  let eval : A.data L.shape -> A.data =
    function
    | L.Bool c  -> Some c
    | L.Not (Some b) -> Some (not b)
    | L.And (Some l, Some r) -> Some (l && r)
    | L.Or (Some l, Some r) -> Some (l || r)
    | L.Impl (Some l, Some r) -> Some ((not l) || r)
    | _ -> None

  let make : ro t -> Ego.Id.t L.shape -> A.data =
    fun graph term -> eval (L.map_children term (S.get_data graph))

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
      let nw_cls = S.add_node graph (L.Mk (Bool n)) in
      S.merge graph nw_cls cls

end

module EGraph = Make (L) (A) (MA)
module Extractor = MakeExtractor (L) (C)


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



let qf = Query.of_sexp L.op_of_string 
let (@->) from into = EGraph.Rule.make_constant ~from:(qf from) ~into:(qf into) 
let rewrite from into ~if_ = EGraph.Rule.make_conditional ~from:(qf from) ~into:(qf into) ~cond:if_ 

let rules = [
  (* def_imply *)  [%s ("?a" => "?b")]  @->     [%s ((not "?a") || "?b")];
  (* double_neg *) [%s (not (not "?a"))] @-> [%s "?a"];
  (* assoc_or *)    [%s ( "?a" || ("?b" || "?c"))] @-> [%s (("?a" || "?b") || "?c")];
  (* dist_and_or *) [%s ("?a" && ("?b" || "?c"))] @-> [%s (("?a" && "?b") || ("?a" && "?c"))];
  (* dist_or_and *) [%s ("?a" || ("?b" || "?c"))] @-> [%s (("?a" || "?b") && ("?a" || "?c"))];
  (* comm_or *)     [%s ("?a" || "?b")] @->        [%s ("?b" || "?a")];
  (* comm_and *)    [%s ("?a" && "?b")] @-> [%s ("?b" && "?a")];
  (* lem *)         [%s ("?a" || (not "?a"))] @->    [%s"true"];
  (* or_true *)     [%s ("?a" || "true")] @-> [%s "true"];
  (* and_true *)    [%s ("?a" && "true")] @-> [%s"?a"];
  (* contrapositive *) [%s ("?a" => "?b")] @->    [%s ((not "?b") => (not "?a"))];
  (* lem_imply *) [%s (("?a" => "?b") && ((not "?a") => "?c")) ] @-> [%s ("?b" || "?c") ];
]

let proves  ?(match_limit=1_000) ?(ban_length=5) ?node_limit ?fuel start goals () =
  let graph = EGraph.init () in
  let start = EGraph.add_node graph (L.of_sexp start) in
  let scheduler = Ego.Generic.Scheduler.Backoff.with_params ~match_limit ~ban_length in
  ignore @@ EGraph.run_until_saturation ~scheduler ?fuel ?node_limit graph rules;
  List.iter (fun goal ->
    let goal = EGraph.add_node graph (L.of_sexp goal) in
    Alcotest.(check bool)
      "goal can be proved from start"
      true
      (EGraph.class_equal (EGraph.freeze graph) start goal)
  ) goals

let proves_cached ?(match_limit=1_000) ?(ban_length=5) ?node_limit ?fuel start goals () =
  let graph = EGraph.init () in
  let start = EGraph.add_node graph (L.of_sexp start) in
  let goals = List.map (fun goal -> EGraph.add_node graph (L.of_sexp goal)) goals in
  let last =
    let rec last acc ls = match ls with
      | [] -> acc
      | h :: t -> last h t in
    last start goals in
  let scheduler = Ego.Generic.Scheduler.Backoff.with_params ~match_limit ~ban_length in
  ignore @@ EGraph.run_until_saturation ~scheduler ?fuel ?node_limit ~until:(fun graph ->
    EGraph.class_equal (EGraph.freeze graph) start last
  ) graph rules;
  List.iter (fun goal ->
    Alcotest.(check bool)
      "goal can be proved from start"
      true
      (EGraph.class_equal (EGraph.freeze graph) start goal)
  ) goals

let () =
  Alcotest.run "prop" [
    "ematch tests", [
      "check matches after merging", `Quick, 
        (fun () -> let graph = EGraph.init () in
        let n1 = EGraph.add_node graph (L.of_sexp [%s (x && z)]) in
        let n2 = EGraph.add_node graph (L.of_sexp [%s (y && z)]) in
        EGraph.merge graph n1 n2;
        EGraph.rebuild graph;
        let query = qf [%s "?a" && z] in
        let matches = EGraph.find_matches (EGraph.freeze graph) query |> Iter.length in
        Alcotest.(check int) "2 matches" 2 matches);

      "check matches after saturating", `Quick, 
        fun () -> let graph = EGraph.init () in
        let scheduler = Ego.Generic.Scheduler.Backoff.with_params ~match_limit:1000 ~ban_length:5 in
        let _ = EGraph.add_node graph (L.of_sexp [%s (x && y)]) in
        let query = [%s "?a" && "?b"] @-> [%s "?b" && "?a"] in
        ignore @@ EGraph.run_until_saturation ~scheduler graph [query];
        let q = qf [%s "?a" && "?b"] in
        let matches = EGraph.find_matches (EGraph.freeze graph) q |> Iter.length in
        Alcotest.(check int) "2 matches" 2 matches
    ];
    "proving contrapositive", [
      "proves idempotent", `Quick, proves [%s (x => y)] [[%s (x => y)]];
      "proves negation", `Quick, proves [%s (x => y)] [[%s (x => y)];
                                                       [%s ((not x) || y)]];
      "proves double negation", `Quick, proves [%s (x => y)] [[%s (x => y)];
                                                              [%s ((not x) || y)];
                                                              [%s ((not x) || (not (not y)))]];
      "proves commutativity", `Quick, proves [%s (x => y)] [[%s (x => y)];
                                                            [%s ((not x) || y)];
                                                            [%s ((not x) || (not (not y)))];
                                                            [%s ((not (not y)) || (not x))];
                                                           ];
      "proves contrapositive", `Quick, proves [%s (x => y)] [[%s (x => y)];
                                                             [%s ((not x) || y)];
                                                             [%s ((not x) || (not (not y)))];
                                                             [%s ((not (not y)) || (not x))];
                                                             [%s ((not y) => (not x))];
                                                            ]];
    "proving chain", [
      "proves idempotent", `Quick, proves [%s ((x => y) && (y => z))] [[%s ((x => y) && (y => z))]];
      "proves contrapositive", `Quick, proves [%s ((x => y) && (y => z))]
                                         [[%s ((x => y) && (y => z))];
                                          [%s (((not y) => (not x)) && (y => z))]];
      "proves commutativity", `Quick, proves [%s ((x => y) && (y => z))]
                                         [[%s ((x => y) && (y => z))];
                                          [%s (((not y) => (not x)) && (y => z))];
                                          [%s ((y => z) && ((not y) => (not x)))]];
      "proves negation", `Quick, proves [%s ((x => y) && (y => z))]
                                         [[%s ((x => y) && (y => z))];
                                          [%s (((not y) => (not x)) && (y => z))];
                                          [%s ((y => z) && ((not y) => (not x)))];
                                          [%s (z || (not x))]
                                         ];
      "proves commutativity", `Quick, proves
                                        ~node_limit:(`Bounded 10_000)
                                        ~fuel:(`Bounded 60)
                                        [%s ((x => y) && (y => z))]
                                        [[%s ((x => y) && (y => z))];
                                         [%s (((not y) => (not x)) && (y => z))];
                                         [%s ((y => z) && ((not y) => (not x)))];
                                         [%s (z || (not x))];
                                         [%s ((not x) || z)]; ];
      "proves chain", `Quick, proves_cached
                                ~match_limit:(10_000) ~ban_length:5
                                ~node_limit:(`Bounded 600_000)
                                ~fuel:(`Bounded 50)
                                [%s ((x => y) && (y => z))]
                                [[%s ((x => y) && (y => z))];
                                 [%s (((not y) => (not x)) && (y => z))];
                                 [%s ((y => z) && ((not y) => (not x)))];
                                 [%s (z || (not x))];
                                 [%s ((not x) || z)];
                                 [%s (x => z)];
                                ]
    ]]

