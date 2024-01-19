open Ego.Basic

let sexp =
  (module struct
    type t = Sexplib.Sexp.t
    let pp = Sexplib.Sexp.pp_hum
    let equal = Sexplib.Sexp.equal
  end : Alcotest.TESTABLE with type t = Sexplib.Sexp.t)

let documentation_example () =
  let graph = EGraph.init () in
  let expr_id = EGraph.add_sexp graph [%s ((a << 1) / 2)] in
  let from = Query.of_sexp [%s ("?a" << 1)]
  and into = Query.of_sexp [%s ("?a" * 2)] in
  let rule = Rule.make ~from ~into |> function
  | Some rule -> rule
  | None -> Alcotest.fail "could not build rule" in
  Alcotest.(check bool)
    "should reach equality saturation"
    true (EGraph.run_until_saturation graph [rule]);
  let cost_function score (sym, children) =
    let node_score = 
      match Symbol.to_string sym with
      | "*" -> 1.
      | "/" -> 1.
      | "<<" -> 2.
      | _ -> 0. in
    node_score +. List.fold_left (fun acc vl -> acc +. score vl) 0. children in
  let result = EGraph.extract cost_function graph expr_id in
  Alcotest.(check sexp)
    "extracted expression has been simplified"
    [%s ((a * 2) / 2)]
    result

(*
      We start off with two exprs, (g 1) and (g 2), and merge these two.
      Then we add a rule (g ?a) -> (h ?a), creating (h 1) and (h 2) which are also equal to (g 1) and (g 2).
      We extract the cheapest term using a cost function constructed such that (h 2) is lowest cost term, with cost 11.
      Previously (h 1), which has cost 12, was extracted instead.

      (h 1): 12
      (h 2): 11
      (g 1): inf
      (g 2): inf
*)
let test_match () =
  let graph = EGraph.init () in
  let expr_id1 = EGraph.add_sexp graph [%s (g 1)] in
  let _ = EGraph.add_sexp graph [%s (g 2)] in
  let from = Query.of_sexp [%s (g 1)]
  and into = Query.of_sexp [%s (g 2)] in
  let rule1 = Rule.make ~from ~into |> function
  | Some rule -> rule
  | None -> Alcotest.fail "could not build rule" in
  let from = Query.of_sexp [%s (g "?a")]
  and into = Query.of_sexp [%s (h "?a")] in
  let rule2 = Rule.make ~from ~into |> function
  | Some rule -> rule
  | None -> Alcotest.fail "could not build rule" in
  Alcotest.(check bool)
    "should reach equality saturation"
    true (EGraph.run_until_saturation graph [rule1; rule2]);
  let cost_function score (sym, children) =
    let node_score =
      match Symbol.to_string sym with
      | "g" -> 9999999.
      | "h" -> 10.
      | "1" -> 2.
      | "2" -> 1.
      | _ -> 9999999. in
    node_score +. List.fold_left (fun acc vl -> acc +. score vl) 0. children in
  let result = EGraph.extract cost_function graph expr_id1 in
  Alcotest.(check sexp)
    "cheapest expression is (h 2)"
    [%s (h 2)]
    result


let () =
  Alcotest.run "basic" [
    ("documentation", ["example given in documentation works as written", `Quick, documentation_example]);
    ("test matching", ["test matching", `Quick, test_match])
  ]
