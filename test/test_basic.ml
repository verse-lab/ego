
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


let () =
  Alcotest.run "basic" [
    ("documentation", ["example given in documentation works as written", `Quick, documentation_example])
  ]
