[@@@warning "-26-32"]
open Containers
open Defs

let build_and_display_graph graph =
  let dot = Odot.string_of_graph @@ EGraph.to_dot graph in
  let outfile =Feather.(collect stdout (process "mktemp" [])) in
  (* write dot representation to temp file *)
  IO.with_out outfile (Fun.flip IO.write_line dot);
  (* run dot and output  *)
  Feather.(run @@ process "dot" [(* "-Gdpi=100"; *) "-Tpng"; outfile; "-O"]);
  (* show result with feh *)
  Feather.(run @@ process "feh" [outfile ^ ".png"])

let run_and_check1 ?node_limit ?fuel rules s1 f =
  let graph = EGraph.init () in
  let term_1 = EGraph.add_node graph (L.of_sexp s1) in
  let reached_saturation = EGraph.run_until_saturation ?node_limit ?fuel graph rules in
  begin
    match fuel, node_limit with
      _ , Some _ | Some _, _ -> ()
    | None, None -> Alcotest.(check bool) "reaches equality saturation" true reached_saturation
  end;
  f graph term_1

let check_print ?node_limit ?fuel rules s1 =
  run_and_check1 ?node_limit ?fuel rules s1 (fun graph _term_1 ->
    build_and_display_graph graph
  )

let check_extract ?node_limit ?fuel rules s1 s2 =
  run_and_check1 ?node_limit ?fuel rules s1 (fun graph term_1 ->
    Alcotest.(check sexp) "sexps are equal" s2 @@ L.to_sexp @@ Extractor.extract graph term_1
  )


let check_proves_equal ?node_limit ?fuel rules s1 s2 =
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


let () =
  Alcotest.run "basic" [
    "experiments", [
      "first experiment", `Quick, 
      fun () ->
        check_extract ~node_limit:(`Bounded 10000_175_000) ~fuel:(`Bounded 4000) rules
          [%s (d x (pow x 3))] [%s ((x * x) * 3.)]
    ]
  ]


  (* let graph = EGraph.init () in
   * let expr = L.of_sexp [%s (2 * 2)] in
   * let expr_id = EGraph.add_node graph expr in
   * (\* let expr2 = L.of_sexp [%s (3 * (2 * a))] in
   *  * let expr2_id = EGraph.add_node graph expr2 in
   *  * let expr3 = L.of_sexp [%s ((2 * 3) / 0)] in
   *  * let expr3_id = EGraph.add_node graph expr3 in *\)
   * 
   * build_and_display_graph graph;
   * print_endline @@ Sexplib0.Sexp.to_string_hum @@ L.to_sexp (Extractor.extract graph expr_id) *)




