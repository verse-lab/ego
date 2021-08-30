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

let () =
  check_print ~node_limit:175_000 ~fuel:35 rules [%s (d x (pow x 3))]

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




