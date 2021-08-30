open [@warning "-33"] Containers
open Language
open Types
module Id = Id

let lappend_pair a (b,c) = (a,b,c)
type 'a query = 'a Query.t

type ('node, 'analysis, 'data, 'permission) egraph = {
  mutable version: int;
  analysis: 'analysis;

  uf: Id.store;                                        (* tracks equivalence classes of
                                                          class ids  *)
  class_members:
    (('node * Id.t) Vector.vector * 'data option) Id.Map.t;       (* maps classes to the canonical nodes
                                                                     they contain, and any classes that are
                                                                     children of these nodes *)
  hash_cons: ('node, Id.t) Hashtbl.t;                  (* maps cannonical nodes to their
                                                          equivalence classes *)
  worklist: Id.t Vector.vector;                        (* List of equivalence classes for which
                                                          nodes are out of date - i.e
                                                          cannoncial(node) != node *)
}


module MakeInt (L: LANGUAGE) = struct

  let (.@[]) self fn = fn self [@@inline always]


  (* *** Initialization *)
  let init analysis = {
    version=0;
    analysis;
    uf=Id.create_store ();
    class_members=Id.Map.create 10;
    hash_cons=Hashtbl.create 10;
    worklist=Vector.create ();
  }

  let get_analysis self = self.analysis

  (* *** Eclasses *)
  let new_class self =
    let id = Id.make self.uf () in
    id

  let get_class_members self id =
    match Id.Map.find_opt self.class_members id with
    | Some (classes, _) -> classes
    | None ->
      let cls = Vector.create () in
      Id.Map.add self.class_members id (cls, None);
      cls

  let set_data self id data =
    let (classes, _) = Id.Map.find self.class_members (Id.find self.uf id) in
    Id.Map.replace self.class_members (Id.find self.uf id) (classes, Some data)

  let get_data self id =
    let (_, data) = Id.Map.find self.class_members (Id.find self.uf id) in
    Option.get_exn data

  let canonicalise self node = L.map_children node (Id.find self.uf)

  let find self vl = Id.find self.uf vl

  let append_to_worklist self vl =
    Vector.push self.worklist vl

  (* *** Exports *)
  (* **** Export eclasses *)
  let eclasses self =
    let r = Id.Map.create 10 in
    Hashtbl.iter (fun node eid ->
      let eid = Id.find self.uf eid in
      match Id.Map.find_opt r eid with
      | None -> let ls = Vector.of_list [node] in Id.Map.add r eid ls
      | Some ls -> Vector.push ls node
    ) self.hash_cons;
    r

  let class_equal self cls1 cls2 =
    Id.equal self.uf cls1 cls2

end

module MakePrinter (L: LANGUAGE) (A: sig type[@warning "-34"] t type data [@@deriving show] end) = struct

  open (MakeInt(L))

  (* *** Pretty printing *)
  let pp fmt self =
    let eclasses = eclasses self in
    let rec pp_node_by_id fmt id =
      let vls = Id.Map.find eclasses id in
      let open Format in
      pp_print_string fmt "{";
      pp_open_hovbox fmt 1;
      Vector.pp
        ~pp_sep:(fun fmt () -> pp_print_string fmt ","; pp_print_space fmt ())
        (L.pp_shape pp_node_by_id) fmt vls;
      pp_close_box fmt ();
      pp_print_string fmt "}" in
    let pp fmt self = 
      let open Format in
      pp_print_string fmt "(egraph";
      pp_open_hovbox fmt 1;
      pp_print_space fmt ();
      pp_print_string fmt "(eclasses ";
      pp_open_hvbox fmt 1;
      Id.Map.to_seq self.class_members
      |> Seq.to_list
      |> pp_print_list ~pp_sep:pp_print_space
           (fun fmt (cls, (elts, data)) ->
              pp_print_string fmt "(";
              pp_open_hvbox fmt 1;
              EClassId.pp fmt cls;
              if not @@ Vector.is_empty elts then
                pp_print_space fmt ();
              Vector.pp ~pp_sep:pp_print_space
                (fun fmt (node, id) ->
                   pp_print_string fmt "(";
                   pp_open_hbox fmt ();
                   EClassId.pp fmt id;
                   pp_print_space fmt ();
                   L.pp_shape pp_node_by_id fmt node; 
                   begin match data with
                   | None -> ()
                   | Some data -> 
                     pp_print_space fmt ();
                     pp_print_string fmt ":";
                     A.pp_data fmt data;
                   end;
                   pp_close_box fmt ();
                   pp_print_string fmt ")";
                ) fmt elts;
              pp_close_box fmt ();
              pp_print_string fmt ")";
           ) fmt;
      pp_close_box fmt ();
      pp_print_string fmt ")";
      pp_print_space fmt ();
      pp_print_string fmt "(enodes ";
      pp_open_hvbox fmt 1;
      Hashtbl.to_seq self.hash_cons
      |> Seq.to_list
      |> pp_print_list ~pp_sep:pp_print_space
           (fun fmt (node, cls) ->
              pp_print_string fmt "(";
              pp_open_hvbox fmt 1;
              EClassId.pp fmt cls;
              pp_print_space fmt ();
              L.pp_shape pp_node_by_id fmt node; 
              pp_close_box fmt ();
              pp_print_string fmt ")";
           ) fmt;
      pp_close_box fmt ();
      pp_print_string fmt ")";
      pp_close_box fmt ();
      pp_print_string fmt ")" in
    pp fmt self

  (* **** Export as dot *)
  let to_dot self =
    let eclasses = eclasses self in

    let pp_node_by_id fmt id =
      let seen_set = Id.Set.create 10 in
      let rec  pp_node_by_id fmt id =
        let id = self.@[find] id in
        if Id.Set.mem seen_set id
        then ()
        else begin
          Id.Set.insert seen_set id;
          let vls = Id.Map.find eclasses id in
          let open Format in
          pp_print_string fmt "{";
          pp_open_hovbox fmt 1;
          Vector.pp
            ~pp_sep:(fun fmt () -> pp_print_string fmt ","; pp_print_space fmt ())
            (L.pp_shape pp_node_by_id) fmt vls;
          pp_close_box fmt ();
          pp_print_string fmt "}"
        end in
      pp_node_by_id fmt id in
    let stmt_list = 
      let rev_map =
        Hashtbl.to_seq self.hash_cons
        |> Seq.map Pair.swap
        |> Id.Map.of_seq in
      let to_label id =
        let to_str id = 
          match Id.Map.find_opt rev_map id with
          | None -> Format.to_string EClassId.pp id
          | Some node -> Format.to_string (L.pp_shape pp_node_by_id) node in
        to_str id in
      let to_id id =
        Odot.Double_quoted_id (to_label id) in
      let to_node_id node =
        Odot.Double_quoted_id (Format.to_string (L.pp_shape pp_node_by_id) node) in
      let to_subgraph_id id =
        Odot.Simple_id (Printf.sprintf "cluster_%d"  (Id.repr id)) in
      let eclass_label eclass =
        let eclass_txt = Format.to_string EClassId.pp eclass in
        let data = get_data self eclass |> A.show_data in
        eclass_txt ^ " = " ^ data in
      let sub_graphs =
        (fun f -> Fun.flip Id.Map.iter eclasses (Fun.curry f))
        |> Iter.map (fun (eclass, (enodes: (Id.t L.shape, _) Vector.t)) ->
          let nodes =
            Vector.to_iter enodes
            |> Iter.map (fun (node: Id.t L.shape) ->
              let node_id = to_node_id node in
              let attrs = Odot.[Simple_id "label",
                                Some (Double_quoted_id
                                        (Format.to_string (L.pp_shape pp_node_by_id) node))] in
              Odot.Stmt_node ((node_id, None), attrs))
            |> Iter.to_list in
          Odot.(Stmt_subgraph {
            sub_id= Some (to_subgraph_id eclass);
            sub_stmt_list=
              Stmt_attr (
                Attr_graph [
                  (Simple_id "label", Some (Double_quoted_id (eclass_label eclass)))
                ]) :: nodes;
          })
        )
        |> Iter.to_list in
      let edges =
        (fun f -> Fun.flip Id.Map.iter eclasses (Fun.curry f))
        |> Iter.flat_map (fun (_eclass, enodes) ->
          Vector.to_iter enodes
          |> Iter.flat_map (fun node ->
            let label = to_node_id node in
            Iter.of_list (L.children node)
            |> Iter.map (fun child ->
              let child_label = to_id child in
              Odot.(Stmt_edge (
                Edge_node_id (label, None),
                [Edge_node_id (child_label, None)],
                []
              ))
            )
          )
        )
        |> Iter.to_list in
      (List.append sub_graphs edges) in
    Odot.{
      strict=true;
      kind=Digraph;
      id=None;
      stmt_list;
    }

  (* **** Print as dot *)
  let pp_dot fmt st =
    Format.pp_print_string fmt (Odot.string_of_graph (to_dot st))

end

module MakeExtractor (L: LANGUAGE) (E: COST with type node := Id.t L.shape) = struct

  open (MakeInt(L))

  let extract eg =
    let eclasses = eg.@[eclasses] in
    let cost_map = Id.Map.create 10 in
    let node_total_cost node =
      let has_cost id = Id.Map.mem cost_map (eg.@[find] id) in
      if List.for_all has_cost (L.children node)
      then let cost_f id = fst @@ Id.Map.find cost_map (eg.@[find] id) in Some (E.cost cost_f node)
      else None in
    let make_pass enodes =
      let cost, node =
      Vector.to_iter enodes
      |> Iter.map (fun n -> (node_total_cost n, n))
      |> Iter.min_exn ~lt:(fun (c1, _) (c2, _) ->
          (match c1, c2 with
           | None, None -> 0
           | Some _, None -> -1
           | None, Some _ -> 1
           | Some c1, Some c2 -> E.compare c1 c2) = -1) in
      Option.map (fun cost -> (cost, node)) cost in
    let find_costs () =
      let any_changes = ref true in
      while !any_changes do
        any_changes := false;
        Fun.flip Id.Map.iter eclasses (fun eclass enodes ->
          let pass = make_pass enodes in
          match Id.Map.find_opt cost_map eclass, pass with
          | None, Some nw -> Id.Map.replace cost_map eclass nw; any_changes := true
          | Some ((cold, _)), Some ((cnew, _) as nw)
            when E.compare cnew cold = -1 ->
            Id.Map.replace cost_map eclass nw; any_changes := true
          | _ -> ()
        )
      done in
    let rec extract eid =
      let eid = eg.@[find] eid in
      let enode = Id.Map.find cost_map eid |> snd in
      let head = L.op enode in
      let children = L.children enode in
      L.Mk (L.make head @@ List.map extract children) in
    find_costs ();
    fun result -> extract result

end

(* ** Graph *)
module MakeOps
    (L: LANGUAGE)
    (A: sig type t type data [@@deriving eq] end)
    (AM: sig
       val make: (Id.t L.shape, A.t, A.data, ro) egraph -> Id.t L.shape -> A.data
       val merge: A.t -> A.data -> A.data -> A.data
       val modify: (Id.t L.shape, A.t, A.data, rw) egraph -> Id.t -> unit
     end) =
struct

  open (MakeInt (L))

  module Rule = struct

    type rule_output =
      | Constant of L.op Query.t
      | Conditional of
          L.op Query.t *
          ((Id.t L.shape, A.t, A.data, rw) egraph -> eclass_id -> eclass_id StringMap.t -> bool)
      | Dynamic of
          ((Id.t L.shape, A.t, A.data, rw) egraph -> eclass_id -> eclass_id StringMap.t -> L.op Query.t option)

    type t = L.op Query.t * rule_output

    let make_constant ~from ~into = (from, Constant into)
    let make_conditional ~from ~into ~cond = (from, Conditional (into, cond))
    let make_dynamic ~from ~generator = (from, Dynamic generator)

  end

  let freeze (graph: (_, _, _, rw) egraph) = (graph:> (_, _, _, ro) egraph)

  (* Adds a node into the egraph, assuming that the cannonical version
     of the node is up to date in the hash cons or 
  *)
  let add_enode self (enode: Id.t L.shape) =
    let node = self.@[canonicalise] enode in
    let id = match Hashtbl.find_opt self.hash_cons enode with
      | None ->
        self.version <- self.version + 1;
        (* There are no nodes congruent to this node in the graph *)
        let id = self.@[new_class] in
        let cls = self.@[get_class_members] id in
        Vector.append_list cls @@ List.map (fun child ->
          (node, child)
        ) (L.children node);
        Hashtbl.replace self.hash_cons node id;
        let data = AM.make (freeze self) enode in
        self.@[set_data] id data;
        AM.modify self id;
        id
      | Some id -> id in
    Id.find self.uf id

  let rec add_node self (L.Mk op: L.t) : Id.t =
    add_enode self @@ L.map_children op (add_node self)

  let rec subst self pat env =
    match pat with
    | Query.V id -> StringMap.find id env
    | Q (sym, args) ->
      let enode = L.make sym (List.map (fun arg -> self.@[subst] arg env) args) in
      self.@[add_enode] enode

  let merge self a b =
    let (+=) va vb = Vector.append va vb in
    let a = Id.find self.uf a in
    let b = Id.find self.uf b in
    if Id.eq_id a b then ()
    else begin
      self.version <- self.version + 1;
      let a_data, b_data = self.@[get_data] a, self.@[get_data] b in
      assert (Id.eq_id a (Id.union self.uf a b));
      self.@[get_class_members] b += self.@[get_class_members] a;
      Vector.clear (self.@[get_class_members] a);
      self.@[set_data] b (AM.merge self.analysis a_data b_data);
      self.@[append_to_worklist] b;
    end

  let repair (self: (Id.t L.shape, 'b, 'c, rw) egraph) ecls_id =
    let (+=) va vb = Vector.append_iter va vb in
    let uses = self.@[get_class_members] ecls_id in
    let uses =
      let res = Vector.copy uses in
      Vector.clear uses;
      res in
    (* update canonical uses in hashcons *)
    Vector.to_iter uses (fun (p_node, p_eclass) ->
      Hashtbl.remove self.hash_cons p_node;
      let p_node = self.@[canonicalise] p_node in
      Hashtbl.replace self.hash_cons p_node (self.@[find] p_eclass)
    );
    let new_uses = Hashtbl.create 10  in
    Vector.to_iter uses (fun (p_node, p_eclass) ->
      let p_node = self.@[canonicalise] p_node in
      begin match Hashtbl.find_opt new_uses p_node with
      | None -> ()
      | Some nd -> self.@[merge] p_eclass nd
      end;
      Hashtbl.replace new_uses p_node (self.@[find] p_eclass)
    );
    (self.@[get_class_members] (self.@[find] ecls_id)) += (Hashtbl.to_iter new_uses);
    (* update eclass *)
    AM.modify self ecls_id;
    let uses = self.@[get_class_members] ecls_id in        
    Vector.to_iter uses (fun (p_node, p_eclass) ->
      let p_eclass_data = self.@[get_data] p_eclass in
      let new_data = AM.merge self.analysis p_eclass_data (AM.make (freeze self) p_node) in
      if not @@ A.equal_data p_eclass_data new_data
      then begin
        self.@[set_data] p_eclass new_data;
        self.@[append_to_worklist] p_eclass
      end
    )

  let rebuild (self: (Id.t L.shape, 'b, 'c, rw) egraph) =
    while not @@ Vector.is_empty self.worklist do
      let worklist = Id.Set.of_iter (Vector.to_iter self.worklist |> Iter.map (self.@[find])) in
      Vector.clear self.worklist;
      Id.Set.to_iter worklist (fun ecls_id ->
        self.@[repair] ecls_id
      )
    done

  (* ** Matching *)
  let ematch eg (classes: (Id.t L.shape, 'a) Vector.t Id.Map.t) pattern =
    let rec enode_matches p enode env =
      match[@warning "-8"] p with
      | Query.Q (f, _) when not @@ L.equal_op f (L.op enode) ->
        None
      | Q (_, args) ->
        (fun f -> List.iter2 (Fun.curry f) args (L.children enode))
        |> Iter.fold_while (fun env (qvar, trm) ->
          match env with
          | None -> None, `Stop
          | Some env ->
            match match_in qvar trm env with
            | Some _ as res -> res, `Continue
            | None -> None, `Stop
        ) (Some env)
    and match_in p eid env =
      let eid = find eg eid in
      match p with
      | V id -> begin
          match StringMap.find_opt id env with
          | None -> Some (StringMap.add id eid env)
          | Some eid' when Id.eq_id eid eid' -> Some env
          | _ -> None
        end
      | p ->
        Vector.to_iter (Id.Map.find classes eid)
        |> Iter.find_map (fun enode -> enode_matches p enode env) in
    (fun f -> Id.Map.iter (Fun.curry f) classes)
    |> Iter.filter_map (fun (eid,_) ->
      match match_in pattern eid StringMap.empty with
      | Some env -> Some ((eid, env))
      | _ -> None
    )

  (* ** Rewriting System *)
  let apply_rules (eg: (Id.t L.shape, _, _, _) egraph) (rules : Rule.t list) =
    let eclasses = eclasses eg in
    let find_matches (from_rule, to_rule) =
      ematch eg eclasses from_rule |> Iter.map (lappend_pair to_rule) in
    let for_each_match = Iter.of_list rules |> Iter.flat_map find_matches in
    for_each_match begin fun (to_rule, eid, env) ->
      match to_rule with
      | Constant to_rule ->
        let new_eid = subst eg to_rule env in
        merge eg eid new_eid
      | Conditional (to_rule, cond) ->
        if cond eg eid env then
          let new_eid = subst eg to_rule env in
          merge eg eid new_eid
        else ()
      | Dynamic cond ->
        match cond eg eid env with
        | None -> ()
        | Some to_rule ->
          let new_eid = subst eg to_rule env in
          merge eg eid new_eid
    end;
    rebuild eg

  let run_until_saturation ?node_limit ?fuel ?until eg rules =
    match fuel, node_limit, until with
    | None, None, None ->
      let rec loop last_version =
        apply_rules eg rules;
        if not @@ Int.equal eg.version last_version
        then loop eg.version
        else ()  in
      loop eg.version; true
    | None, None, Some pred ->
      let rec loop last_version =
        apply_rules eg rules;
        if not @@ Int.equal eg.version last_version
        then if pred eg then false else loop eg.version
        else true in
      loop eg.version
    | None, Some node_limit, None ->
      let rec loop last_version =
        apply_rules eg rules;
        if not @@ Int.equal eg.version last_version
        then if Hashtbl.length eg.hash_cons < node_limit
          then loop eg.version
          else false
        else true  in
      loop eg.version
    | None, Some node_limit, Some pred ->
      let rec loop last_version =
        apply_rules eg rules;
        if not @@ Int.equal eg.version last_version
        then if Hashtbl.length eg.hash_cons < node_limit
          then if pred eg then false else loop eg.version
          else false
        else false  in
      loop eg.version
    | Some fuel, None, None ->
      let rec loop fuel last_version =
        apply_rules eg rules;
        if not @@ Int.equal eg.version last_version
        then if fuel > 0
          then loop (fuel - 1) eg.version
          else false
        else true  in
      loop fuel eg.version
    | Some fuel, None, Some pred ->
      let rec loop fuel last_version =
        apply_rules eg rules;
        if not @@ Int.equal eg.version last_version
        then if fuel > 0
          then if pred eg then false else loop (fuel - 1) eg.version
          else false
        else true  in
      loop fuel eg.version
    | Some fuel, Some node_limit, None ->
      let rec loop fuel last_version =
        apply_rules eg rules;
        if not @@ Int.equal eg.version last_version
        then if fuel > 0 && Hashtbl.length eg.hash_cons < node_limit
          then loop (fuel - 1) eg.version
          else false
        else true in
      loop fuel eg.version      
    | Some fuel, Some node_limit, Some pred ->
      let rec loop fuel last_version =
        apply_rules eg rules;
        if not @@ Int.equal eg.version last_version
        then if fuel > 0 && Hashtbl.length eg.hash_cons < node_limit
          then if pred eg then false else loop (fuel - 1) eg.version
          else false
        else true in
      loop fuel eg.version

  let iter_children self cls =
    Id.Map.find (eclasses self) (self.@[find] cls) |> Vector.to_iter


end



module Make
    (L: LANGUAGE)
    (A: sig type t type data [@@deriving show, eq] end)
    (MakeAnalysisOps: functor
       (S: GRAPH_API
        with type 'p t = (Id.t L.shape, A.t, A.data, 'p) egraph
         and type analysis := A.t
         and type data := A.data
         and type 'a shape := 'a L.shape
         and type node := L.t) -> sig
       val make: (Id.t L.shape, A.t, A.data, ro) egraph -> Id.t L.shape -> A.data
       val merge: A.t -> A.data -> A.data -> A.data
       val modify: (Id.t L.shape, A.t, A.data, rw) egraph -> Id.t -> unit
     end)
= struct


  module rec EGraph : sig
    type 'p t = (Id.t L.shape, A.t, A.data, 'p) egraph

    module Rule: sig
      type t
      val make_constant : from:L.op query -> into:L.op query -> t
      val make_conditional :
        from:L.op query ->
        into:L.op query ->
        cond:((Id.t L.shape, A.t, A.data, rw) egraph -> eclass_id -> eclass_id StringMap.t -> bool) ->
        t

      val make_dynamic :
        from:L.op query ->
        generator:((Id.t L.shape, A.t, A.data, rw) egraph ->
                   eclass_id -> eclass_id StringMap.t -> L.op query option) ->
        t

    end

    val freeze : rw t -> ro t
    val init : A.t -> 'p t
    val class_equal: ro t -> eclass_id -> eclass_id -> bool
    val new_class : rw t -> eclass_id
    val set_data : rw t -> eclass_id -> A.data -> unit
    val get_data : _ t -> eclass_id -> A.data
    val get_analysis : rw t -> A.t
    val canonicalise : rw t -> Id.t L.shape -> Id.t L.shape
    val find : ro t -> eclass_id -> eclass_id
    val append_to_worklist : rw t -> eclass_id -> unit
    val eclasses: rw t -> (Id.t L.shape, Vector.rw) Vector.t Id.Map.t
    val pp : Format.formatter -> (Id.t L.shape, 'a, A.data, _) egraph -> unit
    val to_dot : (Id.t L.shape, A.t, A.data, _) egraph -> Odot.graph
    val pp_dot : Format.formatter -> (Id.t L.shape, A.t, A.data, _) egraph -> unit
    val add_node : rw t -> L.t -> eclass_id
    val merge : rw t -> eclass_id -> eclass_id -> unit
    val iter_children : ro t -> eclass_id -> Id.t L.shape Iter.t
    val rebuild : rw t -> unit
    val apply_rules : (Id.t L.shape, A.t, A.data, rw) egraph -> Rule.t list -> unit
    val run_until_saturation:
      ?node_limit:int -> ?fuel:int -> ?until:((Id.t L.shape, A.t, A.data, rw) egraph -> bool) -> (Id.t L.shape, A.t, A.data, rw) egraph -> Rule.t list -> bool
  end
  = struct
    type 'p t = (Id.t L.shape, A.t, A.data, 'p) egraph
    include (MakeInt (L))
    include (MakePrinter (L) (A))
    include (MakeOps (L) (A) (Analysis))
  end
  and Analysis : sig
    val make: (Id.t L.shape, A.t, A.data, ro) egraph -> Id.t L.shape -> A.data
    val merge: A.t -> A.data -> A.data -> A.data
    val modify: (Id.t L.shape, A.t, A.data, rw) egraph -> Id.t -> unit
  end = MakeAnalysisOps (EGraph)

  include EGraph

end
