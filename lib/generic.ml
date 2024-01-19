open [@warning "-33"] Containers
open Language
open Types
module Id = Id

let dedup cmp vec =
  let prev = ref None in
  Vector.filter_in_place (fun elt ->
    match !prev with
    | None -> prev := Some elt; true
    | Some last_value ->
      if Int.equal (cmp last_value elt) 0
      then false
      else begin
        prev := Some elt;
        true
      end
  ) vec


(* let lappend_pair a (b,c) = (a,b,c) *)
type 'a query = 'a Query.t

type ('node, 'data) eclass = {
  mutable id: Id.t;
  nodes: 'node Vector.vector;
  mutable data: 'data;
  parents: ('node * Id.t) Vector.vector;
}

type ('node, 'analysis, 'data, 'permission) egraph = {
  mutable version: int;
  analysis: 'analysis;

  uf: Id.store;                                        (* tracks equivalence classes of
                                                          class ids  *)
  class_data:
    ('node, 'data) eclass Id.Map.t;       (* maps classes to the canonical nodes
                                                                     they contain, and any classes that are
                                                                     children of these nodes *)
  hash_cons: ('node, Id.t) Hashtbl.t;                  (* maps cannonical nodes to their
                                                          equivalence classes *)
  pending: ('node * Id.t) Vector.vector;

  pending_analysis: ('node * Id.t) Vector.vector;
}


module MakeInt (L: LANGUAGE) (* (A: ANALYSIS) *) = struct

  let (.@[]) self fn = fn self [@@inline always]

  (* *** Initialization *)
  let init analysis = {
    version=0;
    analysis;
    uf=Id.create_store ();
    class_data=Id.Map.create 10;
    hash_cons=Hashtbl.create 10;
    pending=Vector.create ();
    pending_analysis=Vector.create ();
  }

  (* *** Eclasses *)
  let get_analysis self = self.analysis

  let get_class_data self id =
    match Id.Map.find_opt self.class_data id with
    | Some data -> data
    | None -> failwith @@ Printf.sprintf "attempted to set the data of an unbound class %s " (EClassId.show id)

  let remove_class_data self id =
    match Id.Map.find_opt self.class_data id with
    | Some classes -> Id.Map.remove self.class_data id; Some classes
    | None -> None

  let set_data self id data =
    match Id.Map.find_opt self.class_data id with
    | None -> failwith @@ Printf.sprintf "attempted to set the data of an unbound class %s " (EClassId.show id)
    | Some class_data -> class_data.data <- data

  let get_data self id =
    match Id.Map.find_opt self.class_data (Id.find self.uf id) with
    | None -> failwith @@ Printf.sprintf "attempted to get the data of an unbound class %s " (EClassId.show id)
    | Some class_data -> class_data.data

  let canonicalise self node = L.map_children node (Id.find self.uf)

  let find self vl = Id.find self.uf vl

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

module MakePrinter (L: LANGUAGE) (A: ANALYSIS) = struct

  open (MakeInt(L))

  (* **** Export as dot *)
  let to_dot self =
    let eclasses = eclasses self in

    let pp_node_by_id fmt id =
      let pp_node_by_id fmt id =
        let id = self.@[find] id in
        begin
          let vls = Id.Map.find_opt eclasses id |> Option.get_lazy Vector.create in
          let open Format in
          pp_print_string fmt "{";
          pp_open_hovbox fmt 1;
          Vector.pp
            ~pp_sep:(fun fmt () -> pp_print_string fmt ","; pp_print_space fmt ())
            (L.pp_shape EClassId.pp) fmt vls;
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
    (A: ANALYSIS)
    (AM: sig
       val make: (Id.t L.shape, A.t, A.data, ro) egraph -> Id.t L.shape -> A.data
       val merge: A.t -> A.data -> A.data -> A.data * (bool * bool)
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

  let new_class self =
    let id = Id.make self.uf () in
    Id.Map.add self.class_data id {id; nodes=Vector.create (); data=A.default; parents=Vector.create ()};
    id

  let freeze (graph: (_, _, _, rw) egraph) = (graph:> (_, _, _, ro) egraph)

  (* Adds a node into the egraph, assuming that the cannonical version
     of the node is up to date in the hash cons or 
  *)
  let add_enode self (node: Id.t L.shape) =
    let node = self.@[canonicalise] node in
    let id = match Hashtbl.find_opt self.hash_cons node with
      | None ->
        self.version <- self.version + 1;
        let id = Id.make self.uf () in
        let cls = {
          id;
          nodes=Vector.of_list [node];
          data = AM.make (freeze self) node;
          parents=Vector.create ()
        } in
        
        List.iter (fun child ->
          let tup = (node, id) in
          Vector.push ((self.@[get_class_data] child).parents) tup
        ) (L.children node);

        Vector.push self.pending (node,id);

        Id.Map.add self.class_data id cls;

        Hashtbl.add self.hash_cons node id;

        AM.modify self id;
        id
      | Some id -> self.@[find] id in
    Id.find self.uf id

  let rec add_node self (L.Mk op: L.t) : Id.t =
    add_enode self @@ L.map_children op (add_node self)

  let rec subst self pat env =
    match pat with
    | Query.V id -> StringMap.find id env
    | Q (sym, args) ->
      let enode = L.make sym (List.map (fun arg -> self.@[subst] arg env) args) in
      self.@[add_enode] enode

  let merge self id1 id2 =
    let (+=) va vb = Vector.append va vb in
    let id1 = Id.find self.uf id1 in
    let id2 = Id.find self.uf id2 in
    if Id.eq_id id1 id2 then ()
    else begin
      self.version <- self.version + 1;
      (* cls2 has fewer children *)
      let id1, id2 =
        if Vector.length (self.@[get_class_data] id1).parents < Vector.length (self.@[get_class_data] id2).parents
        then (id2, id1)
        else (id1, id2) in

      (* make cls1 the new root *)
      assert (Id.eq_id id1 (Id.union self.uf id1 id2));

      let cls2 = self.@[remove_class_data] id2
                 |> Option.get_exn_or "Invariant violation" in
      let cls1 = self.@[get_class_data] id1 in
      assert (Id.eq_id id1 cls1.id);

      self.pending += cls2.parents;

      let (did_update_cls1, did_update_cls2) =
        let data, res = (AM.merge self.analysis cls1.data cls2.data) in
        cls1.data <- data;
        res in

      if did_update_cls1 then self.pending_analysis += cls1.parents;
      if did_update_cls2 then self.pending_analysis += cls2.parents;

      cls1.nodes += cls2.nodes;
      cls1.parents += cls2.parents;
      AM.modify self id1
    end

  let rebuild_classes self =
    Id.Map.to_seq_values self.class_data |> Seq.iter (fun cls ->
      Vector.map_in_place (fun node -> self.@[canonicalise] node) cls.nodes;
      Vector.sort' (L.compare_shape EClassId.compare) cls.nodes;
      dedup (L.compare_shape EClassId.compare) cls.nodes
    )

  let process_unions self =
    (* let init_size = Hashtbl.length self.hash_cons in     *)
    while not @@ Vector.is_empty self.pending do

      let rec update_hash_cons () =
        match Vector.pop self.pending with
        | None -> ()
        | Some (node,cls) ->
          let old_node = node in
          let node = self.@[canonicalise] node in
          if not @@ ((L.compare_shape EClassId.compare old_node node) = 0) then
            Hashtbl.remove self.hash_cons old_node;
          begin match (Hashtbl.find_opt self.hash_cons node) with
          | None -> Hashtbl.add self.hash_cons node cls
          | Some memo_cls -> self.@[merge] memo_cls cls
          end;
          update_hash_cons () in
      update_hash_cons ();

      let rec update_analysis () =
        match Vector.pop self.pending_analysis with
        | None -> ()
        | Some (node, class_id) ->
          let class_id = self.@[find] class_id in
          let node_data = AM.make (freeze self) node in
          let cls = self.@[get_class_data] class_id in
          assert (Id.eq_id cls.id class_id);
          let (did_update_left, _did_update_right) =
            let data,res = AM.merge self.analysis cls.data node_data in
            cls.data <- data;
            res in
          if did_update_left then begin
            Vector.append self.pending_analysis cls.parents;
            AM.modify self class_id
          end;
          update_analysis () in
      update_analysis ()
    done
    (* let _final_size = Hashtbl.length self.hash_cons in
     * print_endline @@ Printf.sprintf "after rebuilding size of nodes is %d => %d"  init_size final_size *)

  let rebuild (self: (Id.t L.shape, 'b, 'c, rw) egraph) =
    process_unions self;
    rebuild_classes self

  (* ** Matching *)
  let ematch eg (classes: (Id.t L.shape, 'a) Vector.t Id.Map.t) pattern =
    let concat_map f l = Iter.concat (Iter.map f l) in
    let rec enode_matches p enode env =
      match[@warning "-8"] p with
      | Query.Q (f, _) when not @@ L.equal_op f (L.op enode) ->
        Iter.empty
      | Q (_, args) ->
        (fun f -> List.iter2 (Fun.curry f) args (L.children enode))
        |> Iter.fold (fun envs (qvar, trm) ->
          concat_map (fun env' -> match_in qvar trm env') envs) (Iter.singleton env)
    and match_in p eid env =
      let eid = find eg eid in
      match p with
      | V id -> begin
          match StringMap.find_opt id env with
          | None -> Iter.singleton (StringMap.add id eid env)
          | Some eid' when Id.eq_id eid eid' -> Iter.singleton env
          | _ -> Iter.empty
        end
      | p ->
        match Id.Map.find_opt classes eid with
        | Some v -> Vector.to_iter v |> concat_map (fun enode -> enode_matches p enode env)
        | None -> Iter.empty
      in
    (fun f -> Id.Map.iter (Fun.curry f) classes)
    |> concat_map (fun (eid, _) ->
       Iter.map (fun s -> (eid, s)) (match_in pattern eid StringMap.empty))

  let find_matches eg =
    let eclasses = eclasses eg in
    fun rule ->  ematch eg eclasses rule

  let iter_children self cls =
    (* let old_cls = cls in *)
    let cls = (self.@[find] cls) in
    Id.Map.find_opt (eclasses self) cls |> Option.map Vector.to_iter |> Option.get_or ~default:Iter.empty

  module BuildRunner (S : SCHEDULER with type 'a egraph := (Id.t L.shape, A.t, A.data, rw) egraph
                                     and type rule := Rule.t) = struct

    (* ** Rewriting System *)
    let apply_rules scheduler iteration (eg: (Id.t L.shape, _, _, _) egraph) (rules : (Rule.t * S.data) array) =
      let find_matches = find_matches eg in
      let for_each_match =
        Iter.of_array rules
        |> Iter.flat_map (fun ((from_rule, to_rule), meta_data) ->
          S.guard_rule_usage eg scheduler meta_data iteration (fun () -> find_matches from_rule)
          |> Iter.map (fun (eid,env) -> (to_rule, eid, env))
        ) in
      for_each_match begin fun (to_rule, eid, env) ->
        match to_rule with
        | Rule.Constant to_rule ->
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

    let run_until_saturation ?scheduler ?(node_limit=`Bounded 10_000) ?(fuel=`Bounded 30) ?until eg rules =
      let scheduler = match scheduler with None -> S.default () | Some scheduler -> scheduler in
      let rules = Iter.of_list rules
                  |> Iter.map (fun rule -> (rule, S.create_rule_metadata scheduler rule))
                  |> Iter.to_array in
      let rule_data () = Array.to_iter rules |> Iter.map snd in
      match fuel, node_limit, until with
      | `Unbounded, `Unbounded, None ->
        let rec loop last_version ind =
          apply_rules scheduler ind eg rules;
          if not @@ Int.equal eg.version last_version
          then loop eg.version (ind + 1)
          else if S.should_stop scheduler ind (rule_data ()) then () else loop eg.version (ind + 1) in
        loop eg.version 0; true
      | `Unbounded, `Unbounded, Some pred ->
        let rec loop last_version ind =
          apply_rules scheduler ind eg rules;
          if not @@ Int.equal eg.version last_version
          then if pred eg then false else loop eg.version (ind + 1)
          else if S.should_stop scheduler ind (rule_data ()) then true else loop eg.version (ind + 1) in
        loop eg.version 0
      | `Unbounded, `Bounded node_limit, None ->
        let rec loop last_version ind =
          apply_rules scheduler ind eg rules;
          if not @@ Int.equal eg.version last_version
          then if Hashtbl.length eg.hash_cons < node_limit
            then loop eg.version (ind + 1)
            else false
          else if S.should_stop scheduler ind (rule_data ()) then true else loop eg.version (ind + 1)  in
        loop eg.version 0
      | `Unbounded, `Bounded node_limit, Some pred ->
        let rec loop last_version ind =
          apply_rules scheduler ind eg rules;
          if not @@ Int.equal eg.version last_version
          then if Hashtbl.length eg.hash_cons < node_limit
            then if pred eg then false else loop eg.version (ind + 1)
            else false
          else if S.should_stop scheduler ind (rule_data ()) then true else loop eg.version (ind + 1) in
        loop eg.version 0
      | `Bounded fuel, `Unbounded, None ->
        let rec loop last_version ind =
          apply_rules scheduler ind eg rules;
          if not @@ Int.equal eg.version last_version
          then if fuel > ind
            then loop eg.version (ind + 1)
            else false
          else if S.should_stop scheduler ind (rule_data ()) then true else loop eg.version (ind + 1) in
        loop eg.version 0
      | `Bounded fuel, `Unbounded, Some pred ->
        let rec loop last_version ind =
          apply_rules scheduler ind eg rules;
          if not @@ Int.equal eg.version last_version
          then if fuel > ind
            then if pred eg then false else loop eg.version (ind + 1)
            else false
          else if S.should_stop scheduler ind (rule_data ()) then true else loop eg.version (ind + 1)   in
        loop eg.version 0
      | `Bounded fuel, `Bounded node_limit, None ->
        let rec loop last_version ind =
          apply_rules scheduler ind eg rules;
          if not @@ Int.equal eg.version last_version
          then if fuel > ind && Hashtbl.length eg.hash_cons < node_limit
            then loop eg.version (ind + 1)
            else false
          else if S.should_stop scheduler ind (rule_data ()) then true else loop eg.version (ind + 1)  in
        loop eg.version 0
      | `Bounded fuel, `Bounded node_limit, Some pred ->
        let rec loop last_version ind =
          apply_rules scheduler ind eg rules;
          if not @@ Int.equal eg.version last_version
          then if fuel > ind && Hashtbl.length eg.hash_cons < node_limit
            then if pred eg then false else loop eg.version (ind + 1)
            else false
          else if S.should_stop scheduler ind (rule_data ()) then true else loop eg.version (ind + 1) in
        loop eg.version 0

  end

  include (BuildRunner (Scheduler.Backoff))

  let apply_rules (eg: (Id.t L.shape, _, _, _) egraph) (rules : Rule.t list) =
      let find_matches = find_matches eg in
      let for_each_match =
        Iter.of_list rules
        |> Iter.flat_map
             (fun (from_rule, to_rule) ->
                find_matches from_rule
                |> Iter.map (fun (eid,env) -> (to_rule, eid, env))
        ) in
      for_each_match begin fun (to_rule, eid, env) ->
        match to_rule with
        | Rule.Constant to_rule ->
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
end




module Make
    (L: LANGUAGE)
    (A: ANALYSIS)
    (MakeAnalysisOps: functor
       (S: GRAPH_API
        with type 'p t = (Id.t L.shape, A.t, A.data, 'p) egraph
         and type analysis := A.t
         and type data := A.data
         and type 'a shape := 'a L.shape
         and type node := L.t) -> sig
       val make: (Id.t L.shape, A.t, A.data, ro) egraph -> Id.t L.shape -> A.data
       val merge: A.t -> A.data -> A.data -> A.data * (bool * bool)
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
    (* val append_to_worklist : rw t -> eclass_id -> unit *)
    val eclasses: rw t -> (Id.t L.shape, Vector.rw) Vector.t Id.Map.t
    (* val pp : Format.formatter -> (Id.t L.shape, 'a, A.data, _) egraph -> unit *)
    val to_dot : (Id.t L.shape, A.t, A.data, _) egraph -> Odot.graph
    val pp_dot : Format.formatter -> (Id.t L.shape, A.t, A.data, _) egraph -> unit
    val add_node : rw t -> L.t -> eclass_id
    val merge : rw t -> eclass_id -> eclass_id -> unit
    val iter_children : ro t -> eclass_id -> Id.t L.shape Iter.t
    val rebuild : rw t -> unit

    val find_matches : ro t -> L.op query -> (eclass_id * eclass_id StringMap.t) Iter.t
    val apply_rules : (Id.t L.shape, A.t, A.data, rw) egraph -> Rule.t list -> unit
    val run_until_saturation:
      ?scheduler:Scheduler.Backoff.t ->
      ?node_limit:[`Bounded of int | `Unbounded] ->
      ?fuel:[`Bounded of int | `Unbounded] ->
      ?until:((Id.t L.shape, A.t, A.data, rw) egraph -> bool) -> (Id.t L.shape, A.t, A.data, rw) egraph -> Rule.t list -> bool

    module BuildRunner (S : SCHEDULER
                        with type 'a egraph := (Id.t L.shape, A.t, A.data, rw) egraph
                         and type rule := Rule.t) :
    sig 
      val apply_rules :
        S.t ->
        int ->
        (Id.t L.shape, A.t, A.data, rw) egraph ->
        (Rule.t * S.data) array -> unit
      val run_until_saturation :
        ?scheduler:S.t ->
        ?node_limit:[`Bounded of int | `Unbounded] ->
        ?fuel:[`Bounded of int | `Unbounded] ->
        ?until:((Id.t L.shape, A.t, A.data, rw) egraph -> bool) ->
        (Id.t L.shape, A.t, A.data, rw) egraph -> Rule.t list -> bool
    end
  end
  = struct
    let _unsafe = 10
    type 'p t = (Id.t L.shape, A.t, A.data, 'p) egraph
    include (MakeInt (L))
    include (MakePrinter (L) (A))
    include (MakeOps (L) (A) (Analysis))
  end
  and Analysis : sig
    val make: (Id.t L.shape, A.t, A.data, ro) egraph -> Id.t L.shape -> A.data
    val merge: A.t -> A.data -> A.data -> A.data * (bool * bool)
    val modify: (Id.t L.shape, A.t, A.data, rw) egraph -> Id.t -> unit
  end = MakeAnalysisOps (EGraph)

  include EGraph

end
