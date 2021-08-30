open Language

type ('node, 'analysis, 'data, 'permission) egraph

module MakePrinter : functor (L : LANGUAGE) (A : ANALYSIS) -> sig
  val pp : Format.formatter -> (Id.t L.shape, A.t, A.data, 'b) egraph -> unit
  val to_dot : (Id.t L.shape, A.t, A.data, 'b) egraph -> Odot.graph
  val pp_dot : Format.formatter -> (Id.t L.shape, A.t, A.data, 'b) egraph -> unit
end

module MakeExtractor : functor
  (L : LANGUAGE)
  (E : COST with type node := Id.t L.shape) -> sig
  val extract : (Id.t L.shape, 'a, 'b, rw) egraph -> Id.t -> L.t
end

module Make :
  functor
    (L : LANGUAGE)
    (A : ANALYSIS)
    (MakeAnalysisOps : functor
       (S : GRAPH_API with type 'p t = (Id.t L.shape, A.t, A.data, 'p) egraph
                       and type analysis := A.t
                       and type 'a shape := 'a L.shape
                       and type data := A.data
                       and type node := L.t) ->
       ANALYSIS_OPS with type  'p t = (Id.t L.shape, A.t, A.data, 'p) egraph
                     and type analysis := A.t
                     and type data := A.data
                     and type node := Id.t L.shape) ->
  sig
    type 'p t = (Id.t L.shape, A.t, A.data, 'p) egraph
    module Rule:
      RULE with type query := L.op Query.t
            and type 'a egraph := (Id.t L.shape, A.t, A.data, 'a) egraph

    val freeze : rw t -> ro t
    val init : A.t -> 'p t
    val class_equal: ro t -> Id.t -> Id.t -> bool
    val new_class : rw t -> Id.t
    val set_data : rw t -> Id.t -> A.data -> unit
    val get_data : _ t -> Id.t -> A.data
    val get_analysis: rw t -> A.t
    val canonicalise : rw t -> Id.t L.shape -> Id.t L.shape
    val find : ro t -> Id.t -> Id.t
    val append_to_worklist : rw t -> Id.t -> unit
    val eclasses: rw t -> (Id.t L.shape, Containers.Vector.rw) Containers.Vector.t Id.Map.t
    val iter_children : ro t -> Id.t -> Id.t L.shape Iter.t
    val pp : Format.formatter -> (Id.t L.shape, 'a, A.data, _) egraph -> unit
    val to_dot : (Id.t L.shape, A.t, A.data, _) egraph -> Odot.graph
    val pp_dot : Format.formatter -> (Id.t L.shape, A.t, A.data, _) egraph -> unit
    val add_node : rw t -> L.t -> Id.t
    val merge : rw t -> Id.t -> Id.t -> unit
    val rebuild : rw t -> unit
    val apply_rules : (Id.t L.shape, A.t, A.data, rw) egraph -> Rule.t list -> unit
    val run_until_saturation:
      ?node_limit:int -> ?fuel:int -> ?until:((Id.t L.shape, A.t, A.data, rw) egraph -> bool) -> (Id.t L.shape, A.t, A.data, rw) egraph -> Rule.t list -> bool

  end
