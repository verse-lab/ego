open Containers
module StringSet = Set.Make(String)
module StringMap = Stdlib.Map.Make(String)

let str p v = Format.to_string p v

type sexp = Sexplib.Sexp.t = Atom of string | List of sexp list

type rw = [`RW]
type ro = [`RO]


module type LANGUAGE = sig
  type 'a shape
  type op

  type t = Mk of t shape [@@unboxed]

  val equal_op: op -> op -> bool

  val pp_shape: (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a shape -> unit
  val compare_shape: ('a -> 'a -> int) -> 'a shape -> 'a shape -> int
  val op: 'a shape -> op
  val children: 'a shape -> 'a list
  val map_children: 'a shape -> ('a -> 'b) -> 'b shape
  val make : op -> 'a list -> 'a shape
end

module type ANALYSIS = sig
  type t
  type data [@@deriving show, eq]
  val default: data
end

module type ANALYSIS_OPS = sig
  type 'a t 
  type analysis
  type node
  type data
  val make : ro t -> node -> data
  val merge : analysis -> data -> data -> data * (bool * bool)
  val modify : rw t -> Id.t -> unit
end


module type COST = sig
  type t
  type node
  val compare : t -> t -> int
  val cost : (Id.t -> t) -> node -> t
end

module type GRAPH_API = sig
  type 'p t

  type analysis
  type data
  type node
  type 'a shape

  val freeze : rw t -> ro t
  val class_equal : ro t -> Id.t -> Id.t -> bool
  val iter_children : ro t -> Id.t -> Id.t shape Iter.t
  val set_data : rw t -> Id.t -> data -> unit
  val get_data : ro t -> Id.t -> data
  val get_analysis : rw t -> analysis
  val add_node : rw t -> node -> Id.t
  val merge : rw t -> Id.t -> Id.t -> unit
end

module type RULE = sig
  type t
  type query
  type 'a egraph

  val make_constant : from:query -> into:query -> t
  val make_conditional :
    from:query ->
    into:query ->
    cond:(rw egraph -> Id.t -> Id.t StringMap.t -> bool) ->
    t

  val make_dynamic :
    from:query ->
    generator:(rw egraph -> Id.t -> Id.t StringMap.t -> query option) -> t

end

module type SCHEDULER = sig

  type 'a egraph

  type t

  type data

  type rule

  val default : unit -> t

  val should_stop: t -> int -> data Iter.t -> bool

  val create_rule_metadata: t -> rule -> data

  val guard_rule_usage:
    rw egraph -> t -> data -> int ->
    (unit -> (Id.t * Id.t StringMap.t) Iter.t) -> (Id.t * Id.t StringMap.t) Iter.t

end
