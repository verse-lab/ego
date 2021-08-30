module type S = sig
  type t
  type elt
  val create : unit -> t
  val push : elt -> t -> unit
  val pop : t -> elt
  val pop_opt : t -> elt option
  val append : t -> elt list -> unit
  val clear : t -> unit
  val copy : t -> t
  val is_empty : t -> bool
  val length : t -> int
  val iter : (elt -> unit) -> t -> unit
  val fold : ('a -> elt -> 'a) -> 'a -> t -> 'a
end  

module Make:
  functor (Elt : Containers.Hashtbl.HashedType) -> S with type elt = Elt.t
