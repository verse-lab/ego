type t = private int
type store
val eq_id : t -> t -> bool
val repr : t -> int
val hash : t -> int

val create_store : unit -> store
val make : store -> unit -> t
val find : store -> t -> t
val equal : store -> t -> t -> bool
val union : store -> t -> t -> t

module Map : Hashtbl.S with type key = t
module Set : CCHashSet.S with type elt = t
module OrderedSet : Ordered_set.S with type elt = t
