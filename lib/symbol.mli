open Containers
type t = private int
val repr : t -> int
val intern : string -> t
val pp : Format.formatter -> t -> unit
val to_string: t -> string

module SymbolMap : Map.S with type key = t
