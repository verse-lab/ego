type egraph

module Symbol : sig
  type t = private int
  val intern : string -> t
  val to_string : t -> string
end

module Query : sig
  type t [@@deriving show]
  val of_sexp : Sexplib0.Sexp.t -> t
  val to_sexp : t -> Sexplib0.Sexp.t
end

module Rule : sig

  type t [@@deriving show]

  val make: from:Query.t -> into:Query.t -> t option

end

module EGraph : sig
  type t = egraph

  val pp : ?pp_id:(Format.formatter -> Id.t -> unit) -> Format.formatter -> t -> unit
  val pp_dot : Format.formatter -> t -> unit

  val init : unit -> t

  val add_sexp: t -> Sexplib.Sexp.t -> Id.t

  val to_dot : t -> Odot.graph

  val merge : t -> Id.t -> Id.t -> unit

  val rebuild : t -> unit

  val extract: ((Id.t -> float) -> (Symbol.t * Id.t list) -> float) -> t -> Id.t -> Sexplib0.Sexp.t

  val apply_rules : t -> Rule.t list -> unit

  val run_until_saturation: ?fuel:int -> t -> Rule.t list -> bool

end

