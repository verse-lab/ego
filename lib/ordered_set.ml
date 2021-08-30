open Containers

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

module Make (Elt: Hashtbl.HashedType) : S with type elt = Elt.t = struct

  module Set = CCHashSet.Make(Elt)

  type t = {
    elts: Elt.t Queue.t;
    cache: Set.t
  }

  type elt = Elt.t

  let create () = {elts=Queue.create (); cache=Set.create 1}
  let push vl st =
    if Set.mem st.cache vl
    then ()
    else (Queue.push vl st.elts; Set.insert st.cache vl)

  let pop  st =
    let hd = Queue.pop st.elts in
    Set.remove st.cache hd;
    hd

  let pop_opt  st =
    match Queue.peek_opt st.elts with
    | None -> None
    | Some hd ->
      ignore @@ Queue.pop st.elts;
      Set.remove st.cache hd;
      Some hd

  let append st elts = List.iter (fun elt -> push elt st) elts

  let clear st = Queue.clear st.elts; Set.clear st.cache

  let copy st = {elts=Queue.copy st.elts; cache=Set.copy st.cache}

  let is_empty st = Queue.is_empty st.elts

  let length st = Queue.length st.elts

  let iter f st = Queue.iter f st.elts

  let fold f acc st = Queue.fold f acc st.elts

end
