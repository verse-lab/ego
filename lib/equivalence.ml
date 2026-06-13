open Containers
(* module IntMap = Map.Make (Int) *)
module IntMap = Hashtbl.Make (Int)
module IntSet = CCHashSet.Make (Int)

module Make  = functor () -> struct

  type elem =
    | Root
    | Link of elem_ref
  and elem_ref = int
  and store = {
    mutable limit: int;
    content: elem IntMap.t
  } 

  type t = elem_ref

  let repr v = v

  let (.@[]) store rf = IntMap.find store.content rf
  let (.@[]<-) store rf vl = IntMap.replace store.content rf vl

  let create_store () = {limit=0; content=IntMap.create 100}

  let hash = Int.hash

  let make (store: store) () =
    let x = store.limit in
    store.limit <- x + 1;
    IntMap.replace store.content x Root;
    x

  let rec find store x =
    match store.@[x] with
    | Root -> x
    | Link y ->
      let z = find store y in
      if not @@ Equal.physical z y then
        store.@[x] <- Link z;
      z
  let equal store t1 t2 =
    let t1 = find store t1 in
    let t2 = find store t2 in
    Equal.physical t1 t2

  let link store x y =
    if Equal.physical x y then x
    else match[@warning "-8"] store.@[x], store.@[y] with
      | Root, Root -> store.@[y] <- Link x; x
        (* if vx < vy then (store.@[x] <- Link y; y)
         * else if vy > vx then (store.@[y] <- Link x; x)
         * else (store.@[y] <- Link x;
         *       store.@[x] <- make_raw ();
         *       x) *)

  let union store x y =
    let x = find store x in
    let y = find store y in
    link store x y

module Map = IntMap

module Set = IntSet

end



