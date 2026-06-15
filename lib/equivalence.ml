open Containers
(* module IntMap = Map.Make (Int) *)
module IntMap = Hashtbl.Make (Int)
module IntSet = CCHashSet.Make (Int)

module Make  = functor () -> struct

  type parent = Parent of idx [@@unboxed]
  and idx = int
  and store = parent Vector.vector

  type t = idx

  let repr v = v

  let (.@[]) = Vector.get
  let (.@[]<-) = Vector.set

  let create_store () = Vector.create ()

  let hash = Int.hash

  let make (store: store) () =
    let x = Vector.length store in
    Vector.push store (Parent x);
    x

  let rec find store x =
    let (Parent y) = store.@[x] in
    if y = x then x
    else begin
      let z = find store y in
      if y <> z then store.@[x] <- Parent z;
      z
    end

  let equal store t1 t2 =
    let t1 = find store t1 in
    let t2 = find store t2 in
    Int.equal t1 t2

  let link store x y =
    if x <> y then store.@[y] <- Parent x;
    x

  let union store x y =
    let x = find store x in
    let y = find store y in
    link store x y

module Map = IntMap

module Set = IntSet

end



