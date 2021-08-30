open Containers

include (Equivalence.Make ())

let eq_id = (Equal.map repr Equal.int)

module OrderedSet = Ordered_set.Make (struct
    type nonrec t = t
    let equal = eq_id
    let hash = hash
  end)

