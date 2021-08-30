open Containers

type eclass_id = Id.t
type enode = Symbol.t * eclass_id list

let str p v = Format.to_string p v

(* ** ID *)
module EClassId = struct
  type t = eclass_id
  let pp fmt id =
    Format.pp_print_string fmt @@ Printf.sprintf "e%d" (Id.repr id)
  let show = str pp

  let compare (a:t) (b: t) =
    Int.compare (a :> int) (b :> int)

  let%test "IDs print correctly" =
    let store = Id.create_store () in
    Alcotest.(check string)
      "should pretty print as e0"
      "e0" (str pp (Id.make store ()))

end


(* ** Node *)
module ENode = struct

  type t = enode

  let children (_, children) = children

  let canonicalise uf (sym, children) =
    (sym, List.map (Id.find uf) children)

  let hash : enode Hash.t = Hash.(pair poly (list Id.hash))

  let%test "node hashes correctly" =
    let store = Id.create_store () in
    let i1 = Id.make store () in
    Alcotest.(check int)
      "hash values should match"
      (hash (Symbol.intern "example", [i1]))
      (hash (Symbol.intern "example", [i1]))

  let%test "node hashes correctly after union" =
    let store = Id.create_store () in
    let i1 = Id.make store () in
    let i2 = Id.make store () in
    let hash_1 = hash (Symbol.intern "example", [i1]) in
    ignore @@ Id.union store i1 i2;
    let hash_2 = hash (Symbol.intern "example", [i1]) in
    Alcotest.(check int)
      "hash values should match"
      hash_1
      hash_2

  let equal : enode Equal.t = Equal.(pair poly (list Id.eq_id))

  let pp ?(pp_id=EClassId.pp) fmt (sym, children) =
    match children with
    | [] -> Symbol.pp fmt sym
    | children ->
      let open Format in
      pp_print_string fmt "(";
      pp_open_hvbox fmt 1;
      Symbol.pp fmt sym;
      pp_print_space fmt ();
      pp_print_list ~pp_sep:(pp_print_space) pp_id fmt children;
      pp_close_box fmt ();
      pp_print_string fmt ")"

  let%test "leaf nodes prints correctly" =
    Alcotest.(check string)
      "should pretty print as sexp"
      "example"
      (str (pp ~pp_id:EClassId.pp)
                 (Symbol.intern "example", []))

  let%test "node prints correctly" =
    let store = Id.create_store () in
    Alcotest.(check string)
      "should pretty print as sexp"
      "(example e0 e1 e2)"
      (str (pp ~pp_id:EClassId.pp)
                 (Symbol.intern "example",
                  List.init 3 (fun _ -> Id.make store ()))
              )
  module Set = Set.Make (struct
      type t = enode
      let compare n1 n2 = Int.compare (hash n1) (hash n2)
    end)

end
