open Containers

type t = string * Id.t list

type op = string
let equal_op = String.equal
let pp_op = String.pp

let pp pp_children fmt = function
  | (sym, []) -> Format.pp_print_string fmt sym
  | (sym, children) ->
    let open Format in
    pp_print_string fmt "(";
    pp_open_hvbox fmt 1;
    pp_print_string fmt sym;
    pp_print_space fmt ();
    pp_print_list ~pp_sep:pp_print_space pp_children fmt children;
    pp_close_box fmt ();
    pp_print_string fmt ")"

let compare =
  Pair.compare String.compare
    (List.compare (fun id1 id2 ->
       Fun.uncurry Int.compare @@ Pair.map_same Id.repr (id1,id2)))

let op = fst
let children = snd
let map_children t f = Pair.map_snd (List.map f) t
let make = Pair.make

let show pp_children = Format.to_string (pp pp_children)
let rec of_sexp f : Sexplib0.Sexp.t -> t =
  function
  | Atom str -> (str, [])
  | List (Atom head :: tail) ->
    (head, List.to_iter tail |> Iter.map (of_sexp f) |> Iter.map f |> Iter.to_list)
  | _ -> failwith "invalid sexp structure"

