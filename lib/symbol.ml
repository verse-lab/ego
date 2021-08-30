open Containers

type t = int

module SymbolMap = Map.Make(Int)
module StrMap = Hashtbl.Make(String)

let repr v = v
let tbl = StrMap.create 10
let strs = CCVector.create ()

let intern str =
  match StrMap.find_opt tbl str with
  | Some id -> id
  | None ->
    let id = Vector.length strs in
    Vector.push strs str;
    StrMap.add tbl str id;
    id

let pp fmt s =
  Format.pp_print_string fmt (Vector.get strs s)

let to_string s = Vector.get strs s
