open Containers
open Language

type 'sym t =
  | V of string
  | Q of 'sym * 'sym t list

let rec of_sexp intern : sexp -> _ t = function
  | Atom str when String.prefix ~pre:"?" str -> V (String.drop 1 str)
  | Atom sym -> Q (intern sym, [])
  | List (Atom sym :: children) ->
    Q (intern sym, List.map (of_sexp intern) children)
  | _ -> invalid_arg "Query sexp not of the expected form"

let rec to_sexp to_string : _ t -> sexp = function
  | V str -> Atom ("?" ^ str)
  | Q (head, children) -> List (Atom (to_string head) :: List.map (to_sexp to_string) children)

let rec pp symbol_pp fmt = function
  | V sym -> Format.pp_print_string fmt ("?" ^ sym)
  | Q (sym, []) -> symbol_pp fmt sym
  | Q (sym, children) ->
    let open Format in
    pp_print_string fmt "(";
    pp_open_hvbox fmt 1;
    symbol_pp fmt sym;
    pp_print_space fmt ();
    pp_print_list ~pp_sep:pp_print_space (pp symbol_pp) fmt children;
    pp_close_box fmt ();
    pp_print_string fmt ")"

let show symbol_pp = str (pp symbol_pp)

let%test "terms are printed as expected" =
  Alcotest.(check string)
    "prints as expected"
    "(+ 1 ?a)" (str (pp Symbol.pp) (Q (Symbol.intern "+", [Q (Symbol.intern "1", []); V "a"])))

let variables query =
  let rec loop acc = 
    function
      V sym -> StringSet.add sym acc
    | Q (_, children) ->
      List.fold_left loop acc children in
  loop StringSet.empty query

