open Containers
open Language

module Backoff = struct

  type t = {match_limit: int; ban_length: int}

  type data = {
    mutable times_applied: int;
    mutable banned_until: int;
    mutable times_banned: int;
    mutable match_limit: int;
    mutable ban_length: int;
  }

  let with_params ~match_limit ~ban_length = {match_limit; ban_length}

  let default () : t = {
    match_limit = 1_000;
    ban_length = 5;
  }

  let create_rule_metadata ({match_limit; ban_length}: t) _ = {
    times_applied = 0;
    banned_until = 0;
    times_banned = 0;
    match_limit;
    ban_length;
  }

  let should_stop _ iteration stats =
    let banned = stats
                 |> Iter.filter (fun data -> data.banned_until > iteration)
                 |> Iter.to_array in

    if Array.length banned = 0
    then true
    else begin
      let min_ban =
        Iter.of_array banned
        |> Iter.map (fun data -> data.banned_until)
        |> Iter.min_exn ~lt:Int.(<) in
      let delta = min_ban - iteration in

      Iter.of_array banned
      |> Iter.iter (fun data -> data.banned_until <- data.banned_until - delta) ;

      false
    end


  let guard_rule_usage _ (_ : t) (data: data) iteration
        (gen_matches: (unit -> (Id.t * Id.t StringMap.t) Iter.t)) :
    (Id.t * Id.t StringMap.t) Iter.t =
    if iteration < data.banned_until
    then Iter.empty
    else begin
      let elts = Iter.to_array (gen_matches ()) in
      let total_len = Array.length elts in
      let threshold = data.match_limit lsl data.times_banned in
      if total_len > threshold
      then begin
        let ban_length = data.ban_length lsl data.times_banned in
        data.times_banned <- data.times_banned + 1;
        data.banned_until <- iteration + ban_length;
        Iter.empty
      end
      else begin
        data.times_applied <- data.times_applied + 1;
        Iter.of_array elts
      end

    end

end

module Simple = struct


  type t = unit

  type data = unit

  let init () : t = ()

  let create_rule_metadata _ _ = ()

  let should_stop _ _iteration _stats = true

  let guard_rule_usage _ (_ : t) ((): data) _iteration
        (gen_matches: (unit -> (Id.t * Id.t StringMap.t) Iter.t)) : (Id.t * Id.t StringMap.t) Iter.t =
    gen_matches ()

end
