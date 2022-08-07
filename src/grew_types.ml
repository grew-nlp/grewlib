(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2021 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: https://grew.fr                                                     *)
(*    License: CeCILL (see LICENSE folder or "http://cecill.info/")            *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open Printf

open Grew_base


(* ================================================================================ *)
module Pid = struct
  (* type t = int *)
  type t = Ker of int | Ext of int

  let compare = Stdlib.compare

  let to_id = function
    | Ker i -> sprintf "p_%d" i
    | Ext i -> sprintf "n_%d" i

  let to_string = function
    | Ker i -> sprintf "Ker %d" i
    | Ext i -> sprintf "Ext %d" i
end (* module Pid *)

(* ================================================================================ *)
module Pid_set = Set.Make (Pid)

(* ================================================================================ *)
module Pid_map =
struct
  include Map.Make (Pid)

  exception True

  let exists fct map =
    try
      iter
        (fun key value ->
           if fct key value
           then raise True
        ) map;
      false
    with True -> true

  (* union of two maps*)
  let union_map m m' = fold (fun k v m'' -> (add k v m'')) m m'
end (* module Pid_map *)

(* ================================================================================ *)
module Gid = struct
  type t = int

  let compare = Stdlib.compare

  let to_string i = sprintf "%d" i
end (* module Gid *)

(* ================================================================================ *)
module Gid_map =  Map.Make (Gid)

(* ================================================================================ *)
module Gid_set = Set.Make (Gid)

(* ================================================================================ *)
module Massoc_gid = Massoc_make (Gid)

(* ================================================================================ *)
module Massoc_pid = Massoc_make (Pid)

(* ================================================================================ *)
module Massoc_string = Massoc_make (String)

(* ================================================================================ *)
module Projection = struct

  type t = 
    | Leaf of int
    | Node of t String_opt_map.t

  let empty = Node String_opt_map.empty

  (* nbre of occurrences *)
  let rec cardinal = function
    | Leaf n -> n
    | Node map -> String_opt_map.fold (fun _ t acc -> acc + (cardinal t)) map 0

  (* nbre of element *)
  let rec size = function
    | Leaf _ -> 1
    | Node map -> String_opt_map.fold (fun _ t acc -> acc + (size t)) map 0

  let rec insert data t =
    match (data, t) with
    | ([], Leaf i) -> Leaf (i+1)
    | (value::tail, Node map) ->
      let sub = 
        match (tail, String_opt_map.find_opt value map) with
        | (_,Some t') -> t'
        | ([],None) -> Leaf 0
        | (_,None) -> Node String_opt_map.empty in
      Node (String_opt_map.add value (insert tail sub) map)
    | _ -> Error.bug "[Projection.insert] inconsitent data"

  let rec prune_unambiguous depth t =
    match (depth, t) with
    | (1, Node map) ->
      Node (
        String_opt_map.fold
          (fun k v acc -> 
             if size v > 1 
             then String_opt_map.add k v acc
             else acc
          ) map String_opt_map.empty
      )
    | (n, Node map) -> Node (String_opt_map.map (fun v -> prune_unambiguous (depth - 1) v) map)
    | _ -> Error.bug "[Projection.prune_unambiguous] no enough depth in the projection"

  let to_json keys t = 
    let rec loop acc keys partial t =
      match (keys, t) with
      | ([], Leaf n) -> (`Assoc ["feats", (`Assoc (List.rev partial)); "freq", `Int n]) :: acc
      | (key :: tail, Node map) ->
        String_opt_map.fold
          (fun value sub_t acc2 ->
             let new_partial = (key, match value with Some s -> `String s | _ -> `Null) :: partial in
             loop acc2 tail new_partial sub_t
          ) map acc
      | _ -> Error.bug "[Projection.to_json] inconsistent data" in
    `List (loop [] keys [] t)
end