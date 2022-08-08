(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2022 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: https://grew.fr                                                    *)
(*    License: CeCILL (see LICENSE folder or "http://cecill.info/")               *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open Printf

module String_set = Set.Make (String)

module String_map = Map.Make (String)

module String_opt_map = Map.Make (struct type t = string option let compare = compare end)

module Int_set = Set.Make (struct type t = int let compare = Stdlib.compare end)

module Int_map = Map.Make (struct type t = int let compare = Stdlib.compare end)

let to_uname = function
  | "cat" -> "upos"
  | "pos" -> "xpos"
  | "phon" -> "form"
  | x -> x

type cmp = Eq | Neq
let string_of_cmp = function Eq -> "=" | Neq -> "<>"
let cmp_fct cmp = match cmp with Eq -> (=) | Neq -> (<>)

(* ================================================================================ *)
module Clustered = struct

  type 'a t = 
    | Leaf of 'a
    | Node of 'a t String_opt_map.t

  let empty = Node String_opt_map.empty


  (* nbre of element *)
  let rec size = function
    | Leaf _ -> 1
    | Node map -> String_opt_map.fold (fun _ t acc -> acc + (size t)) map 0

  let rec update fct path def t =
    match (path, t) with
    | ([], Leaf i) -> Leaf (fct i)
    | (value::tail, Node node) ->
      let sub = 
        match (tail, String_opt_map.find_opt value node) with
        | (_,Some t') -> t'
        | ([],None) -> Leaf (fct def)
        | (_,None) -> Node String_opt_map.empty in
      Node (String_opt_map.add value (update fct tail def sub) node)
    | _ -> failwith "[Clustered.update] inconsistent path"

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
    | _ -> failwith "[Clustered.insert] inconsistent data"

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
    | _ -> failwith "[Clustered.prune_unambiguous] no enough depth in the projection"

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
      | _ -> failwith "[Clustered.to_json] inconsistent data" in
    `List (loop [] keys [] t)

  let fold fct t init =
    let rec loop acc path = function
    | Leaf l -> fct acc path l
    | Node som ->
        String_opt_map.fold 
          (fun k v acc2 -> 
            loop acc2 (path @ [k]) v
          ) som acc in
    loop init [] t 
    

  (* nbre of occurrences *)
  let rec cardinal = function
  | Leaf n -> n
  | Node map -> String_opt_map.fold (fun _ t acc -> acc + (cardinal t)) map 0

  (* val fold: ('b -> string option list -> 'a -> 'b) -> 'a t -> 'b -> 'b *)

  let to_json keys t = 
    let data = 
      fold 
      (fun acc value_opt_list freq ->
        `Assoc [
          ("feats", `Assoc (List.map2 (fun key value_opt -> (key, CCOption.map_or ~default:`Null (fun x -> `String x) value_opt)) keys value_opt_list));
          ("freq", `Int freq)
        ] :: acc
      ) t [] in
    `List data







  (* let to_json keys t = 
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
    `List (loop [] keys [] t) *)




end (* module Clustered *)
