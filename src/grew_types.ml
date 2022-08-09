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


(* ================================================================================ *)
module Clustered = struct

type 'a t = 
  | Empty of 'a (* the `null` value *)
  | Leaf of 'a
  | Node of 'a t String_opt_map.t


let dump to_string t = 
  let rec loop indent = function 
  | Empty _ -> Printf.printf "__EMPTY__\n%!"
  | Leaf a -> Printf.printf "%s%s\n%!" (String.make indent ' ') (to_string a)
  | Node som -> 
    String_opt_map.iter
    (fun key a ->
      Printf.printf "%s%s\n%!" (String.make indent ' ') (CCOption.get_or ~default:"undefined" key);
      loop (indent+1) a
    ) som in
  loop 0 t

let empty null = Empty null

let fold_layer fct_leaf init fct_node closure t =
  let rec loop t =
    match t with
    | Empty _ -> closure init
    | Leaf l -> fct_leaf l
    | Node som -> closure (String_opt_map.fold (fun key sub acc -> fct_node key (loop sub) acc) som init) in
  loop t

  (* nbre of element *)
  let rec size = function
    | Empty _ -> 0
    | Leaf _ -> 1
    | Node map -> String_opt_map.fold (fun _ t acc -> acc + (size t)) map 0

  let rec update fct path def t =
    match (path, t) with
    | ([], Empty null) -> Leaf (fct null)
    | ([], Leaf i) -> Leaf (fct i)
    | (value::tail, Empty null) ->
      let sub = 
        match tail with
        | [] -> Leaf null
        | _ -> Node String_opt_map.empty in
      Node (String_opt_map.add value (update fct tail def sub) String_opt_map.empty)
    | (value::tail, Node node) ->
      let sub = 
        match (tail, String_opt_map.find_opt value node) with
        | (_, Some t') -> t'
        | ([], None) -> Leaf def
        | (_, None) -> Node String_opt_map.empty in
      Node (String_opt_map.add value (update fct tail def sub) node)
    | _ -> failwith "[Clustered.update] inconsistent path"

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
    let rec loop path acc = function
    | Empty _ -> init
    | Leaf l -> fct path l acc
    | Node som ->
        String_opt_map.fold 
          (fun k v acc2 -> 
            loop (path @ [k]) acc2 v
          ) som acc in
    loop [] init t 
end (* module Clustered *)
