(**********************************************************************************)
(*    grewlib • a Graph Rewriting library dedicated to NLP applications           *)
(*                                                                                *)
(*    Copyright 2011-2024 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: https://grew.fr                                                    *)
(*    License: CeCILL (see LICENSE folder or "http://cecill.info/")               *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open Printf

module String_set = Set.Make (String)

module String_map = Map.Make (String)

module String_opt_set = CCSet.Make (struct type t = string option let compare = compare end)

module String_opt_map = CCMap.Make (struct type t = string option let compare = compare end)

module Int_set = Set.Make (struct type t = int let compare = Stdlib.compare end)

module Int_map = Map.Make (struct type t = int let compare = Stdlib.compare end)

(* ================================================================================ *)
module Clustered = struct

  type 'a t = 
    | Empty of int (* virtual depth *)
    | Leaf of 'a
    | Node of 'a t String_opt_map.t
  
  let rec depth = function
    | Node som -> 1 + (som |> String_opt_map.choose |> snd |> depth)
    | _ -> 0
  
  (* nbre of element *)
  let rec nb_clusters = function
    | Empty _ -> 0
    | Leaf _ -> 1
    | Node map -> String_opt_map.fold (fun _ t acc -> acc + (nb_clusters t)) map 0
  
  let rec cardinal elt_size = function
    | Empty _ -> 0
    | Leaf x -> elt_size x
    | Node map -> String_opt_map.fold (fun _ t acc -> acc + (cardinal elt_size t)) map 0
  
  
  let merge_sizes s1 s2 =
    String_opt_map.merge
      (fun _ opt1 opt2 ->
        match (opt1, opt2) with
        | (Some i1, Some i2) -> Some (i1+i2)
        | (None, Some s) -> Some s
        | (Some s, None) -> Some s
        | (None, None) -> None
      ) s1 s2 

  (** outputs a raw display of the structure (to be used only for debug) *)
  let _dump to_string t = 
    let rec loop indent = function 
    | Empty depth -> printf "%s_EMPTY of depth %d_\n%!" (String.make indent ' ') depth
    | Leaf a -> printf "%s_LEAF_ %s\n%!" (String.make indent ' ') (to_string a)
    | Node som -> 
      String_opt_map.iter
      (fun key a ->
        printf "%s_NODE_ %s\n%!" (String.make indent ' ') (CCOption.get_or ~default:"__undefined__" key);
        loop (indent+2) a
      ) som in
    loop 0 t

  let rec sizes elt_size = function
  | Empty d -> List.init d (fun _ -> String_opt_map.empty) 
  | Leaf _ -> []
  | Node map ->
      let (first_layer: int String_opt_map.t) = 
        String_opt_map.map
          (fun sm -> cardinal elt_size sm
          ) map in
      let sub_layers = 
        String_opt_map.fold
          (fun _ sm acc -> 
            let (sub_sizes: int String_opt_map.t list) = sizes elt_size sm in
            match acc with
            | None -> Some sub_sizes
            | Some prev -> Some (List.map2 merge_sizes sub_sizes prev)
          ) map None in
      match sub_layers with
      | None -> failwith "[BUG] empty map"
      | Some s -> first_layer :: s
  
  
  let empty depth = Empty depth
  
  let build_layer sub_fct key_fct item_list =
    match item_list with
    | [] -> failwith "[Clustered.build_layer] item_list cannot be empty"
    | _ -> 
      let som = 
        List.fold_left
        (fun acc item ->
          String_opt_map.add (key_fct item) (sub_fct item) acc
        ) String_opt_map.empty item_list in
      Node som
  
  
  let get_opt default key_list t =
    let rec loop = function
    | (l, Empty d) when List.length l = d -> default
    | ([], Leaf v) -> v
    | (key::key_tail, Node som) -> 
      begin
        match String_opt_map.find_opt key som with
      | None -> default
      | Some t' -> loop (key_tail, t')
      end
    | _ -> failwith "[Clustered.get_opt] inconsistent path"
    in loop (key_list,t)
  
  let fold_layer default fct_leaf init fct_node closure t =
    let rec loop t =
      match t with
      | Empty _ -> fct_leaf default
      | Leaf l -> fct_leaf l
      | Node som -> closure (String_opt_map.fold (fun key sub acc -> fct_node key (loop sub) acc) som init) in
    loop t

  let rec update fct path def t =
    match (path, t) with
    | ([], Empty _) -> Leaf (fct def)
    | ([], Leaf i) -> Leaf (fct i)
    | (value::tail, Empty _) ->
      let sub = 
        match tail with
        | [] -> Leaf def
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
             if nb_clusters v > 1 
             then String_opt_map.add k v acc
             else acc
          ) map String_opt_map.empty
      )
    | (_, Node map) -> Node (String_opt_map.map (fun v -> prune_unambiguous (depth - 1) v) map)
    | _ -> failwith "[Clustered.prune_unambiguous] no enough depth in the projection"

  (* NB: unused function *)
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
      | _ -> failwith "[Clustered.to_json] inconsistent data" in
    `List (loop [] keys [] t) *)

  let rec map (fct: 'a -> 'b) = function
    | Empty d -> Empty d
    | Leaf a -> Leaf (fct a) 
    | Node m -> Node (String_opt_map.map (fun sm -> map fct sm) m)
  
  let fold fct t init =
    let rec loop path acc = function
    | Empty _ -> acc
    | Leaf l -> fct path l acc
    | Node som ->
        String_opt_map.fold 
          (fun k v acc2 -> 
            loop (path @ [k]) acc2 v
          ) som acc in
    loop [] init t 

  let merge_keys misc_key merge_item default filter_list t =
    let d = depth t in
    fold
      (fun key_list item acc ->
        let new_key_list = List.map2 (fun f x -> if f x then x else misc_key) filter_list key_list in
        update (fun x -> merge_item item x) new_key_list default acc
      ) t (Empty d)

  let iter fct t =
    let rec loop path = function
    | Empty _ -> ()
    | Leaf l -> fct path l
    | Node som ->
        String_opt_map.iter
          (fun k v -> 
            loop (path @ [k]) v
          ) som in
    loop [] t

  let get_all_keys depth t =
    let rec loop = function
    | (0, Node som) -> 
      String_opt_map.fold 
        (fun k _ acc -> String_opt_set.add k acc)
        som String_opt_set.empty
    | (i, Node som) when i > 0 -> 
      String_opt_map.fold 
        (fun _ v acc -> String_opt_set.union (loop (i-1,v)) acc)
        som String_opt_set.empty
    | _ -> String_opt_set.empty in

    loop (depth,t) |> String_opt_set.to_list 

end (* module Clustered *)
