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
module List_ = struct
  let rec cut size = function
    | [] -> []
    | _ when size=0 -> []
    | x::t -> x:: (cut (size-1) t)

  let rec set position elt = function
    | [] -> failwith "List_.set"
    | _::t when position = 0 -> elt::t
    | x::t -> x:: (set (position-1) elt t)

  let rec remove elt = function
    | [] -> raise Not_found
    | a::tail when a = elt -> tail
    | a::tail -> a::(remove elt tail)

  let index_opt x l =
    let rec loop i = function
      | [] -> None
      | h::t when h=x -> Some i
      | _::t -> loop (i+1) t in
    loop 0 l

  let rec opt_map f = function
    | [] -> []
    | x::t ->
      match f x with
      | None -> opt_map f t
      | Some r -> r :: (opt_map f t)

  let rec try_map exc fct = function
    | [] -> []
    | x::t -> let tail =  try_map exc fct t in
      try (fct x)::tail
      with e ->
        if e = exc
        then tail
        else raise e

  let rec flat_map f = function
    | [] -> []
    | x::t -> (f x)@(flat_map f t)

  let opt_mapi fct =
    let rec loop i = function
      | [] -> []
      | h::t ->
        match fct i h with
        | None -> loop (i+1) t
        | Some res -> res :: (loop (i+1) t)
    in loop 0

  let foldi_left f init l =
    fst
      (List.fold_left
         (fun (acc,i) elt -> (f i acc elt, i+1))
         (init,0) l
      )

  (* list intersection. Not efficient, do not use on large list *)
  let intersect l1 l2 =
    List.fold_left
      (fun acc x -> if (List.exists (fun y -> y = x) l1) then x::acc else acc
      ) [] l2

  let to_string string_of_item sep = function
    | [] -> ""
    | h::t -> List.fold_left (fun acc elt -> acc ^ sep ^ (string_of_item elt)) (string_of_item h) t

  let rev_to_string string_of_item sep = function
    | [] -> ""
    | h::t -> List.fold_left (fun acc elt -> (string_of_item elt) ^ sep ^ acc) (string_of_item h) t

  let rec sort_insert elt = function
    | [] -> [elt]
    | h::t when elt<h -> elt::h::t
    | h::t -> h::(sort_insert elt t)

  let rec sort_mem elt = function
    | [] -> false
    | h::_ when elt<h -> false
    | h::_ when elt=h -> true
    | h::t (* when elt>h *) -> sort_mem elt t

  let rec sort_assoc_opt key = function
    | [] -> None
    | (k,_)::_ when key<k -> None
    | (k,_)::t when key>k -> sort_assoc_opt key t
    | (_,v)::_ -> Some v

  let rec sort_mem_assoc key = function
    | [] -> false
    | (k,_)::_ when key<k -> false
    | (k,_)::t when key>k -> sort_mem_assoc key t
    | (_,v)::_ -> true

  let rec sort_remove_assoc key = function
    | [] -> []
    | (k,_)::_ as t when key<k -> t
    | (k,v)::t when key>k -> (k,v) :: (sort_remove_assoc key t)
    | (_,v)::t -> t

  let rec sort_update_assoc key value = function
    | [] -> [(key,value)]
    | (k,_)::_ as t when key<k -> (key,value) :: t
    | (k,_)::t when key=k -> (key,value) :: t
    | (k,v)::t -> (k,v) :: (sort_update_assoc key value t)

  let rec sort_remove_assoc_opt key = function
    | [] -> None
    | (k,_)::_ when key<k -> None
    | (k,_)::t when key=k -> Some t
    | x::t (* when key>k *) ->
      (match sort_remove_assoc_opt key t with
       | None -> None
       | Some new_t -> Some (x :: new_t)
      )

  exception Usort

  let rec usort_remove key = function
    | [] -> raise Not_found
    | x::t when key < x -> raise Not_found
    | x::t when key = x -> t
    | x::t -> x::(usort_remove key t)

  let usort_insert_opt ?(compare=Stdlib.compare) elt l =
    let rec loop = function
      | [] -> [elt]
      | x::t when compare elt x < 0 -> elt :: x :: t
      | x::t when compare elt x > 0 -> x :: (loop t)
      | _ -> raise Usort in
    try Some (loop l) with Usort -> None

  let rec sort_disjoint l1 l2 =
    match (l1,l2) with
    | [], _ | _, [] -> true
    | h1::t1 , h2::t2 when h1<h2 -> sort_disjoint t1 l2
    | h1::t1 , h2::t2 when h1>h2 -> sort_disjoint l1 t2
    | _ -> false

  let sort_is_empty_inter l1 l2 =
    let rec loop = function
      | [], _ | _, [] -> true
      | x1::t1, x2::t2 when x1 < x2 -> loop (t1, x2::t2)
      | x1::t1, x2::t2 when x1 > x2 -> loop (x1::t1, t2)
      | x1::t1, x2::t2 -> false in
    loop (l1,l2)

  let sort_inter l1 l2 =
    let rec loop = function
      | [], _ | _, [] -> []
      | x1::t1, x2::t2 when x1 < x2 -> loop (t1, x2::t2)
      | x1::t1, x2::t2 when x1 > x2 -> loop (x1::t1, t2)
      | x1::t1, x2::t2 -> x1 :: loop (t1, t2) in
    loop (l1,l2)

  let sort_union l1 l2 =
    let rec loop = function
      | [], l | l, [] -> l
      | x1::t1, x2::t2 when x1 < x2 -> x1 :: loop (t1, x2::t2)
      | x1::t1, x2::t2 when x1 > x2 -> x2 :: loop (x1::t1, t2)
      | x1::t1, x2::t2 -> x1 :: loop (t1, t2) in
    loop (l1,l2)


  exception Not_disjoint
  let sort_disjoint_union ?(compare=Stdlib.compare) l1 l2 =
    let rec loop = function
      | [], l | l, [] -> l
      | x1::t1, x2::t2 when (compare x1 x2) < 0 -> x1 :: loop (t1, x2::t2)
      | x1::t1, x2::t2 when (compare x1  x2) > 0 -> x2 :: loop (x1::t1, t2)
      | _ -> raise Not_disjoint in
    loop (l1,l2)

  let sort_include l1 l2 =
    let rec loop = function
      | [], l -> true
      | l, [] -> false
      | x1::t1, x2::t2 when x1 < x2 -> false
      | x1::t1, x2::t2 when x1 > x2 -> loop (x1::t1, t2)
      | x1::t1, x2::t2 -> loop (t1, t2) in
    loop (l1,l2)

  let sort_included_diff l1 l2 =
    let rec loop = function
      | [], l -> failwith "[sort_included_diff] not included"
      | l, [] -> l
      | x1::t1, x2::t2 when x1 < x2 -> x1 :: loop (t1, x2::t2)
      | x1::t1, x2::t2 when x1 > x2 -> failwith "[sort_included_diff] not included"
      | x1::t1, x2::t2 -> loop (t1, t2) in
    loop (l1,l2)

  let sort_diff l1 l2 =
    let rec loop = function
      | [], l -> []
      | l, [] -> l
      | x1::t1, x2::t2 when x1 < x2 -> x1 :: loop (t1, x2::t2)
      | x1::t1, x2::t2 when x1 > x2 -> loop (x1::t1, t2)
      | x1::t1, x2::t2 -> loop (t1, t2) in
    loop (l1,l2)

  let prev_next_iter fct list =
    let int_fct prev next elt = fct ?prev ?next elt in
    let rec loop prev = function
      | [] -> ()
      | [last] -> int_fct prev None last
      | head::snd::tail -> int_fct prev (Some snd) head; loop (Some head) (snd::tail)
    in loop None list
end (* module List_ *)


(* ================================================================================ *)
module type S = sig
  type key

  type +'a t

  val empty: 'a t

  (* an empty list returned if the key is undefined *)
  val assoc: key -> 'a t -> 'a list

  val is_empty: 'a t -> bool

  val to_string: ('a -> string) -> 'a t -> string

  val iter: (key -> 'a -> unit) -> 'a t -> unit

  val add_opt: key -> 'a -> 'a t -> 'a t option

  val replace: key -> 'a list -> 'a t -> 'a t

  val fold: ('b -> key -> 'a -> 'b) -> 'b -> 'a t -> 'b

  val fold_on_list: ('b -> key -> 'a list -> 'b) -> 'b -> 'a t -> 'b

  (* raise Not_found if no (key,elt) *)
  val remove: key -> 'a -> 'a t -> 'a t
  val remove_opt: key -> 'a -> 'a t -> 'a t option

  (* raise Not_found if no (key,elt) *)
  val remove_key: key -> 'a t -> 'a t

  (* [mem key value t ] test if the couple (key, value) is in the massoc [t]. *)
  val mem: key -> 'a -> 'a t -> bool

  (* mem_key key t] tests is [key] is associated to at least one value in [t]. *)
  val mem_key: key -> 'a t -> bool

  exception Not_disjoint
  val disjoint_union: 'a t -> 'a t -> 'a t


  val exists: (key -> 'a -> bool) -> 'a t -> bool

  val rename: (key * key) list -> 'a t -> 'a t

  val map_key: (key -> key) -> 'a t -> 'a t

  val find_opt: (key -> 'a -> bool) -> 'a t -> key option

  val filter: ('a -> bool) -> 'a t -> 'a t

  val filter_key: (key -> bool) -> 'a t -> 'a t

end (* module type S *)

(* ================================================================================ *)
module Massoc_make (Ord: Set.OrderedType) = struct
  module M = Map.Make (Ord)

  type key = Ord.t

  type 'a t = ('a list) M.t

  let empty = M.empty

  let is_empty t = (t=empty)

  let assoc key t =
    try M.find key t
    with Not_found -> []

  let to_string _ _ = failwith "Not implemented"

  let iter fct t =
    M.iter
      (fun key list -> List.iter (fun elt -> fct key elt) list
      ) t

  let replace = M.add

  let add_opt key elt t =
    match M.find_opt key t with
    | None -> Some (M.add key [elt] t)
    | Some list ->
      match List_.usort_insert_opt elt list with
      | Some l -> Some (M.add key l t)
      | None -> None

  let fold fct init t =
    M.fold
      (fun key list acc ->
         List.fold_left
           (fun acc2 elt ->
              fct acc2 key elt)
           acc list)
      t init

  let fold_on_list fct init t = M.fold (fun key list acc -> fct acc key list) t init

  (* [Not_found] raised in the value is not defined *)
  let remove key value t =
    match M.find key t with
    | [one] when one=value -> M.remove key t
    | old -> M.add key (List_.usort_remove value old) t

  let remove_opt key value t =
    try Some (remove key value t)
    with Not_found -> None

  let remove_key key t = M.remove key t

  let rec mem key value t =
    try List_.sort_mem value (M.find key t)
    with Not_found -> false

  let rec mem_key key t = M.mem key t


  exception Not_disjoint
  let disjoint_union t1 t2 =
    M.fold
      (fun key list acc ->
         try
           let old = M.find key acc in
           M.add key (List_.sort_disjoint_union list old) acc
         with
         | Not_found -> M.add key list acc
         | List_.Not_disjoint -> raise Not_disjoint
      ) t1 t2

  exception True
  let exists fct t =
    try
      M.iter
        (fun key list ->
           if List.exists (fun elt -> fct key elt) list
           then raise True
        ) t;
      false
    with True -> true

  exception Find of key
  let find_opt fct t =
    try
      M.iter
        (fun key list ->
           if List.exists (fun elt -> fct key elt) list
           then raise (Find key)
        ) t;
      None
    with Find key -> Some key

  let rename mapping t =
    M.fold
      (fun key value acc ->
         let new_key = try List.assoc key mapping with Not_found -> key in
         M.add new_key value acc
      ) t M.empty

  let map_key fct t = M.fold (fun key value acc -> M.add (fct key) value acc) t M.empty

  let filter test t =
    M.fold
      (fun key value acc ->
         match List.filter test value with
         | [] -> acc
         | l -> M.add key l acc
      ) t M.empty

  let filter_key test t =
    M.fold
      (fun key value acc ->
        if test key
        then M.add key value acc
        else acc
      ) t M.empty
end (* module Massoc_make *)

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