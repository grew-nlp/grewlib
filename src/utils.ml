open Log
open Printf 

module StringMap = Map.Make (String)

module IntSet = Set.Make (struct type t = int let compare = Pervasives.compare end)

module IntMap = 
  struct 
    include Map.Make (struct type t = int let compare = Pervasives.compare end)
(** returns the image of a map [m]*)
	
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

    let range key_set m = 
      IntSet.fold (fun k s -> (IntSet.add (find k m) s)) key_set IntSet.empty
	
    let keys m = 
      fold (fun k v s -> (IntSet.add k s)) m IntSet.empty
	
(* union of two maps*)
    let union_map m m' = fold (fun k v m'' -> (add k v m'')) m m'
	
    exception MatchNotInjective
	
(*
 * union of two injective maps having different ranges :
 * \forall x \neq y \in m: m(x) \neq m(y)
 * \forall x' \neq y' \in m': m'(x) \neq m'(y)
 * \forall x \in m /\ m': m(x) = m'(x)
 * \forall x \in m : x \not\in\m' => \forall y \in m' m(x) \neq m'(y)
 *)
    let union_if m m' = 
      let keys_m = keys m in
      let keys_m' = keys m' in
      let inter_keys = IntSet.inter keys_m keys_m' in
      if IntSet.for_all (fun elt -> (find elt m) = (find elt m')) inter_keys
      then 
	let keys_s_m' = IntSet.diff keys_m' inter_keys in
	let range_m = range keys_m m in 
	let range_m' = range keys_s_m' m' in
	if (IntSet.inter range_m range_m') = IntSet.empty
	then union_map m m'
	else raise MatchNotInjective
      else raise MatchNotInjective	  
  end
    
module Loc = struct
  type t = string * int 

  let to_string (file,line) = Printf.sprintf "(file: %s, line: %d)" (Filename.basename file) line

  let opt_to_string = function
    | None -> ""
    | Some x -> to_string x
end

 



module File = struct
  let write data name =
    let out_ch = open_out name in
    fprintf out_ch "%s\n" data;
    close_out out_ch
 end


module Array_ = struct
  let dicho_mem elt array =
    let rec loop low high =
      (if low > high 
      then false
      else
	match (low+high)/2 with
	| middle when array.(middle) = elt -> true
	| middle when array.(middle) < elt -> loop (middle+1) high
	| middle -> loop low (middle - 1)
      ) in 
    loop 0 ((Array.length array) - 1)

  (* dichotomic search in a sorted array *)
  let dicho_find elt array =
    let rec loop low high =
      (if low > high then raise Not_found);
      match (low+high)/2 with
      | middle when array.(middle) = elt -> middle
      | middle when array.(middle) < elt -> loop (middle+1) high
      | middle -> loop low (middle - 1) in 
    loop 0 ((Array.length array) - 1)
      
  let dicho_find_assoc elt array =
    let rec loop low high =
      (if low > high then raise Not_found);
      match (low+high)/2 with
      | middle when fst array.(middle) = elt -> middle
      | middle when fst array.(middle) < elt -> loop (middle+1) high
      | middle -> loop low (middle - 1) in 
    loop 0 ((Array.length array) - 1)
end


module List_ = struct
  let rec rm elt = function
    | [] -> raise Not_found
    | x::t when x=elt -> t
    | x::t -> x::(rm elt t)

  let rec opt = function
    | [] -> []
    | None :: t -> opt t
    | Some x :: t -> x :: (opt t)

  let rec opt_map f = function
    | [] -> []
    | x::t -> 
	match f x with
	| None -> opt_map f t
	| Some r -> r :: (opt_map f t)

  let rec flat_map f = function
    | [] -> []
    | x::t -> (f x)@(flat_map f t)

  let iteri fct = 
    let rec loop i = function 
      | [] -> ()
      | h::t -> (fct i h); (loop (i+1) t) in
    loop 0
      
  let mapi fct = 
    let rec loop i = function 
      | [] -> []
      | h::t -> let head = fct i h in head :: (loop (i+1) t)
    in loop 0

  let foldi_left f init l =
    fst 
      (List.fold_left 
	 (fun (acc,i) elt -> (f i acc elt, i+1))
	 (init,0) l
      )

  let rec remove elt = function
    | [] -> raise Not_found
    | a::tail when a = elt -> tail
    | a::tail -> a::(remove elt tail)
		      
  let to_string string_of_item sep = function
    | [] -> ""
    | h::t -> List.fold_left (fun acc elt -> acc ^ sep ^ (string_of_item elt)) (string_of_item h) t
	  
  let rec sort_insert elt = function
    | [] -> [elt]
    | h::t when elt<h -> elt::h::t 
    | h::t -> h::(sort_insert elt t)

  let rec sort_mem elt = function
    | [] -> false
    | h::_ when elt<h -> false
    | h::_ when elt=h -> true
    | h::t (* when elt>h *) -> sort_mem elt t		   
  exception Usort

  let rec usort_remove key = function 
    | [] -> raise Not_found
    | x::t when key < x -> raise Not_found
    | x::t when key = x -> t
    | x::t -> x::(usort_remove key t)

  let usort_insert ?(compare=Pervasives.compare) elt l =
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
      
  exception Not_disjoint
  let sort_disjoint_union ?(compare=Pervasives.compare) l1 l2 = 
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

end


module Massoc = struct
  (* Massoc is implemented with caml lists *)
  (* invariant: we suppose that all 'a list in the structure are not empty! *) 
  type 'a t = (int * 'a list) list
	
  let empty = []

  let is_empty t = (t=[])

  let rec assoc key = function
    | [] -> []
    | (h,_)::_ when key<h -> []
    | (h,v)::t when key=h -> v
    | (h,_)::t (* when key>h *) -> assoc key t

  let to_string elt_to_string t = 
    List_.to_string 
      (fun (i,elt_list) -> 
	sprintf "%d -> [%s]" i (List_.to_string elt_to_string "," elt_list)
      ) "; " t
    
  let iter fct t =
    List.iter 
      (fun (key,list) ->
	List.iter 
	  (fun elt -> fct key elt)
	  list
      ) t
	
  let rec add key elt = function
    | [] -> Some [(key, [elt])]
    | (h,list)::t when h=key -> 
	(match List_.usort_insert elt list with 
	| Some new_list -> Some ((h, new_list)::t)
	| None -> None
	)
    | ((h,_)::_) as t when key<h -> Some ((key,[elt])::t)
    | (h,l)::t (* when key>h *) -> 
	match (add key elt t) with Some t' -> Some ((h,l)::t') | None -> None
	    
  let fold_left fct init t =
    List.fold_left 
      (fun acc (key,list) ->
	List.fold_left 
	  (fun acc2 elt ->
	    fct acc2 key elt)
	  acc list)
      init t
	    
  let rec remove key value = function
    | [] -> raise Not_found 
    | (h,_)::_ when key<h -> raise Not_found 
    | (h,[v])::t when key=h && value=v -> t 
    | (h,list)::t when key=h -> (h,List_.usort_remove value list)::t
    | (h,list)::t (* when key>h *) -> (h,list) :: (remove key value t)

  let rec remove_key key = function
    | [] -> raise Not_found
    | (h,_)::_ when key<h -> raise Not_found 
    | (h,list)::t when key=h -> t
    | (h,list)::t (* when key>h *) -> (h,list) :: (remove_key key t)
	  
  let rec mem key value = function
    | [] -> false
    | (h,_)::_ when key<h -> false
    | (h,list)::t when key=h -> List_.sort_mem value list
    | (h,list)::t (* when key>h *) -> mem key value t

  let rec mem_key key = function
    | [] -> false
    | (h,_)::_ when key<h -> false
    | (h,_)::t when key=h -> true
    | (h,_)::t (* when key>h *) -> mem_key key t

  exception Not_disjoint
  let disjoint_union t1 t2 = 
    let rec loop = function
      | [], t | t, [] -> t
      | ((h1,l1)::t1, (h2,l2)::t2) when h1 < h2 -> (h1,l1)::(loop (t1,((h2,l2)::t2)))
      | ((h1,l1)::t1, (h2,l2)::t2) when h1 > h2 -> (h2,l2)::(loop (((h1,l1)::t1),t2))
      | ((h1,l1)::t1, (h2,l2)::t2) (* when h1=h2*) ->
	  try (h1,List_.sort_disjoint_union l1 l2)::(loop (t1, t2))
	  with List_.Not_disjoint -> raise Not_disjoint
    in loop (t1, t2)

  exception Duplicate
  let merge_key i j t =
    try
      let i_list = List.assoc i t in
      disjoint_union (remove_key i t) [j,i_list]
    with 
    | Not_found -> (* no key i *) t
    | Not_disjoint -> raise Duplicate


  let exists fct t = List.exists (fun (key,list) -> List.exists (fun value -> fct key value) list) t
end

exception Build of (string * Loc.t option)
exception Run of (string * Loc.t option)
exception Bug of string

module Error = struct
  let build_ ?loc message = 
    Log.fmessage "[%s] %s" (match loc with None -> "?" | Some x -> Loc.to_string x) message;
    raise (Build (message, loc))
  let build ?loc = Printf.ksprintf (build_ ?loc)

  let run_ ?loc message = raise (Run (message, loc))
  let run ?loc = Printf.ksprintf (run_ ?loc)

  let bug_ message = raise (Bug message)
  let bug x = Printf.ksprintf bug_ x
end



module Id = struct
  type name = string

  type t = int

  type table = name array

  let build ?(loc:Loc.t option) string table =
    try Array_.dicho_find string table
    with Not_found -> Error.build ?loc "Identifier '%s' not found" string

  let build_opt string table = 
    try Some (Array_.dicho_find string table)
    with Not_found -> None
end
