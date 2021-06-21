(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2021 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: https://grew.fr                                                     *)
(*    License: CeCILL (see LICENSE folder or "http://cecill.info/")            *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open Log
open Printf

module String_set = Set.Make (String)
module String_map = Map.Make (String)

module Int_set = Set.Make (struct type t = int let compare = Stdlib.compare end)
module Int_map = Map.Make (struct type t = int let compare = Stdlib.compare end)

let to_uname = function
  | "cat" -> "upos"
  | "pos" -> "xpos"
  | "phon" -> "form"
  | x -> x

let (<<) f g x = f (g x)

(* ================================================================================ *)
module Loc = struct
  type t = string option * int option

  let empty = (None, None)
  let file f = (Some f, None)
  let file_opt_line fo l = (fo, Some l)
  let file_opt_line_opt fo lo = (fo, lo)
  let set_line l (x,_) = (x, Some l)

  let get_line_opt = snd

  let to_string = function
    | (Some file, Some line) -> sprintf "[file: %s, line: %d]" (Filename.basename file) line
    | (None, Some line) -> sprintf "[line: %d]" line
    | (Some file, None) -> sprintf "[file: %s]" (Filename.basename file)
    | (None, None) -> ""
end (* module Loc *)

(* ================================================================================ *)
module Error = struct

  exception Build of (string * Loc.t option)
  let build_ ?loc message = raise (Build (message, loc))
  let build ?loc = Printf.ksprintf (build_ ?loc)

  exception Run of (string * Loc.t option)
  let run_ ?loc message = raise (Run (message, loc))
  let run ?loc = Printf.ksprintf (run_ ?loc)

  exception Bug of (string * Loc.t option)
  let bug_ ?loc message = raise (Bug (message, loc))
  let bug ?loc = Printf.ksprintf (bug_ ?loc)

  exception Parse of (string * Loc.t option)
  let parse_ ?loc message = raise (Parse (message, loc))
  let parse ?loc = Printf.ksprintf (parse_ ?loc)

  let warning_ ?loc message =
    match loc with
    | Some loc -> Log.fwarning "[%s] %s" (Loc.to_string loc) message
    | None -> Log.fwarning "%s" message
  let warning ?loc = Printf.ksprintf (warning_ ?loc)

end (* module Error *)

(* ================================================================================ *)
module String_ = struct
  let rm_first_char = function "" -> "" | s -> String.sub s 1 ((String.length s) - 1)

  let re_match re s = (Str.string_match re s 0) && (Str.matched_string s = s)

  let rev_concat sep l =
    let rec loop = function
      | [] -> ""
      | [one] -> one
      | h :: tail -> (loop tail) ^ sep ^ h in
    loop l

end (* module String_ *)

(* ================================================================================ *)
module File = struct
  let write data name =
    let out_ch = open_out name in
    fprintf out_ch "%s\n" data;
    close_out out_ch

  let read file =
    let in_ch = open_in file in
    try
      (* if the input file contains an UTF-8 byte order mark (EF BB BF), skip 3 bytes, else get back to 0 *)
      (match input_byte in_ch with 0xEF -> seek_in in_ch 3 | _ -> seek_in in_ch 0);

      let rev_lines = ref [] in
      try
        while true do
          let line = input_line in_ch in
          if (Str.string_match (Str.regexp "^[ \t]*$") line 0) || (line.[0] = '%')
          then ()
          else rev_lines := line :: !rev_lines
        done; assert false
      with End_of_file ->
        close_in in_ch;
        List.rev !rev_lines
    with End_of_file -> [] (* if the file is empty, input_byte raises End_of_file *)


  (* [read_ln file] returns a list of couples (line_num, line). Blank lines and lines starting with '%' are ignored. *)
  let read_ln file =
    let in_ch = open_in file in
    (* if the input file contains an UTF-8 byte order mark (EF BB BF), skip 3 bytes, else get back to 0 *)
    (match input_byte in_ch with 0xEF -> seek_in in_ch 3 | _ -> seek_in in_ch 0);

    let cpt = ref 0 in
    let rev_lines = ref [] in
    try
      while true do
        let line = input_line in_ch in
        incr cpt;
        if (Str.string_match (Str.regexp "^[ \t]*$") line 0) || (line.[0] = '%')
        then ()
        else rev_lines := (!cpt, line) :: !rev_lines
      done; assert false
    with End_of_file ->
      close_in in_ch;
      List.rev !rev_lines

  let load file =
    let ch = open_in file in
    let buff = Buffer.create 32 in
    try
      while true do
        let next = input_line ch in
        Printf.bprintf buff "%s\n" next
      done; assert false
    with End_of_file ->
      close_in ch;
      Buffer.contents buff

  exception Found of int
  let get_suffix_opt file_name =
    let len = String.length file_name in
    try
      for i = len-1 downto 0 do
        if file_name.[i] = '.'
        then raise (Found i)
      done;
      None
    with
    | Found i -> Some (String.sub file_name i (len-i))
end (* module File *)

(* ================================================================================ *)
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
end (* module Array_ *)

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
module type OrderedType = sig
  type t
  val compare: t -> t -> int
end (* module type OrderedType *)

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

end (* module type S *)

(* ================================================================================ *)
module Massoc_make (Ord: OrderedType) = struct
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
end (* module Massoc_make *)

(* ================================================================================ *)
module Id = struct
  type t = int

  type 'a gtable = 'a array * ('a -> string)

  let gbuild ?loc key (table,conv) =
    try Array_.dicho_find key table
    with Not_found -> Error.build ?loc "[Id.gbuild] Identifier '%s' not found" (conv key)

  let gbuild_opt key (table, _) =
    try Some (Array_.dicho_find key table)
    with Not_found -> None

  type name = string
  type table = string array

  let build ?(loc:Loc.t option) key table =
    try Array_.dicho_find key table
    with Not_found -> Error.build ?loc "[Id.build] Identifier '%s' not found" key

  let build_opt key table =
    try Some (Array_.dicho_find key table)
    with Not_found -> None

  (* [get_pos_opt id] returns Some v (float) iff id is "Wv" else None *)
  let get_pos_opt name =
    let len = String.length name in
    if len > 0 && name.[0] = 'W'
    then
      begin
        let sub = String.sub name 1 (len-1) in
        float_of_string_opt sub
      end
    else None

end (* module Id *)

(* ================================================================================ *)
module Timeout = struct
  exception Stop of float

  let counter = ref 0.
  let duration = ref 0.
  let timeout = ref None

  let start () = counter := Unix.gettimeofday ()
  let stop () = duration := Unix.gettimeofday () -. !counter

  let check () =
    match !timeout with
    | None -> ()
    | Some delay ->
      if Unix.gettimeofday () -. !counter > delay
      then raise (Stop delay)
  
  let get_duration () = !duration
    
end (* module Timeout *)

(* ================================================================================ *)
module Global = struct
  let current_loc = ref Loc.empty
  let label_flag = ref false
  let current_dir = ref "."

  let get_loc () = !current_loc
  let loc_string () = Loc.to_string !current_loc

  let get_line_opt () = snd (get_loc ())

  let get_dir () = !current_dir
  let new_file filename =
    current_dir := Filename.dirname filename;
    current_loc := (Some filename, Some 1);
    label_flag := false

  let new_string () =
    current_loc := (None , Some 1);
    label_flag := false

  let new_line () = match !current_loc with
    | (_,None) -> ()
    | (fo, Some l) -> current_loc := (fo, Some (l+1))

  let debug = ref false
  let safe_commands = ref false

  let track_rules = ref false
  let track_history = ref false
  let track_impact = ref false
end (* module Global *)

module Dependencies = struct
  let lex_cmp (i1, j1) (i2,j2) = match Stdlib.compare i1 i2 with 0 -> Stdlib.compare j1 j2 | x -> x

  let rec insert_sorted i = function
    | h::t when h < i -> h :: (insert_sorted i t)
    | l -> i::l

  let is_projective edge_list =
    let sorted_edge_list = List.sort lex_cmp edge_list in
    let rec loop position from_here from_before = function
      | [] ->
        (* Printf.printf "=N=> pos=%d H=[%s] B=[%s]\n" position (String.concat "," (List.map string_of_int from_here)) (String.concat "," (List.map string_of_int from_before)); *)
        true
      | (i,j) :: tail ->
        (* Printf.printf "=S=> (%d, %d) pos=%d H=[%s] B=[%s]\n" i j position (String.concat "," (List.map string_of_int from_here)) (String.concat "," (List.map string_of_int from_before)); *)
        let rec reduce_from_before = function
          | h::t when h <= i -> reduce_from_before t
          | l -> l in
        let (new_from_here, new_from_before) =
          if i > position
          then ([], reduce_from_before (from_here @ from_before))
          else (from_here, reduce_from_before from_before) in
        (* Printf.printf "   ...> NH=[%s] NB=[%s]\n" (String.concat "," (List.map string_of_int new_from_here)) (String.concat "," (List.map string_of_int new_from_before)); *)
        match new_from_before with
        | h::t when j > h -> false
        | h::t when j = h -> loop i new_from_here new_from_before tail
        | _ -> loop i (insert_sorted j new_from_here) new_from_before tail in
    loop 0 [] [] sorted_edge_list

end

