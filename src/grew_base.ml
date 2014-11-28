(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2013 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: http://grew.loria.fr                                               *)
(*    License: CeCILL (see LICENSE folder or "http://www.cecill.info")            *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open Log
open Printf

module String_set = Set.Make (String)
module String_map = Map.Make (String)

module Int_set = Set.Make (struct type t = int let compare = Pervasives.compare end)
module Int_map = Map.Make (struct type t = int let compare = Pervasives.compare end)

(* ================================================================================ *)
module Loc = struct
  type t = string * int

  let to_string (file,line) = sprintf "(file: %s, line: %d)" (Filename.basename file) line

  let opt_set_line line = function
    | None -> None
    | Some (file,_) -> Some (file, line)

  let opt_to_string = function
    | None -> ""
    | Some x -> to_string x
end (* module Loc *)

(* ================================================================================ *)
module Error = struct

  exception Build of (string * Loc.t option)
  exception Run of (string * Loc.t option)
  exception Bug of (string * Loc.t option)

  let build_ ?loc message = raise (Build (message, loc))
  let build ?loc = Printf.ksprintf (build_ ?loc)

  let run_ ?loc message = raise (Run (message, loc))
  let run ?loc = Printf.ksprintf (run_ ?loc)

  let bug_ ?loc message = raise (Bug (message, loc))
  let bug ?loc = Printf.ksprintf (bug_ ?loc)
end (* module Error *)

(* ================================================================================ *)
module String_ = struct

  let to_float string =
    try float_of_string string
    with _ ->
      try float_of_string (Str.global_replace (Str.regexp "\\.") "," string)
      with _ -> Error.build "[String_.to_float] cannot convert '%s'" string

  let of_float float = Str.global_replace (Str.regexp ",") "." (sprintf "%g" float)

  let rm_first_char = function "" -> "" | s -> String.sub s 1 ((String.length s) - 1)
end (* module String_ *)

(* ================================================================================ *)
module Dot = struct
  let to_png_file dot output_file =
    let temp_file_name,out_ch = Filename.open_temp_file ~mode:[Open_rdonly;Open_wronly;Open_text] "grewui_" ".dot" in
    fprintf out_ch "%s" dot;
    close_out out_ch;
    ignore(Sys.command(sprintf "dot -Tpng -o %s %s " output_file temp_file_name))
end (* module Dot *)

(* ================================================================================ *)
module File = struct
  let write data name =
    let out_ch = open_out name in
    fprintf out_ch "%s\n" data;
    close_out out_ch

  let read file =
    let in_ch = open_in file in
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
  let rec set position elt = function
    | [] -> failwith "List_.set"
    | _::t when position = 0 -> elt::t
    | x::t -> x:: (set (position-1) elt t)

  let rec rm elt = function
    | [] -> raise Not_found
    | x::t when x=elt -> t
    | x::t -> x::(rm elt t)

  let pos x l =
    let rec loop i = function
    | [] -> None
    | h::t when h=x -> Some i
    | _::t -> loop (i+1) t in
    loop 0 l

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

  let rec sort_assoc key = function
    | [] -> None
    | (k,_)::_ when key<k -> None
    | (k,_)::t when key>k -> sort_assoc key t
    | (_,v)::_ -> Some v

  let rec sort_remove_assoc key = function
    | [] -> []
    | (k,_)::_ as t when key<k -> t
    | (k,v)::t when key>k -> (k,v) :: (sort_remove_assoc key t)
    | (_,v)::t -> t

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

  let sort_union l1 l2 =
    let rec loop = function
      | [], l | l, [] -> l
      | x1::t1, x2::t2 when x1 < x2 -> x1 :: loop (t1, x2::t2)
      | x1::t1, x2::t2 when x1 > x2 -> x2 :: loop (x1::t1, t2)
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

  let foldi_left f init l =
    fst
      (List.fold_left
         (fun (acc,i) elt -> (f i acc elt, i+1))
         (init,0) l
      )

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

  val add: key -> 'a -> 'a t -> 'a t option

  val fold: ('b -> key -> 'a -> 'b) -> 'b -> 'a t -> 'b

  (* raise Not_found if no (key,elt) *)
  val remove: key -> 'a -> 'a t -> 'a t

  (* raise Not_found if no (key,elt) *)
  val remove_key: key -> 'a t -> 'a t

  (* [mem key value t ] test if the couple (key, value) is in the massoc [t]. *)
  val mem: key -> 'a -> 'a t -> bool

  (* mem_key key t] tests is [key] is associated to at least one value in [t]. *)
  val mem_key: key -> 'a t -> bool

  exception Not_disjoint
  val disjoint_union: 'a t -> 'a t -> 'a t

  exception Duplicate
  val merge_key: key -> key -> 'a t -> 'a t

  val exists: (key -> 'a -> bool) -> 'a t -> bool

  val rename: (key * key) list -> 'a t -> 'a t
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

  let to_string _ _ = failwith "Not implemted"

  let iter fct t =
    M.iter
      (fun key list -> List.iter (fun elt -> fct key elt) list
      ) t

  let add key elt t =
    try
      let list = M.find key t in
      match List_.usort_insert elt list with
        | Some l -> Some (M.add key l t)
        | None -> None
    with Not_found -> Some (M.add key [elt] t)

  let fold fct init t =
    M.fold
      (fun key list acc ->
        List.fold_left
          (fun acc2 elt ->
            fct acc2 key elt)
          acc list)
      t init

  (* Not found raised in the value is not defined *)
  let remove key value t =
    match M.find key t with
      | [one] when one=value -> M.remove key t
      | old -> M.add key (List_.usort_remove value old) t

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

  exception Duplicate

  let merge_key i j t =
    try
      let old_i = M.find i t in
      let old_j = try M.find j t with Not_found -> [] in
      M.add j (List_.sort_disjoint_union old_i old_j) (M.remove i t)
    with
      | Not_found -> (* no key i *) t
      | List_.Not_disjoint -> raise Duplicate

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

  let rename mapping t =
    M.fold
      (fun key value acc ->
        let new_key = try List.assoc key mapping with Not_found -> key in
        M.add new_key value acc
      ) t M.empty
end (* module Massoc_make *)

(* ================================================================================ *)
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
end (* module Id *)

(* ================================================================================ *)
(* copy from leopar *)
module Timeout = struct
  exception Stop

  let counter = ref 0.
  let timeout = ref None

  let start () = counter := Unix.time ()

  let check () =
    match !timeout with
    | None -> ()
    | Some delay ->
        if Unix.time () -. !counter > delay
        then raise Stop
end (* module Timeout *)
