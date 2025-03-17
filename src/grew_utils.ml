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


let (<<) f g x = f(g(x))

(* ================================================================================ *)
module Cmp = struct
  (** This module introduces a two values types for Equalty / Disequality *)

  type t = Eq | Neq

  let to_string = function Eq -> "=" | Neq -> "<>"

  let fct = function Eq -> (=) | Neq -> (<>)
  (** [fct t] return a function of type 'a -> 'a -> bool which corresponds either to equlaity or disequality *)
end

(* ================================================================================ *)
module Loc = struct
  type t = string option * int option

  let empty = (None, None)
  let file f = (Some f, None)
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
end (* module Error *)

(* ================================================================================ *)
module Warning = struct
  let magenta_ ?loc message =
    let prefix = match loc with Some l -> sprintf "[%s] " (Loc.to_string l) | None -> "" in
    ANSITerminal.eprintf [ANSITerminal.magenta] "%s%s\n%!" prefix message;
    flush stderr
  let magenta ?loc = Printf.ksprintf (magenta_ ?loc)
end

(* ================================================================================ *)
module Info = struct
  let print x = Printf.ksprintf (fun s -> ANSITerminal.printf [] "%s\n" s; flush stdout) x
  let green x = Printf.ksprintf (fun s -> ANSITerminal.printf [ANSITerminal.green] "%s\n" s; flush stdout) x
  let blue x = Printf.ksprintf (fun s -> ANSITerminal.printf [ANSITerminal.blue] "%s\n" s; flush stdout) x
  let red x = Printf.ksprintf (fun s -> ANSITerminal.printf [ANSITerminal.red] "%s\n" s; flush stdout) x
  let magenta x = Printf.ksprintf (fun s -> ANSITerminal.printf [ANSITerminal.magenta] "%s\n" s; flush stdout) x
end

(* ================================================================================ *)
module Range = struct
  type t = (int option * int option)

  let to_string = function
    | (None, None) -> ""
    | (Some x, None) -> sprintf "[%d:]" x
    | (None, Some y) -> sprintf "[:%d]" y
    | (Some x, Some y) -> sprintf "[%d:%d]" x y

  (** Python like substring extraction
     [get_range (init_opt, final_opt) s] return the python output of s[init_opt:final_opt]
     NB: indexes correspond to UTF-8 chars. ex: [get_range (None, Some (-1)) "été"] ==> "ét"
  *)
  let extract (iopt,fopt) s =
    match (iopt, fopt) with
    | (None, None) -> s
    | _ ->
      match CCUtf8_string.of_string s with
      | None -> Error.run "[String_.get_range] '%s' is not a valid UTF-8 string encoding" s
      | Some utf8_s ->
        let char_list = CCUtf8_string.to_list utf8_s in
        let len = CCUtf8_string.n_chars utf8_s in
        let init = match iopt with
          | None -> 0
          | Some i when i < 0 -> max (len + i) 0
          | Some i -> min i len in
        let final = match fopt with
          | None -> len
          | Some i when i < 0 -> max (len + i) 0
          | Some i -> min i len in
        if final < init
        then ""
        else char_list |> CCList.drop init |> CCList.take (final - init) |> CCUtf8_string.of_list |> CCUtf8_string.to_string
end (* module Range *)

(* ==================================================================================================== *)
module File = struct
  (* get the last modif time of a [file]. Returns [min_float] if the file does not exist *)
  let last_modif file =
    try
      let stat = Unix.stat file in
      stat.Unix.st_mtime
    with Unix.Unix_error _ -> Float.min_float
  
  let concat_names l = List.fold_left Filename.concat "" l

  type path_status = 
  | File 
  | Directory
  | Dont_exist

  let get_path_status path =
    try 
      match Sys.is_directory path with
      | true -> Directory
      | false -> File
    with Sys_error _ -> Dont_exist

  let ensure_directory dir =
    match get_path_status dir with
    | Directory -> ()
    | File -> Error.run "Cannot build directory `%s`, a file with the same name already exists" dir
    | Dont_exist -> Unix.mkdir dir 0o755

end

(* ================================================================================ *)
module String_ = struct
  let rm_first_char = function "" -> "" | s -> String.sub s 1 ((String.length s) - 1)

  let re_match re s = (Str.string_match re s 0) && (Str.matched_string s = s)

  let get_suffix_opt file_name =
    let exception Found of int in
    let len = String.length file_name in
    try
      for i = len-1 downto 0 do
        if file_name.[i] = '.'
        then raise (Found i)
      done;
      None
    with
    | Found i -> Some (String.sub file_name i (len-i))

  (* [extend_path ~env path] replaces each substring "${XXX}" in [path] by the value of the env variable XXX.
     raise [Error.run] if some variable is undefined. *)
  let extend_path ~env path =
    Str.global_substitute
      (Str.regexp {|\${\([^}]*\)}|})
      (fun _ ->
        let varname = Str.matched_group 1 path in
          match Sys.getenv_opt varname with
          | Some v -> v
          | None ->
            match List.assoc_opt varname env with
            | Some v -> v
            | None -> Error.run "Environment variable `%s` is undefined" varname
      )
      path

  let of_float_clean f =
    let s = string_of_float f in
    if String.ends_with ~suffix:"." s
    then String.sub s 0 ((String.length s) - 1)
    else s

  let nfc s = Uunf_string.normalize_utf_8 `NFC s

  let escape_lt_gt s =
    s 
    |> Str.global_replace (Str.regexp_string ">") "&gt;"
    |> Str.global_replace (Str.regexp_string "<") "&lt;"
end (* module String *)

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

  let shuffle_N n =
    Random.self_init ();
    let a = Array.init n (fun x -> x) in
    for i = n-1 downto 1 do
      let j = Random.int (i+1) in
      let tmp = a.(i) in
      a.(i) <- a.(j);
      a.(j) <- tmp
    done;
    a
end (* module Array_ *)

(* ================================================================================ *)
module List_ = struct

  let rec remove elt = function
    | [] -> raise Not_found
    | a::tail when a = elt -> tail
    | a::tail -> a::(remove elt tail)

  let index_opt x l =
    let rec loop i = function
      | [] -> None
      | h::_ when h=x -> Some i
      | _::t -> loop (i+1) t in
    loop 0 l

  let rec try_map exc fct = function
    | [] -> []
    | x::t -> let tail =  try_map exc fct t in
      try (fct x)::tail
      with e ->
        if e = exc
        then tail
        else raise e

  (* list intersection. Not efficient, do not use on large list *)
  let intersect l1 l2 =
    List.fold_left
      (fun acc x -> if (List.exists (fun y -> y = x) l1) then x::acc else acc
      ) [] l2

  let rec sort_insert elt = function
    | [] -> [elt]
    | h::t when elt<h -> elt::h::t
    | h::t -> h::(sort_insert elt t)

  let rec sort_mem elt = function
    | [] -> false
    | h::_ when elt<h -> false
    | h::_ when elt=h -> true
    | _::t (* when elt>h *) -> sort_mem elt t

  let rec sort_assoc_opt key = function
    | [] -> None
    | (k,_)::_ when key<k -> None
    | (k,_)::t when key>k -> sort_assoc_opt key t
    | (_,v)::_ -> Some v

  let rec sort_mem_assoc key = function
    | [] -> false
    | (k,_)::_ when key<k -> false
    | (k,_)::t when key>k -> sort_mem_assoc key t
    | (_,_)::_ -> true

  let rec sort_remove_assoc key = function
    | [] -> []
    | (k,_)::_ as t when key<k -> t
    | (k,v)::t when key>k -> (k,v) :: (sort_remove_assoc key t)
    | _::t -> t

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
    | x::_ when key < x -> raise Not_found
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
    | h1::t1 , h2::_ when h1<h2 -> sort_disjoint t1 l2
    | h1::_ , h2::t2 when h1>h2 -> sort_disjoint l1 t2
    | _ -> false

  let sort_is_empty_inter l1 l2 =
    let rec loop = function
      | [], _ | _, [] -> true
      | x1::t1, x2::t2 when x1 < x2 -> loop (t1, x2::t2)
      | x1::t1, x2::t2 when x1 > x2 -> loop (x1::t1, t2)
      | _ -> false in
    loop (l1,l2)

  let sort_inter l1 l2 =
    let rec loop = function
      | [], _ | _, [] -> []
      | x1::t1, x2::t2 when x1 < x2 -> loop (t1, x2::t2)
      | x1::t1, x2::t2 when x1 > x2 -> loop (x1::t1, t2)
      | x1::t1, _::t2 -> x1 :: loop (t1, t2) in
    loop (l1,l2)

  let sort_union l1 l2 =
    let rec loop = function
      | [], l | l, [] -> l
      | x1::t1, x2::t2 when x1 < x2 -> x1 :: loop (t1, x2::t2)
      | x1::t1, x2::t2 when x1 > x2 -> x2 :: loop (x1::t1, t2)
      | x1::t1, _::t2 -> x1 :: loop (t1, t2) in
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
      | [], _ -> true
      | _, [] -> false
      | x1::_, x2::_ when x1 < x2 -> false
      | x1::t1, x2::t2 when x1 > x2 -> loop (x1::t1, t2)
      | _::t1, _::t2 -> loop (t1, t2) in
    loop (l1,l2)

  let sort_included_diff l1 l2 =
    let rec loop = function
      | [], _ -> failwith "[sort_included_diff] not included"
      | l, [] -> l
      | x1::t1, x2::t2 when x1 < x2 -> x1 :: loop (t1, x2::t2)
      | x1::_, x2::_ when x1 > x2 -> failwith "[sort_included_diff] not included"
      | _::t1, _::t2 -> loop (t1, t2) in
    loop (l1,l2)

  let sort_diff l1 l2 =
    let rec loop = function
      | [], _ -> []
      | l, [] -> l
      | x1::t1, x2::t2 when x1 < x2 -> x1 :: loop (t1, x2::t2)
      | x1::t1, x2::t2 when x1 > x2 -> loop (x1::t1, t2)
      | _::t1, _::t2 -> loop (t1, t2) in
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

  let mem key value t =
    try List_.sort_mem value (M.find key t)
    with Not_found -> false

  let  mem_key key t = M.mem key t

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
module Id = struct
  type t = int

  type name = string
  type table = string array

  let build ?(loc:Loc.t option) key table =
    try Array_.dicho_find key table
    with Not_found -> Error.build ?loc "[Id.build] Identifier '%s' not found" key

  let build_opt key table =
    try Some (Array_.dicho_find key table)
    with Not_found -> None

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

  let get_loc () = !current_loc
  let loc_string () = Loc.to_string !current_loc

  let get_line_opt () = snd (get_loc ())

  let new_file filename =
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

  (* store the most recent timestamp of a file after last grs loading *)
  let grs_timestamp = ref Float.min_float

  let reset_grs_timestamp () =
    grs_timestamp := Float.min_float
  let update_grs_timestamp file =
    let stat = Unix.stat file in
    let ts = stat.Unix.st_mtime in
    if ts > !grs_timestamp then grs_timestamp := ts

  let get_grs_timestamp () = !grs_timestamp
end (* module Global *)

(* ================================================================================ *)
module Dependencies = struct
  let lex_cmp (i1, j1) (i2,j2) = match Stdlib.compare i1 i2 with 0 -> Stdlib.compare j1 j2 | x -> x

  let rec insert_sorted i = function
    | h::t when h < i -> h :: (insert_sorted i t)
    | l -> i::l

  let is_projective edge_list =
    let sorted_edge_list = List.sort lex_cmp edge_list in
    let rec loop position from_here from_before = function
      | [] ->
        true
      | (i,j) :: tail ->
        let rec reduce_from_before = function
          | h::t when h <= i -> reduce_from_before t
          | l -> l in
        let (new_from_here, new_from_before) =
          if i > position
          then ([], reduce_from_before (from_here @ from_before))
          else (from_here, reduce_from_before from_before) in
        match new_from_before with
        | h::_ when j > h -> false
        | h::_ when j = h -> loop i new_from_here new_from_before tail
        | _ -> loop i (insert_sorted j new_from_here) new_from_before tail in
    loop 0 [] [] sorted_edge_list

end

(* ================================================================================ *)
module Pid = struct
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

end (* module Pid_map *)

(* ================================================================================ *)
module Pid_massoc = Massoc_make (Pid)

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
module Gid_massoc = Massoc_make (Gid)

(* ================================================================================ *)
module Feature_value = struct
  type t =
    | String of string
    | Float of float

  (* Typing float/string for feature value is hardcoded, should evolve with a new config implementation *)
  let numeric_feature_values = [
    "level";                        (* used for edges in UDtoSUD grs *)
    "freq"; "freq_old"; "freq_new"; (* used for nodes in POStoSSQ grs *)
    "_start"; "_stop";              (* nodes in Orfeo timestamps *)
    "AlignBegin"; "AlignEnd";       (* nodes in SUD_Naija *)
    "length"; "delta"; "weight";
    "Duration";
    "MeanF0";
    "SemitonesFromUtteranceMean";
    "AvgAmplitude";
    "AvgAmplitudeNormalized";
    "MaxAmplitude";
    "MaxAmplitudeNormalized";
    "MeanF0Normalized";
    "DurationNormalized";
    "MeanF0Normalized";
    "MeanF0NormalizedGlobal";
    "MaxAmplitudeNormalizedGlobal";
    "DurationNormalizedGlobal";
    "AvgAmplitudeNormalizedGlobal";
    "DurationGlobalZscore";
    "DurationLocalZscore";
    "MeanF0GlobalZscore";
    "MeanF0LocalZscore";
    "AvgAmplitudeGlobalZscore";
    "AvgAmplitudeLocalZscore";
    "MaxAmplitudeGlobalZscore";
    "MaxAmplitudeLocalZscore";
  ]

  let parse ?loc feature_name string_value =
    if List.mem feature_name numeric_feature_values
    then
      begin
        match float_of_string_opt string_value with
        | Some f -> Float f
        | None -> Error.run ?loc "The feature \"%s\" must be numeric, it cannot be associated with value: \"%s\"" feature_name string_value
      end
    else String (string_value |> String_.nfc)

  let to_string ?(quote=false)= function
    | String s -> s
      |> Str.global_replace (Str.regexp "\"") "\\\""
      |> sprintf (if quote then "\"%s\"" else "%s")
    | Float f -> 
      String_.of_float_clean f

  let to_json = function
    | String s -> `String s
    | Float f -> `String (String_.of_float_clean f)

  let extract_range ?loc range = function
    | String s -> String (Range.extract range s)
    | Float f when range = (None, None) -> Float f
    | Float f -> Error.run ?loc "Cannot extract substring from a numeric feature \"%g\"" f

  let concat ?loc = function
    | [one] -> one
    | l ->
      let rec loop = function
        | [] -> ""
        | String s :: tail -> s ^ (loop tail)
        | Float _ :: _ -> Error.run ?loc "Cannot concat with numeric value" in
      String (loop l)
    end (* module Feature_value *)

(* ================================================================================ *)
module Sbn = struct
  type state = Blank | Token of int | String of int

  let parse_line l =
    let l = l  |> String_.nfc in
    let stack = ref [] in
    let rec loop state pos =
      match (state, l.[pos]) with
      | (_, '%') -> ()
      | (Blank, '"') -> loop (String pos) (pos + 1)
      | (Blank, c) when c <> ' ' -> loop (Token pos) (pos + 1)
      | (Token i, ' ') -> stack := String.sub l i (pos-i) :: !stack; loop Blank (pos+1)
      | (String i, '"') -> stack := String.sub l i (pos-i+1) :: !stack; loop Blank (pos+1)
      | _ -> loop state (pos+1)
    in loop (Token 0) 0;
    List.rev !stack

  type node = {
    index: int;
    feats: (string * string) list;
    concept: string;
    box: int;
  }

  (* may be between boxes or between nodes *)
  type rel = {
    src: int;
    label: string;
    tar: int;
  }

  type graph = {
    meta: (string * string) list;
    sem_nodes: node list;
    sem_edges: rel list;
    box_edges: rel list;
    box_number: int;
  }
  let init = { meta=[]; box_number = 1; sem_nodes = []; sem_edges = []; box_edges = []; }

  let graph_to_json graph =
    let cpt = ref 0 in (* negative counter! *)
    let meta = `Assoc (List.map (fun (f,v) -> (f, `String v)) graph.meta) in
    let (sem_nodes, (value_nodes, value_edges)) =
      List.fold_left
        (fun (acc_sem_nodes, (acc_value_nodes, acc_value_edges)) node ->
          (
           (string_of_int node.index, `Assoc [("concept",`String node.concept)]) :: acc_sem_nodes,
           List.fold_left
            (fun (avn,ave) (f,v) ->
              incr cpt;
              let vid = Printf.sprintf "V%d" !cpt in
                (
                  (
                    vid, `Assoc [("value",`String v)]) :: avn,
                  `Assoc [("src",`String (string_of_int node.index)); ("label",`String f); ("tar",`String vid)] ::ave
                )
            ) (acc_value_nodes,acc_value_edges) node.feats
          )
      ) ([],([],[])) graph.sem_nodes in

    let box_nodes = List.init graph.box_number
        (fun i ->
           let name = "B"^(string_of_int (i+1)) in
           (name, `Assoc [("label", `String name)])
        ) in
    let sem_edges = List.map
        (fun edge -> `Assoc [("src",`String (string_of_int edge.src)); ("label",`String edge.label); ("tar",`String (string_of_int edge.tar))]
        ) graph.sem_edges in
    let box_edges = List.map
        (fun edge -> `Assoc [("src",`String ("B"^(string_of_int edge.src))); ("label",`String edge.label); ("tar",`String ("B"^(string_of_int edge.tar)))]
        ) graph.box_edges in
    let mix_edges = List.map
        (fun node ->
           `Assoc [("src",`String ("B"^(string_of_int node.box))); ("label",`String "in"); ("tar",`String (string_of_int node.index))]
        ) graph.sem_nodes in

    `Assoc [
      ("meta", meta);
      ("nodes", `Assoc (sem_nodes @ box_nodes @ value_nodes));
      ("edges", `List (sem_edges @ box_edges @ mix_edges @ value_edges));
    ]


  let graph_of_lines lines =
    List.fold_left
      (fun (node_index, acc_graph) line ->
         match parse_line line with
         (* Nothing before the first '%' --> skip the line *)
         | [] -> (node_index, acc_graph)

         (* Line statrting with white spaces (= empty token) --> relations between boxes *)
         | [""; box_rel; index] ->
           let new_box_index = acc_graph.box_number + 1 in
           let new_box_edge = match int_of_string_opt index with
             | Some i -> {src=new_box_index+i;label=box_rel;tar=new_box_index}
             | None -> failwith ("Box ref not int: " ^ line)
           in
           (node_index, {acc_graph with box_edges = new_box_edge :: acc_graph.box_edges; box_number = new_box_index })

         (* Non empty token --> relations between sem_nodes *)
         | node :: relations ->
           let new_node_index = node_index + 1 in
           let rec loop = function
             | [] -> ([],[])
             | label::tar::tail ->
               let (acc_e, acc_f) = loop tail in
               begin
                 match (tar.[0], int_of_string_opt tar) with
                 | ('+', Some i) | ('-', Some i) -> ({ src= new_node_index; label; tar=new_node_index+i }::acc_e, acc_f)
                 | _ -> (acc_e, (label,tar) :: acc_f)
               end
             | _ -> failwith ("odd:" ^ line) in
           let (new_node_edges, feats) = loop relations in
           let new_node = { index = new_node_index; concept= node; feats; box= acc_graph.box_number } in
           (new_node_index, {acc_graph with sem_nodes = new_node :: acc_graph.sem_nodes; sem_edges = new_node_edges @ acc_graph.sem_edges})
      ) (0, init) lines
    |> snd

  let parse sbn_string =
    let lines = Str.split (Str.regexp "\n") sbn_string in
    graph_of_lines lines
  
  let to_json sbn_string = sbn_string |> parse |> graph_to_json
end