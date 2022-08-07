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

let (<<) f g x = f (g x)

type cmp = Eq | Neq
let string_of_cmp = function Eq -> "=" | Neq -> "<>"
let cmp_fct cmp = match cmp with Eq -> (=) | Neq -> (<>)


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
    let prefix = match loc with Some l -> sprintf "[%s] " (Loc.to_string l) | None -> "" in
    ANSITerminal.eprintf [ANSITerminal.blue] "%s%s\n%!" prefix message;
    flush stderr
  let warning ?loc = Printf.ksprintf (warning_ ?loc)

  let info_ ?loc message =
    let prefix = match loc with Some l -> sprintf "[%s] " (Loc.to_string l) | None -> "" in
    ANSITerminal.eprintf [ANSITerminal.green] "%s%s\n%!" prefix message;
    flush stderr
  let info ?loc = Printf.ksprintf (info_ ?loc)

end (* module Error *)

(* ================================================================================ *)
module Range = struct
  type t = (int option * int option)

  let to_string = function
    | (None, None) -> ""
    | (Some x, None) -> sprintf "[%d:]" x
    | (None, Some y) -> sprintf "[:%d]" y
    | (Some x, Some y) -> sprintf "[%d:%d]" x y

  let to_json r = `String (to_string r) 
end

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

  (** Pyhton like substring extraction
     [get_range (init_opt, final_opt) s] return the python output of s[init_opt:final_opt]
     NB: indexes correspond to UTF-8 chars. ex: [get_range (None, Some (-1)) "été"] ==> "ét"
  *)
  let get_range (iopt,fopt) s =
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

