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

open Grew_base

type feature_name = string (* cat, num, ... *)
type feature_atom = string (* V, N, inf, ... *)
type feature_value = string (* V, 4, "free text", ... *)
type suffix = string

type value = String of string | Float of float

let string_of_value = function
  | String s -> Str.global_replace (Str.regexp "\"") "\\\""
    (Str.global_replace (Str.regexp "\\\\") "\\\\\\\\" s)
  | Float i -> String_.of_float i

let conll_string_of_value = function
  | String s -> s
  | Float i -> String_.of_float i

type disjunction = value list

(* ================================================================================ *)
module Pid = struct
  (* type t = int *)
  type t = Pos of int | Neg of int

  let compare = Pervasives.compare

  let to_id = function
    | Pos i -> sprintf "p_%d" i
    | Neg i -> sprintf "n_%d" i

  let to_string = function
    | Pos i -> sprintf "Pos %d" i
    | Neg i -> sprintf "Neg %d" i
end (* module Pid *)

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
module Pid_set = Set.Make (Pid)

(* ================================================================================ *)
module Gid = struct
  type t = int

  let compare = Pervasives.compare

  let to_string i = sprintf "%d" i
end (* module Gid *)

(* ================================================================================ *)
module Gid_map = Map.Make (Gid)

(* ================================================================================ *)
module Massoc_gid = Massoc_make (Gid)

(* ================================================================================ *)
module Massoc_pid = Massoc_make (Pid)

(* ================================================================================ *)
module Massoc_string = Massoc_make (String)


(* ================================================================================ *)
(* This module defines a type for lexical parameter (i.e. one line in a lexical file) *)
module Lex_par = struct

  type item = string list

  let item_to_string l = String.concat "#" l

  type t = item list

  let to_json t =
    `List (List.map (fun item -> `String (item_to_string item)) t)

  let size = List.length
  let append = List.append

  let signature = function
    | [] -> Error.bug "[Lex_par.signature] empty data"
    | v -> List.length v

  let dump t =
    printf "[Lex_par.dump] --> size = %d\n" (List.length t);
    List.iter (fun il -> printf "%s\n" (String.concat "#" il)) t

  let parse_line ?loc nb_var line =
    let line = String_.rm_peripheral_white line in
    if line = "" || line.[0] = '%'
    then None
    else
      let line = Str.global_replace (Str.regexp "\\\\%") "%" line in
      match Str.split (Str.regexp "##\\|#") line with
        | args when List.length args = nb_var -> Some args
        | args -> Error.build ?loc "Wrong param number: '%d instead of %d'" (List.length args) nb_var

  let from_lines ?loc nb_var lines =
    match List_.opt_map (parse_line ?loc nb_var) lines with
    | [] -> Error.build ?loc "Empty lexical parameter list"
    | l -> l

  let load ?loc dir nb_var file =
    try
      let full_file =
        if Filename.is_relative file
        then Filename.concat dir file
        else file in
      let lines = File.read full_file in
      match List_.opt_mapi (fun i line -> parse_line ~loc:(Loc.file_line full_file i) nb_var line) lines with
      | [] -> Error.build ?loc "Empty lexical parameter file '%s'" file
      | l -> l
    with Sys_error _ -> Error.build ?loc "External lexical file '%s' not found" file

  let select index atom t =  List.filter (fun par -> List.nth par index = atom) t

  let get_param_value index = function
    | [] -> Error.bug "[Lex_par.get_param_value] empty parameter"
    | params::_ -> List.nth params index

  let get_command_value index = function
    | [] -> Error.bug "[Lex_par.get_command_value] empty parameter"
    | [one] -> List.nth one index
    | _ -> Error.run "Lexical parameter are not functional"
end (* module Lex_par *)

(* ================================================================================ *)
module Lexicon = struct
  module Line_set = Set.Make (struct type t=string list let compare = Pervasives.compare end)

  type t = {
    header: string list;  (* ordered list of column headers *)
    lines: Line_set.t;
  }

  let rec transpose = function
  | []             -> []
  | []   :: xss    -> transpose xss
  | (x::xs) :: xss -> (x :: List.map List.hd xss) :: transpose (xs :: List.map List.tl xss)

  let build items =
    let tr = transpose items in
    let sorted_tr = List.sort (fun l1 l2 -> Pervasives.compare (List.hd l1) (List.hd l2)) tr in
    match transpose sorted_tr with
    | [] -> Error.bug "[Lexicon.build] inconsistent data"
    | header :: lines_list -> { header; lines = List.fold_right Line_set.add lines_list Line_set.empty }

  let load file =
    let lines = File.read file in
    let items = List.map (fun line -> Str.split (Str.regexp "\\t") line) lines in
    build items

  let reduce sub_list lexicon =
    let sorted_sub_list = List.sort Pervasives.compare sub_list in
    let reduce_line line =
      let rec loop = function
      | ([],_,_) -> []
      | (hs::ts, hh::th, hl::tl)    when hs=hh    -> hl::(loop (ts,th,tl))
      | (hs::ts, hh::th, hl::tl)    when hs>hh    -> loop (hs::ts, th, tl)
      | (hs::ts, hh::th, hl::tl) (* when hs<hh *) -> Error.bug "[Lexicon.reduce] Field '%s' not in lexicon" hs
      | (hs::ts, [], []) -> Error.bug "[Lexicon.reduce] Field '%s' not in lexicon" hs
      | _ -> Error.bug "[Lexicon.reduce] Inconsistent length" in
      loop (sorted_sub_list, lexicon.header, line) in
    let new_lines = Line_set.map reduce_line lexicon.lines in
    { header = sorted_sub_list; lines = new_lines }

  let union lex1 lex2 =
    if lex1.header <> lex2.header then Error.build "[Lexcion.union] different header";
    { header = lex1.header; lines = Line_set.union lex1.lines lex2.lines }

  let select head value lex =
    match List_.index head lex.header with
    | None -> Error.build "[Lexicon.select] cannot find %s in lexicon" head
    | Some index ->
      { lex with lines = Line_set.filter (fun line -> List.nth line index = value) lex.lines}

  let projection head lex =
    match List_.index head lex.header with
    | None -> Error.build "[Lexicon.projection] cannot find %s in lexicon" head
    | Some index ->
      Line_set.fold (fun line acc -> String_set.add (List.nth line index) acc) lex.lines String_set.empty

  exception Not_functional_lexicon
  let read head lex =
    match String_set.elements (projection head lex) with
    | [] -> None
    | [one] -> Some one
    | _ -> raise Not_functional_lexicon

  let read_multi head lex =
    match String_set.elements (projection head lex) with
    | [] -> None
    | l -> Some (String.concat "/" l)
end (* module Lexicon *)

(* ================================================================================ *)
module Concat_item = struct
  type t =
    | Feat of (Gid.t * feature_name)
    | String of string
end (* module Concat_item *)
