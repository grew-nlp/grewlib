(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2018 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: http://grew.fr                                                     *)
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
module Lexicon = struct
  module Line_set = Set.Make (struct type t=string list let compare = Pervasives.compare end)

  type t = {
    header: string list;  (* ordered list of column headers *)
    lines: Line_set.t;
    loc: Loc.t;
  }

  let rec transpose = function
  | []             -> []
  | []   :: xss    -> transpose xss
  | (x::xs) :: xss -> (x :: List.map List.hd xss) :: transpose (xs :: List.map List.tl xss)

  exception Equal of string
  let strict_compare x y =
    match Pervasives.compare x y with
    | 0 -> raise (Equal x)
    | x -> x

  let build loc items =
    let real_items = List.filter (fun (_,x) -> x <> "" && x.[0] <> '%') items in
    match real_items with
      | [] | [_] -> Error.build ~loc "[Lexicon.build] a lexicon must not be empty"
      | (linenum_h, h)::t ->
        let fields = Str.split (Str.regexp "\t") h in
        let l = List.length fields in
        let rec loop = function
          | [] -> []
          | (linenum, line)::tail ->
            let items = Str.split (Str.regexp "\t") line in
            if List.length items <> l then
              begin
                let loc = Loc.set_line linenum loc in
                Error.build ~loc "[Lexicon.build] line with %d items (%d expected!!)" (List.length items) l
              end;
             items :: (loop tail) in
        let items_list = fields ::(loop t) in
        let tr = transpose items_list in
        try
          let sorted_tr = List.sort (fun l1 l2 -> strict_compare (List.hd l1) (List.hd l2)) tr in
          match transpose sorted_tr with
            | [] -> Error.bug ~loc "[Lexicon.build] inconsistent data"
            | header :: lines_list -> { header; lines = List.fold_right Line_set.add lines_list Line_set.empty; loc }
        with Equal v ->
          let loc = Loc.set_line linenum_h loc in
          Error.build ~loc "[Lexicon.build] the field name \"%s\" is used twice" v

  let load file =
    let lines = File.read_ln file in
    let loc = Loc.file file in
    build loc lines

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
    { lexicon with header = sorted_sub_list; lines = new_lines }

  let union lex1 lex2 =
    if lex1.header <> lex2.header then Error.build "[Lexicon.union] different header";
    { lex1 with lines = Line_set.union lex1.lines lex2.lines }
    (* NOTE: the loc field of a union may be not accurate *)

  let select head value lex =
    match List_.index head lex.header with
    | None -> Error.build ~loc:lex.loc "[Lexicon.select] cannot find %s in lexicon" head
    | Some index ->
      let new_set = Line_set.filter (fun line -> List.nth line index = value) lex.lines in
      if Line_set.is_empty new_set
      then None
      else Some { lex with lines = new_set }

  let unselect head value lex =
    match List_.index head lex.header with
    | None -> Error.build ~loc:lex.loc "[Lexicon.unselect] cannot find the fiels \"%s\" in lexicon" head
    | Some index ->
      let new_set = Line_set.filter (fun line -> List.nth line index <> value) lex.lines in
      if Line_set.is_empty new_set
      then None
      else Some { lex with lines = new_set }

  let projection head lex =
    match List_.index head lex.header with
    | None -> Error.build ~loc:lex.loc "[Lexicon.projection] cannot find %s in lexicon" head
    | Some index -> Line_set.fold (fun line acc -> String_set.add (List.nth line index) acc) lex.lines String_set.empty

  exception Not_functional_lexicon
  let read head lex =
    match String_set.elements (projection head lex) with
    | [] -> Error.bug "[Lexicon.read] a lexicon must not be empty"
    | [one] -> one
    | _ -> raise Not_functional_lexicon

  let get head lex = String_set.choose (projection head lex)

  let read_multi head lex =
    match String_set.elements (projection head lex) with
    | [] -> Error.bug "[Lexicon.read] a lexicon must not be empty"
    | l -> String.concat "/" l
end (* module Lexicon *)

(* ================================================================================ *)
module Lexicons = struct
  type t = (string * Lexicon.t) list

 let check ~loc lexicon_name field_name t =
  try
    let lexicon = List.assoc lexicon_name t in
    if not (List.mem field_name lexicon.Lexicon.header)
    then Error.build ~loc "Undefined field name \"%s\" in lexicon %s" field_name lexicon_name
  with Not_found -> Error.build ~loc "Undefined lexicon name \"%s\"" lexicon_name

end

(* ================================================================================ *)
module Concat_item = struct
  type t =
    | Feat of (Gid.t * feature_name)
    | String of string
end (* module Concat_item *)
