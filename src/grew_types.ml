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

open Grew_base
open Grew_ast


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
module Massoc_gid = Massoc_make (Gid)

(* ================================================================================ *)
module Massoc_pid = Massoc_make (Pid)

(* ================================================================================ *)
module Massoc_string = Massoc_make (String)

(* ================================================================================ *)
module Lexicon = struct
  module Line_set = Set.Make (struct type t=string list let compare = Stdlib.compare end)

  type t = {
    header: string list;  (* ordered list of column headers *)
    lines: Line_set.t;
    loc: Loc.t option;
  }

  let rec transpose = function
    | []             -> []
    | []   :: xss    -> transpose xss
    | (x::xs) :: xss -> (x :: List.map List.hd xss) :: transpose (xs :: List.map List.tl xss)

  exception Equal of string
  let strict_compare x y =
    match Stdlib.compare x y with
    | 0 -> raise (Equal x)
    | x -> x

  (** [build loc items] build a lexicon from a list.
      The first list is interpreted as the column headers.
      All other lines are lexicon items.
      It is supposed that all sublist have the same length *)
  let of_item_list ?loc items =
    let real_items = List.filter (fun (_,x) -> x <> "" && x.[0] <> '%') items in
    match real_items with
    | [] | [_] -> Error.build ?loc "[Lexicon.of_ast] a lexicon must not be empty"
    | (linenum_h, h)::t ->
      let fields = Str.split (Str.regexp "\t") h in
      let l = List.length fields in
      let rec loop = function
        | [] -> []
        | (linenum, line)::tail ->
          let norm_line =
            if String.length line > 1 && line.[0] = '\\' && line.[1] = '%'
            then String_.rm_first_char line
            else line in
          let items = Str.split (Str.regexp "\t") norm_line in
          if List.length items <> l then
            begin
              let loc = CCOption.map (Loc.set_line linenum) loc in
              Error.build ?loc "[Lexicon.of_ast] line with %d items (%d expected!!)" (List.length items) l
            end;
          items :: (loop tail) in
      let items_list = fields ::(loop t) in
      let tr = transpose items_list in
      try
        let sorted_tr = List.sort (fun l1 l2 -> strict_compare (List.hd l1) (List.hd l2)) tr in
        match transpose sorted_tr with
        | [] -> Error.bug ?loc "[Lexicon.of_ast] inconsistent data"
        | header :: lines_list -> { header; lines = List.fold_right Line_set.add lines_list Line_set.empty; loc }
      with Equal v ->
        let loc = CCOption.map (Loc.set_line linenum_h) loc in
        Error.build ?loc "[Lexicon.of_ast] the field name \"%s\" is used twice" v

  let load ?loc file =
    try
      let lines = File.read_ln file in
      let loc = Loc.file file in
      of_item_list ~loc lines
    with Sys_error _ -> Error.build ?loc "[Lexicon.load] unable to load file %s" file

  let union lex1 lex2 =
    if lex1.header <> lex2.header then Error.build "[Lexicon.union] different header";
    { lex1 with lines = Line_set.union lex1.lines lex2.lines }
  (* NOTE: the loc field of a union may be not accurate *)

  let filter_opt cmp head value lex =
    match List_.index_opt head lex.header with
    | None -> Error.build ?loc:lex.loc "[Lexicon.filter_opt] cannot find the fiels \"%s\" in lexicon" head
    | Some index ->
      let new_set = Line_set.filter (fun line -> cmp_fct cmp (List.nth line index) value) lex.lines in
      if Line_set.is_empty new_set
      then None
      else Some { lex with lines = new_set }

  let projection head lex =
    match List_.index_opt head lex.header with
    | None -> Error.build ?loc:lex.loc "[Lexicon.projection] cannot find %s in lexicon" head
    | Some index -> Line_set.fold (fun line acc -> String_set.add (List.nth line index) acc) lex.lines String_set.empty

  let read_all head lex =
    match String_set.elements (projection head lex) with
    | [] -> Error.bug "[Lexicon.read] a lexicon must not be empty"
    | l -> l

  let get_opt head lex = String_set.choose_opt (projection head lex)

  let read_multi head lex =
    match String_set.elements (projection head lex) with
    | [] -> Error.bug "[Lexicon.read] a lexicon must not be empty"
    | l -> String.concat "/" l

  let of_ast ?loc dir_opt = function
    | Ast.File filename ->
      let dir = match dir_opt with Some s -> s | None -> Sys.getcwd () in
      if Filename.is_relative filename
      then load ?loc (Filename.concat dir filename)
      else load ?loc filename
    | Ast.Final (line_list) -> of_item_list ?loc line_list
end (* module Lexicon *)

(* ================================================================================ *)
module Lexicons = struct
  type t = (string * Lexicon.t) list

  let check ?loc lexicon_name field_name t =
    match List.assoc_opt lexicon_name t with
    | None ->
      Error.build ?loc "Undefined lexicon name \"%s\"" lexicon_name
    | Some lexicon when not (List.mem field_name lexicon.Lexicon.header) ->
      Error.build ?loc "Undefined field name \"%s\" in lexicon %s" field_name lexicon_name
    | _ -> ()
end (* module Lexicons *)

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