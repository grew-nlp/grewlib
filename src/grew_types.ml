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
(* This module defines a type for lexical parameter (i.e. one line in a lexical file) *)
module Lex_par = struct

  type item = string list * string list (* first list: pattern parameters $id , second list command parameters @id *)

  type t = item list

  let size = List.length
  let append = List.append

  let signature = function
    | [] -> Error.bug "[Lex_par.signature] empty data"
    | (pp,cp)::_ -> (List.length pp,List.length cp)

  let dump t =
    printf "[Lex_par.dump] --> size = %d\n" (List.length t);
    List.iter (fun (pp,cp) ->
      printf "%s##%s\n"
        (String.concat "#" pp)
        (String.concat "#" cp)
    ) t

  let parse_line ?loc nb_p nb_c line =
    let line = String_.rm_peripheral_white line in
    if line = "" || line.[0] = '%'
    then None
    else
      let line = Str.global_replace (Str.regexp "\\\\%") "%" line in
      match Str.split (Str.regexp "##") line with
        | [args] when nb_c = 0 ->
          (match Str.split (Str.regexp "#") args with
            | l when List.length l = nb_p -> Some (l,[])
            | _ -> Error.build ?loc
              "Illegal lexical parameter line: \"%s\" doesn't contain %d args"
              line nb_p)
        | [args; values] ->
          (match (Str.split (Str.regexp "#") args, Str.split (Str.regexp "#") values) with
            | (lp,lc) when List.length lp = nb_p && List.length lc = nb_c -> Some (lp,lc)
            | _ -> Error.build ?loc
              "Illegal lexical parameter line: \"%s\" doesn't contain %d args and %d values"
              line nb_p nb_c)
        | _ -> Error.build ?loc "Illegal param line: '%s'" line

  let from_lines ?loc nb_p nb_c lines =
    match List_.opt_map (parse_line ?loc nb_p nb_c) lines with
    | [] -> Error.build ?loc "Empty lexical parameter list"
    | l -> l

  let load ?loc dir nb_p nb_c file =
    try
      let full_file =
        if Filename.is_relative file
        then Filename.concat dir file
        else file in
      let lines = File.read full_file in
      match List_.opt_mapi (fun i line -> parse_line ~loc:(Loc.file_line full_file i) nb_p nb_c line) lines with
      | [] -> Error.build ?loc "Empty lexical parameter file '%s'" file
      | l -> l
    with Sys_error _ -> Error.build ?loc "External lexical file '%s' not found" file

  let select index atom t =
    match
      List_.opt_map
        (fun (p_par, c_par) ->
          let par = List.nth p_par index in
          if atom = par
          then Some (p_par, c_par)
          else None
        ) t
    with
    | [] -> None
    | t -> Some t

  let get_param_value index = function
    | [] -> Error.bug "[Lex_par.get_param_value] empty parameter"
    | (params,_)::_ -> List.nth params index

  let get_command_value index = function
    | [(_,one)] -> List.nth one index
    | [] -> Error.bug "[Lex_par.get_command_value] empty parameter"
    | (_,[sing])::tail when index=0 ->
        Printf.sprintf "%s/%s"
          sing
          (List_.to_string
             (function
               | (_,[s]) -> s
               | _ -> Error.bug "[Lex_par.get_command_value] inconsistent param"
             ) "/" tail
          )
    | (left,_)::_ ->
      Error.run "Lexical parameter are not functional, input parameter%s: %s"
        (if (List.length left) > 1 then "s" else "")
        (String.concat ", " left)
end (* module Lex_par *)

(* ================================================================================ *)
module Concat_item = struct
  type t =
    | Feat of (Gid.t * feature_name)
    | String of string
end (* module Concat_item *)
