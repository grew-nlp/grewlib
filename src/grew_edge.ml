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
open Grew_types
open Grew_ast

(* ================================================================================ *)
(** The module [Label_cst] defines contraints on label edges *)
module Label_cst = struct
  type t =
  | Pos of Label.t list
  | Neg of Label.t list
  | Regexp of (Str.regexp * string)

  let to_string ?domain = function
    | Pos l -> (List_.to_string (Label.to_string ?domain) "|" l)
    | Neg l -> "^"^(List_.to_string (Label.to_string ?domain) "|" l)
    | Regexp (_,re) -> "re\""^re^"\""

  let all = Neg []

  let match_ ?domain cst g_label = match cst with
    | Pos labels -> Label.match_list labels g_label
    | Neg labels -> not (Label.match_list labels g_label)
    | Regexp (re,_) -> String_.re_match re (Label.to_string ?domain g_label)

  let build ?loc ?domain = function
    | Ast.Neg_list p_labels -> Neg (List.sort compare (List.map (Label.from_string ?loc ?domain) p_labels))
    | Ast.Pos_list p_labels -> Pos (List.sort compare (List.map (Label.from_string ?loc ?domain) p_labels))
    | Ast.Regexp re -> Regexp (Str.regexp re, re)
end (* module Label_cst *)

(* ================================================================================ *)
module G_edge = struct
  type t = Label.t

  let to_string ?domain t = Label.to_string ?domain t

  let make ?loc ?domain string = Label.from_string ?loc ?domain string

  let sub = make "__SUB__"

  let build ?domain (ast_edge, loc) =
    match ast_edge.Ast.edge_label_cst with
    | Ast.Pos_list [one] -> Label.from_string ~loc ?domain one
    | Ast.Neg_list _ -> Error.build "Negative edge spec are forbidden in graphs%s" (Loc.to_string loc)
    | Ast.Pos_list _ -> Error.build "Only atomic edge values are allowed in graphs%s" (Loc.to_string loc)
    | Ast.Regexp _ -> Error.build "Regexp are not allowed in graphs%s" (Loc.to_string loc)

  let is_void ?domain t = Label.is_void ?domain t
  let to_dep ?domain ?(deco=false) t = Label.to_dep ?domain ~deco t
  let to_dot ?domain ?(deco=false) t = Label.to_dot ?domain ~deco t

  let color_of_option = function
    | [] -> None
    | c::_ -> Some (String_.rm_first_char c)
end (* module G_edge *)

(* ================================================================================ *)
module P_edge = struct
  type t = {
    id: string; (* an identifier for naming under_label in patterns *)
    label_cst: Label_cst.t;
  }

  let cpt = ref 0
  let fresh_name () = incr cpt; sprintf "__e_%d__" !cpt

  let all = {id=fresh_name (); label_cst=Label_cst.all }

  let get_id t = t.id

  let build ?domain (ast_edge, loc) =
    { id = (match ast_edge.Ast.edge_id with Some s -> s | None -> fresh_name ());
      label_cst = Label_cst.build ~loc ?domain ast_edge.Ast.edge_label_cst
    }

  let to_string ?domain t =
    if String.length t.id > 1 && t.id.[0] = '_' && t.id.[1] = '_'
    then Label_cst.to_string ?domain t.label_cst
    else sprintf "%s:%s" t.id (Label_cst.to_string ?domain t.label_cst)

  type edge_matcher =
    | Fail
    | Binds of string * Label.t list

  let match_ ?domain p_edge g_edge =
    match p_edge with
    | {id; label_cst } when Label_cst.match_ ?domain label_cst g_edge -> Binds (id, [g_edge])
    | _ -> Fail

  let match_list ?domain p_edge g_edge_list =
    match p_edge with
      | {id; label_cst } ->
      ( match List.filter (fun g_edge -> Label_cst.match_ ?domain label_cst g_edge) g_edge_list with
        | [] -> Fail
        | list -> Binds (id, list)
      )
end (* module P_edge *)
