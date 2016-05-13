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
module G_edge = struct
  type t = Label.t

  let to_string label_domain ?(locals=[||]) t = Label.to_string label_domain ~locals t

  let make ?loc label_domain ?(locals=[||]) string = Label.from_string ?loc label_domain ~locals string

  let build label_domain ?locals (ast_edge, loc) =
    match ast_edge.Ast.edge_label_cst with
    | ([one], false) -> Label.from_string ~loc label_domain ?locals one
    | (_, true) -> Error.build "Negative edge spec are forbidden in graphs%s" (Loc.to_string loc)
    | (_, false) -> Error.build "Only atomic edge valus are allowed in graphs%s" (Loc.to_string loc)

  let to_dep label_domain ?(deco=false) t = Label.to_dep label_domain ~deco t
  let to_dot label_domain ?(deco=false) t = Label.to_dot label_domain ~deco t

  let color_of_option = function
    | [] -> None
    | c::_ -> Some (String_.rm_first_char c)
end (* module G_edge *)

(* ================================================================================ *)
module P_edge = struct
  type t = {
      id: string option; (* an identifier for naming under_label in patterns *)
      u_label: Label_cst.t;
    }

  let all = {id=None; u_label= Label_cst.all }

  let get_id t = t.id

  let build label_domain ?locals (ast_edge, loc) =
    { id = ast_edge.Ast.edge_id;
      u_label = Label_cst.build ~loc label_domain ?locals ast_edge.Ast.edge_label_cst
    }

  let to_string label_domain t =
    match t.id with
    | None -> Label_cst.to_string label_domain t.u_label
    | Some i -> sprintf "%s:%s" i (Label_cst.to_string label_domain t.u_label)

  type edge_matcher =
    | Fail
    | Ok of Label.t
    | Binds of string * Label.t list

  let match_ label_domain pattern_edge graph_label =
    match pattern_edge with
    | {id = Some i; u_label = Label_cst.Pos l} when Label.match_list label_domain l graph_label -> Binds (i, [graph_label])
    | {id = None; u_label = Label_cst.Pos l} when Label.match_list label_domain l graph_label -> Ok graph_label
    | {id = Some i; u_label = Label_cst.Neg l} when not (Label.match_list label_domain l graph_label) -> Binds (i, [graph_label])
    | {id = None; u_label = Label_cst.Neg l} when not (Label.match_list label_domain l graph_label) -> Ok graph_label
    | _ -> Fail

  let match_list label_domain pattern_edge graph_edge_list =
    match pattern_edge with
    | {id = None; u_label = Label_cst.Pos l} when List.exists (fun label -> Label.match_list label_domain l label) graph_edge_list ->
        Ok (List.hd graph_edge_list)
    | {id = None; u_label = Label_cst.Neg l} when List.exists (fun label -> not (Label.match_list label_domain l label)) graph_edge_list ->
        Ok (List.hd graph_edge_list)
    | {id = Some i; u_label = Label_cst.Pos l} ->
      ( match List.filter (fun label -> Label.match_list label_domain l label) graph_edge_list with
        | [] -> Fail
        | list -> Binds (i, list)
      )
    | {id = Some i; u_label = Label_cst.Neg l} ->
      ( match List.filter (fun label -> not (Label.match_list label_domain l label)) graph_edge_list with
        | [] -> Fail
        | list -> Binds (i, list)
      )
    | _ -> Fail
end (* module P_edge *)
