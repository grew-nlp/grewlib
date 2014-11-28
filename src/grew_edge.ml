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

  let to_string ?(locals=[||]) t = Label.to_string ~locals t

  let make ?loc ?(locals=[||]) string = Label.from_string ?loc ~locals string

  let build ?locals (ast_edge, loc) =
    match ast_edge.Ast.negative, ast_edge.Ast.edge_labels with
    | (false, [one]) -> Label.from_string ~loc ?locals one
    | (true, _) -> Error.build "Negative edge spec are forbidden in graphs%s" (Loc.to_string loc)
    | (false, _) -> Error.build "Only atomic edge valus are allowed in graphs%s" (Loc.to_string loc)

  let to_dep ?(deco=false) t = Label.to_dep ~deco t
  let to_dot ?(deco=false) t = Label.to_dot ~deco t

  let color_of_option = function
    | [] -> None
    | c::_ -> Some (String_.rm_first_char c)
end (* module G_edge *)

(* ================================================================================ *)
module P_edge = struct
  type u_label =
    | Pos of Label.t list
    | Neg of Label.t list

  type t = {
      id: string option; (* an identifier for naming under_label in patterns *)
      u_label: u_label;
    }

  let all = {id=None; u_label=Neg []}

  let get_id t = t.id

  let make ?loc ?(id=None) ?(neg=false) ?(locals=[||]) = function
    | l when neg -> {id=id; u_label=Neg (List.sort compare (List.map (Label.from_string ?loc ~locals) l))}
    | l -> {id=id; u_label=Pos (List.sort compare (List.map (Label.from_string ?loc ~locals) l))}

  let build ?locals (ast_edge, loc) =
    { id = ast_edge.Ast.edge_id;
      u_label =
      if ast_edge.Ast.negative
      then Neg (List.sort compare (List.map (Label.from_string ~loc ?locals) ast_edge.Ast.edge_labels))
      else Pos (List.sort compare (List.map (Label.from_string ~loc ?locals) ast_edge.Ast.edge_labels))
    }

  let to_string t =
    let pref = match t.id with None -> "" | Some i -> sprintf "%s:" i in
    match t.u_label with
    | Pos l -> pref^(List_.to_string Label.to_string "|" l)
    | Neg l -> pref^"^"^(List_.to_string Label.to_string "|" l)


  let compatible t g_edge = match t.u_label with
  | Pos p -> List_.sort_mem g_edge p
  | Neg n -> not (List_.sort_mem g_edge n)

  type edge_matcher =
    | Fail
    | Ok of Label.t
    | Binds of string * Label.t list

  let match_ pattern_edge graph_label =

    match pattern_edge with
    | {id = Some i; u_label = Pos l} when List.mem graph_label l -> Binds (i, [graph_label])
    | {id = None; u_label = Pos l} when List.mem graph_label l -> Ok graph_label
    | {id = Some i; u_label = Neg l} when not (List.mem graph_label l) -> Binds (i, [graph_label])
    | {id = None; u_label = Neg l} when not (List.mem graph_label l) -> Ok graph_label
    | _ -> Fail

  let match_list pattern_edge graph_edge_list =
    match pattern_edge with
    | {id = None; u_label = Pos l} when List.exists (fun label -> List.mem label l) graph_edge_list ->
        Ok (List.hd graph_edge_list)
    | {id = None; u_label = Neg l} when List.exists (fun label -> not (List.mem label l)) graph_edge_list ->
        Ok (List.hd graph_edge_list)
    | {id = Some i; u_label = Pos l} ->
	(match List.filter (fun label -> List.mem label l) graph_edge_list with
	| [] -> Fail
	| list -> Binds (i, list))
    | {id = Some i; u_label = Neg l} ->
	(match List.filter (fun label -> not (List.mem label l)) graph_edge_list with
	| [] -> Fail
	| list -> Binds (i, list))
    | _ -> Fail
end (* module P_edge *)
