open Printf
open Log

open Utils
open Ast
open Grew_fs
open Grew_edge

module Command  = struct 
  type pid = int (* the int in the pattern *)
  type gid = int (* the int in the graph *)
	
  type cnode =       (* a command node is either: *)
    | Pid of pid     (* a node identified in the pattern *)
    | New of string  (* a node introduced by a new_neighbour *)

  (* the command in pattern *)
  type p = 
    | DEL_NODE of cnode
    | DEL_EDGE_EXPL of (cnode * cnode *Edge.t) 
    | DEL_EDGE_NAME of string
    | ADD_EDGE of (cnode * cnode * Edge.t)
    | COPY_FEAT of (cnode * cnode * string * string) 
    | CONCAT_FEAT of (cnode * cnode * cnode * string * string * string) 
    | ADD_FEAT of (cnode * string * string)
    | DEL_FEAT of (cnode * string)
    | NEW_NEIGHBOUR of (string * Edge.t * pid)
    | SHIFT_EDGE of (cnode * cnode)
    | MERGE_NODE of (cnode * cnode)

  type t = p * Loc.t  (* remember command location to be able to localize a command failure *)

  (* a item in the command history: command applied to a graph *)
  type h = 
    | H_DEL_NODE of gid
    | H_DEL_EDGE_EXPL of (gid * gid *Edge.t) 
    | H_DEL_EDGE_NAME of string
    | H_ADD_EDGE of (gid * gid * Edge.t)
    | H_COPY_FEAT of (gid * gid * string * string) 
    | H_CONCAT_FEAT of (gid * gid * gid * string * string * string) 
    | H_ADD_FEAT of (gid * string * string)
    | H_DEL_FEAT of (gid *string)
    | H_NEW_NEIGHBOUR of (string * Edge.t * gid)
    | H_SHIFT_EDGE of (gid * gid)
    | H_MERGE_NODE of (gid * gid)

  let parse_feat loc string_feat = 
    match Str.split (Str.regexp "\\.") string_feat with
    | [node; feat_name] -> (node, feat_name)
    | _ -> Log.fcritical "[GRS] \"%s\" is not a feature %s" string_feat (Loc.to_string loc)

  let build ?domain table locals ast_command = 
    let get_pid node_name =
      match Id.build_opt node_name table with
      | Some id -> Pid id
      | None -> New node_name in

    match ast_command with
    | (Ast.Del_edge_expl (i, j, lab), loc) ->
	let edge = Edge.make ~locals [lab] in
	(DEL_EDGE_EXPL (get_pid i, get_pid j, edge), loc)
	  
    | (Ast.Del_edge_name id, loc) -> 
	(DEL_EDGE_NAME id, loc)
	  
    | (Ast.Add_edge (i, j, lab), loc) ->
	let edge = Edge.make ~locals [lab] in
	(ADD_EDGE (get_pid i, get_pid j, edge), loc)
	  
    | (Ast.Shift_edge (i, j), loc) ->
	(SHIFT_EDGE (get_pid i, get_pid j), loc)

    | (Ast.Merge_node (i, j), loc) ->
	(MERGE_NODE (get_pid i, get_pid j), loc)
	  
    | (Ast.New_neighbour (name_created, ancestor, label), loc) -> 
	let edge = Edge.make ~locals [label] in
	begin
	  try (NEW_NEIGHBOUR (name_created, edge, Id.build ~loc ancestor table), loc)
	  with Not_found -> 
	    Log.fcritical "[GRS] tries to build a command New_neighbour (%s) on node %s which is not in the pattern %s"
	      (Edge.to_string edge)
	      ancestor
	      (Loc.to_string loc)
	end
	  
    | (Ast.Del_node n, loc) -> 
	(DEL_NODE (get_pid n), loc)
	  
    | (Ast.New_feat (feat, value), loc) ->
        let (node, feat_name) = parse_feat loc feat in
        (ADD_FEAT (get_pid node, feat_name, value), loc)

    | (Ast.Copy_feat (feat1, feat2), loc) ->
        let (node_1, feat_name_1) = parse_feat loc feat1 
        and (node_2, feat_name_2) = parse_feat loc feat2 in
        (COPY_FEAT (get_pid node_1, get_pid node_2, feat_name_1, feat_name_2), loc)

    | (Ast.Concat_feat (feat1, feat2, feat3), loc) ->
        let (node_1, feat_name_1) = parse_feat loc feat1 
        and (node_2, feat_name_2) = parse_feat loc feat2 
        and (node_3, feat_name_3) = parse_feat loc feat3 in
        (CONCAT_FEAT (get_pid node_1, get_pid node_2, get_pid node_3, feat_name_1, feat_name_2, feat_name_3), loc)

    | (Ast.Del_feat (feat), loc) ->
        let (node, feat_name) = parse_feat loc feat in
        (DEL_FEAT (get_pid node, feat_name), loc)
end
