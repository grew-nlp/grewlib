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
    | CPY_FEAT of (cnode * cnode * string) 
    | ADD_FEAT of (cnode * string * string)
    | DEL_FEAT of (cnode *string)
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
    | H_CPY_FEAT of (gid * gid * string) 
    | H_ADD_FEAT of (gid * string * string)
    | H_DEL_FEAT of (gid *string)
    | H_NEW_NEIGHBOUR of (string * Edge.t * gid)
    | H_SHIFT_EDGE of (gid * gid)
    | H_MERGE_NODE of (gid * gid)

 
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
	begin
	  match (Str.split (Str.regexp "\\.") feat ) with
	  | [node; feat_name] -> 
	      Feature.check ?domain loc feat_name [value];
	      (ADD_FEAT (get_pid node, feat_name, value), loc)
	  | _ -> Log.fcritical "[GRS] \"%s\" is not a valid feature %s" feat (Loc.to_string loc)
	end
	  
    | (Ast.Copy_feat (feat1, feat2), loc) ->
	begin
	  match Str.split (Str.regexp "\\.") feat1 with
	  | [node_1; feat_name_1] ->
	      begin
		match Str.split (Str.regexp "\\.") feat2 with
		| [node_2; feat_name_2] when feat_name_1 = feat_name_2 -> (CPY_FEAT (get_pid node_1, get_pid node_2, feat_name_1), loc)
		| [node_2; feat_name_2] -> Log.fcritical "[GRS] Copy feat through different feature name not implemented %s" (Loc.to_string loc)
		| _ -> Log.fcritical "[GRS] \"%s\" is not a feature %s" feat2 (Loc.to_string loc)
	      end
	  | _ -> Log.fcritical "[GRS] \"%s\" is not a feature %s" feat1 (Loc.to_string loc)
	end

    | (Ast.Del_feat (feat), loc) ->
	begin
	  match Str.split (Str.regexp "\\.") feat with
	  | [node; feat_name] -> (DEL_FEAT (get_pid node, feat_name), loc)
	  | _ -> Log.fcritical "[GRS] \"%s\" is not a feature %s" feat (Loc.to_string loc)
	end
end
