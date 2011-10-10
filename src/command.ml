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

  type item =
    | Feat of (cnode * string)
    | String of string

  (* the command in pattern *)
  type p = 
    | DEL_NODE of cnode
    | DEL_EDGE_EXPL of (cnode * cnode *Edge.t) 
    | DEL_EDGE_NAME of string
    | ADD_EDGE of (cnode * cnode * Edge.t)
    | DEL_FEAT of (cnode * string)
    | UPDATE_FEAT of (cnode * string * item list)
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
    | H_DEL_FEAT of (gid *string)
    | H_UPDATE_FEAT of (gid * string * string)
    | H_NEW_NEIGHBOUR of (string * Edge.t * gid)
    | H_SHIFT_EDGE of (gid * gid)
    | H_MERGE_NODE of (gid * gid)

  let build ?domain (kni, kei) table locals ast_command = 
    let get_pid node_name =
      match Id.build_opt node_name table with
      | Some id -> Pid id
      | None -> New node_name in

    let check_node loc node_id kni = 
      if not (List.mem node_id kni) 
      then Error.build ~loc "Unbound node identifier \"%s\"" node_id in

    let check_edge loc edge_id kei = 
      if not (List.mem edge_id kei) 
      then Error.build ~loc "Unbound edge identifier \"%s\"" edge_id in

    match ast_command with
    | (Ast.Del_edge_expl (i, j, lab), loc) ->
        check_node loc i kni; check_node loc j kni;
	let edge = Edge.make ~locals [lab] in
	((DEL_EDGE_EXPL (get_pid i, get_pid j, edge), loc), (kni, kei))
	  
    | (Ast.Del_edge_name id, loc) -> 
        check_edge loc id kei;
        (DEL_EDGE_NAME id, loc), (kni, List_.rm id kei)
	  
    | (Ast.Add_edge (i, j, lab), loc) ->
        check_node loc i kni; check_node loc j kni;
	let edge = Edge.make ~locals [lab] in
	((ADD_EDGE (get_pid i, get_pid j, edge), loc), (kni, kei))
	  
    | (Ast.Shift_edge (i, j), loc) ->
        check_node loc i kni; check_node loc j kni;
	((SHIFT_EDGE (get_pid i, get_pid j), loc), (kni, kei))

    | (Ast.Merge_node (i, j), loc) ->
        check_node loc i kni; check_node loc j kni;
	((MERGE_NODE (get_pid i, get_pid j), loc), (List_.rm i kni, kei))
	  
    | (Ast.New_neighbour (name_created, ancestor, label), loc) -> 
        check_node loc ancestor kni;
        if List.mem name_created kni
        then Error.build ~loc "Node identifier \"%s\" is already used" name_created;
	let edge = Edge.make ~locals [label] in
	begin
	  try ((NEW_NEIGHBOUR (name_created, edge, Id.build ~loc ancestor table), loc), (name_created::kni, kei))
	  with Not_found -> 
	    Log.fcritical "[GRS] tries to build a command New_neighbour (%s) on node %s which is not in the pattern %s"
	      (Edge.to_string edge)
	      ancestor
	      (Loc.to_string loc)
	end
	  
    | (Ast.Del_node n, loc) ->
        check_node loc n kni;
	((DEL_NODE (get_pid n), loc), (List_.rm n kni, kei))
	  
    | (Ast.Del_feat (node,feat_name), loc) ->
        check_node loc node kni;
        ((DEL_FEAT (get_pid node, feat_name), loc), (kni, kei))

    | (Ast.Update_feat ((tar_node, tar_feat_name), ast_items), loc) ->
        check_node loc tar_node kni;
        let items = List.map 
            (function
              | Ast.Qfn_item (node,feat_name) -> check_node loc node kni; Feat (get_pid node, feat_name)
              | Ast.String_item s -> String s) 
            ast_items in
        ((UPDATE_FEAT (get_pid tar_node, tar_feat_name, items), loc), (kni, kei))


end
