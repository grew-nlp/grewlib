open Printf
open Log

open Grew_utils
open Grew_ast
open Grew_edge
open Grew_fs

(* ==================================================================================================== *)
module Command  = struct 
  type cnode =               (* a command node is either: *)
    | Pat of Pid.t           (* a node identified in the pattern *)
    | New of string          (* a node introduced by a new_neighbour *)
    | Act of Pid.t * string  (* a node introduced by a activate *)

  type item =
    | Feat of (cnode * string)
    | String of string
    | Param_in of int
    | Param_out of int

  (* the command in pattern *)
  type p = 
    | DEL_NODE of cnode
    | DEL_EDGE_EXPL of (cnode * cnode * G_edge.t) 
    | DEL_EDGE_NAME of string
    | ADD_EDGE of (cnode * cnode * G_edge.t)
    | DEL_FEAT of (cnode * string)
    | UPDATE_FEAT of (cnode * string * item list)
    | NEW_NEIGHBOUR of (string * G_edge.t * Pid.t)
    | SHIFT_EDGE of (cnode * cnode)
    | SHIFT_IN of (cnode * cnode)
    | SHIFT_OUT of (cnode * cnode)
    | MERGE_NODE of (cnode * cnode)

  type t = p * Loc.t  (* remember command location to be able to localize a command failure *)

  (* a item in the command history: command applied to a graph *)
  type h = 
    | H_DEL_NODE of Gid.t
    | H_DEL_EDGE_EXPL of (Gid.t * Gid.t *G_edge.t)
    | H_DEL_EDGE_NAME of string
    | H_ADD_EDGE of (Gid.t * Gid.t * G_edge.t)
    | H_DEL_FEAT of (Gid.t * string)
    | H_UPDATE_FEAT of (Gid.t * string * string)
    | H_NEW_NEIGHBOUR of (string * G_edge.t * Gid.t)
    | H_SHIFT_EDGE of (Gid.t * Gid.t)
    | H_SHIFT_IN of (Gid.t * Gid.t)
    | H_SHIFT_OUT of (Gid.t * Gid.t)
    | H_MERGE_NODE of (Gid.t * Gid.t)

  let build ?param (kci, kei) table locals ast_command =

    let pid_of_c_ident = function
        | (node_name, None) -> Pat (Pid.Pos (Id.build node_name table))
        | (node_name, Some n) ->  Act (Pid.Pos (Id.build node_name table), n) in

    let check_c_ident loc c_ident kci =
      if not (List.mem c_ident kci)
      then Error.build ~loc "Unbound c_ident identifier \"%s\"" (Ast.c_ident_to_string c_ident) in

    let check_edge loc edge_id kei = 
      if not (List.mem edge_id kei) 
      then Error.build ~loc "Unbound edge identifier \"%s\"" edge_id in

    match ast_command with
      | (Ast.Del_edge_expl (i, j, lab), loc) ->
        check_c_ident loc i kci;
        check_c_ident loc j kci;
	let edge = G_edge.make ~locals lab in
	((DEL_EDGE_EXPL (pid_of_c_ident i, pid_of_c_ident j, edge), loc), (kci, kei))
	  
      | (Ast.Del_edge_name id, loc) ->
        check_edge loc id kei;
        (DEL_EDGE_NAME id, loc), (kci, List_.rm id kei)
	  
      | (Ast.Add_edge (i, j, lab), loc) ->
        check_c_ident loc i kci;
        check_c_ident loc j kci;
	let edge = G_edge.make ~locals lab in
	((ADD_EDGE (pid_of_c_ident i, pid_of_c_ident j, edge), loc), (kci, kei))

      | (Ast.Shift_edge (i, j), loc) ->
        check_c_ident loc i kci;
        check_c_ident loc j kci;
	((SHIFT_EDGE (pid_of_c_ident i, pid_of_c_ident j), loc), (kci, kei))

      | (Ast.Shift_in (i, j), loc) ->
        check_c_ident loc i kci;
        check_c_ident loc j kci;
	((SHIFT_IN (pid_of_c_ident i, pid_of_c_ident j), loc), (kci, kei))

      | (Ast.Shift_out (i, j), loc) ->
        check_c_ident loc i kci;
        check_c_ident loc j kci;
	((SHIFT_OUT (pid_of_c_ident i, pid_of_c_ident j), loc), (kci, kei))

      | (Ast.Merge_node (i, j), loc) ->
        check_c_ident loc i kci;
        check_c_ident loc j kci;
	((MERGE_NODE (pid_of_c_ident i, pid_of_c_ident j), loc), (List_.rm i kci, kei))

      | (Ast.New_neighbour ((name_created, None), ancestor, label), loc) ->
        check_c_ident loc ancestor kci;
        if List.mem (name_created, None) kci
        then Error.build ~loc "Node identifier \"%s\" is already used" name_created;
	let edge = G_edge.make ~locals label in
	begin
	  try
            (
              (NEW_NEIGHBOUR
                 (name_created,
                  edge,
                  Pid.Pos (Id.build ~loc (fst ancestor) table)
                 ), loc),
              ((name_created, None)::kci, kei)
            )
	  with Not_found -> 
	    Log.fcritical "[GRS] tries to build a command New_neighbour (%s) on node %s which is not in the pattern %s"
	      (G_edge.to_string edge)
	      (fst ancestor)
	      (Loc.to_string loc)
	end

      | (Ast.Activate n, loc) -> failwith "Not implemented"
	  
      | (Ast.Del_node n, loc) ->
        check_c_ident loc n kci;
	((DEL_NODE (pid_of_c_ident n), loc), (List_.rm n kci, kei))
	  
      | (Ast.Del_feat (c_ident,feat_name), loc) ->
        check_c_ident loc c_ident kci;
        ((DEL_FEAT (pid_of_c_ident c_ident, feat_name), loc), (kci, kei))

      | (Ast.Update_feat ((c_ident, feat_name), ast_items), loc) ->
        check_c_ident loc c_ident kci;
        let items = List.map 
          (function
            | Ast.Qfn_item (ci,fn) -> check_c_ident loc ci kci; Feat (pid_of_c_ident ci, fn)
            | Ast.String_item s -> String s
            | Ast.Param_item var ->
              match param with
                | None -> Error.build "Unknown command variable '%s'" var
                | Some (par,cmd) ->
                  match (List_.pos var par, List_.pos var cmd) with
                    | (_,Some index) -> Param_out index
                    | (Some index,_) -> Param_in index
                    | _ -> Error.build "Unknown command variable '%s'" var
          ) ast_items in
        ((UPDATE_FEAT (pid_of_c_ident c_ident, feat_name, items), loc), (kci, kei))
      | _ -> failwith "TODO remove with new neighbour"
end (* module Command *)
