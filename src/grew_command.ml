open Printf
open Log

open Grew_utils
open Grew_ast
open Grew_edge
open Grew_fs

(* ==================================================================================================== *)
module Command  = struct 
  type command_node =        (* a command node is either: *)
    | Pat of Pid.t           (* a node identified in the pattern *)
    | New of string          (* a node introduced by a new_neighbour *)
    | Act of Pid.t * string  (* a node introduced by a activate *)

  type item =
    | Feat of (command_node * string)
    | String of string
    | Param_in of int
    | Param_out of int

  (* the command in pattern *)
  type p = 
    | DEL_NODE of command_node
    | DEL_EDGE_EXPL of (command_node * command_node * G_edge.t)
    | DEL_EDGE_NAME of string
    | ADD_EDGE of (command_node * command_node * G_edge.t)
    | DEL_FEAT of (command_node * string)
    | UPDATE_FEAT of (command_node * string * item list)
    | NEW_NEIGHBOUR of (string * G_edge.t * Pid.t)
    | SHIFT_EDGE of (command_node * command_node)
    | SHIFT_IN of (command_node * command_node)
    | SHIFT_OUT of (command_node * command_node)
    | MERGE_NODE of (command_node * command_node)

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

  let build ?param (kai, kei) table locals ast_command =

    let pid_of_act_id = function
        | (node_name, None) -> Pat (Pid.Pos (Id.build node_name table))
        | (node_name, Some n) -> Act (Pid.Pos (Id.build node_name table), n) in

    let check_act_id loc act_id kai =
      if not (List.mem act_id kai)
      then Error.build ~loc "Unbound node identifier \"%s\"" (Ast.act_id_to_string act_id) in

    let check_edge loc edge_id kei = 
      if not (List.mem edge_id kei) 
      then Error.build ~loc "Unbound edge identifier \"%s\"" edge_id in

    match ast_command with
      | (Ast.Del_edge_expl (act_i, act_j, lab), loc) ->
        check_act_id loc act_i kai;
        check_act_id loc act_j kai;
	let edge = G_edge.make ~loc ~locals lab in
	((DEL_EDGE_EXPL (pid_of_act_id act_i, pid_of_act_id act_j, edge), loc), (kai, kei))
	  
      | (Ast.Del_edge_name id, loc) ->
        check_edge loc id kei;
        (DEL_EDGE_NAME id, loc), (kai, List_.rm id kei)

      | (Ast.Add_edge (act_i, act_j, lab), loc) ->
        check_act_id loc act_i kai;
        check_act_id loc act_j kai;
	let edge = G_edge.make ~loc ~locals lab in
	((ADD_EDGE (pid_of_act_id act_i, pid_of_act_id act_j, edge), loc), (kai, kei))

      | (Ast.Shift_edge (act_i, act_j), loc) ->
        check_act_id loc act_i kai;
        check_act_id loc act_j kai;
	((SHIFT_EDGE (pid_of_act_id act_i, pid_of_act_id act_j), loc), (kai, kei))

      | (Ast.Shift_in (act_i, act_j), loc) ->
        check_act_id loc act_i kai;
        check_act_id loc act_j kai;
	((SHIFT_IN (pid_of_act_id act_i, pid_of_act_id act_j), loc), (kai, kei))

      | (Ast.Shift_out (act_i, act_j), loc) ->
        check_act_id loc act_i kai;
        check_act_id loc act_j kai;
	((SHIFT_OUT (pid_of_act_id act_i, pid_of_act_id act_j), loc), (kai, kei))

      | (Ast.Merge_node (act_i, act_j), loc) ->
        check_act_id loc act_i kai;
        check_act_id loc act_j kai;
	((MERGE_NODE (pid_of_act_id act_i, pid_of_act_id act_j), loc), (List_.rm act_i kai, kei))

      | (Ast.New_neighbour (new_id, ancestor, label), loc) ->
        check_act_id loc ancestor kai;
        if List.mem (new_id, None) kai
        then Error.build ~loc "Node identifier \"%s\" is already used" new_id;
	let edge = G_edge.make ~loc ~locals label in
	begin
	  try
            (
              (NEW_NEIGHBOUR
                 (new_id,
                  edge,
                  Pid.Pos (Id.build ~loc (fst ancestor) table)
                 ), loc),
              ((new_id, None)::kai, kei)
            )
	  with Not_found -> 
	    Log.fcritical "[GRS] tries to build a command New_neighbour (%s) on node %s which is not in the pattern %s"
	      (G_edge.to_string edge)
	      (fst ancestor)
	      (Loc.to_string loc)
	end

      | (Ast.Activate n, loc) -> failwith "Not implemented"
	  
      | (Ast.Del_node act_n, loc) ->
        check_act_id loc act_n kai;
        ((DEL_NODE (pid_of_act_id act_n), loc), (List_.rm act_n kai, kei))
	  
      | (Ast.Del_feat (act_id, feat_name), loc) ->
        check_act_id loc act_id kai;
        ((DEL_FEAT (pid_of_act_id act_id, feat_name), loc), (kai, kei))

      | (Ast.Update_feat ((act_id, feat_name), ast_items), loc) ->
        check_act_id loc act_id kai;
        let items = List.map 
          (function
            (* special case of a basic identifier understood as a string *)
            | Ast.Qfn_item ci when Ast.is_simple ci -> String (Ast.complex_id_to_string ci)
            | Ast.Qfn_item ci ->
              let (act_id,feature_name) = Ast.act_qfn_of_ci ci in
              check_act_id loc act_id kai; Feat (pid_of_act_id act_id, feature_name)
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
        ((UPDATE_FEAT (pid_of_act_id act_id, feat_name, items), loc), (kai, kei))
end (* module Command *)
