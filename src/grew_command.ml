(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2013 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: http://grew.loria.fr                                               *)
(*    License: CeCILL (see LICENSE folder or "http://www.cecill.info")            *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open Printf
open Log

open Grew_base
open Grew_types

open Grew_ast
open Grew_edge
open Grew_fs

(* ================================================================================ *)
module Command  = struct
  type command_node =        (* a command node is either: *)
    | Pat of Pid.t           (* a node identified in the pattern *)
    | New of string          (* a node introduced by a new_neighbour *) (* TODO: remove *)
    | Act of Pid.t * string  (* a node introduced by an activate *)

  (* [item] is a element of the RHS of an update_feat command *)
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
    | NEW_NEIGHBOUR of (string * G_edge.t * Pid.t) (* TODO: remove *)
    | SHIFT_EDGE of (command_node * command_node)
    | SHIFT_IN of (command_node * command_node)
    | SHIFT_OUT of (command_node * command_node)
    | MERGE_NODE of (command_node * command_node)
    | ACT_NODE of command_node


  type t = p * Loc.t  (* remember command location to be able to localize a command failure *)

  (* a item in the command history: command applied to a graph *)
  type h =
    | H_DEL_NODE of Gid.t
    | H_DEL_EDGE_EXPL of (Gid.t * Gid.t * G_edge.t)
    | H_DEL_EDGE_NAME of string
    | H_ADD_EDGE of (Gid.t * Gid.t * G_edge.t)
    | H_DEL_FEAT of (Gid.t * string)
    | H_UPDATE_FEAT of (Gid.t * string * string)
    | H_NEW_NEIGHBOUR of (string * G_edge.t * Gid.t) (* TODO: remove *)
    | H_SHIFT_EDGE of (Gid.t * Gid.t)
    | H_SHIFT_IN of (Gid.t * Gid.t)
    | H_SHIFT_OUT of (Gid.t * Gid.t)
    | H_MERGE_NODE of (Gid.t * Gid.t)
    | H_ACT_NODE of (Gid.t * string)

  let build ?param (kai, kei) table locals suffixes ast_command =
    (* kai stands for "known act ident", kei for "known edge ident" *)

    let pid_of_act_id loc = function
        | Ast.Sharp (node_name, n) -> Act (Pid.Pos (Id.build ~loc node_name table), n)
        | Ast.No_sharp (node_name) ->
          try  (* TODO: remove with activate *)
            Pat (Pid.Pos (Id.build ~loc node_name table))
          with _ -> New node_name in

    let pid_of_node_id loc node_id = Pat (Pid.Pos (Id.build ~loc node_id table)) in

    (* check that an act_id is well-defined earlier *)
    let check_act_id loc act_id kai =
      if not (List.mem act_id kai)
      then Error.build ~loc "Unbound node identifier \"%s\"" (Ast.dump_command_node_ident act_id) in

    let check_node_id loc node_id kai =
      if not (List.mem (Ast.No_sharp node_id) kai)
      then Error.build ~loc "Unbound node identifier \"%s\"" node_id in

    (* check that the edge_id is defined in the pattern *)
    let check_edge loc edge_id kei =
      if not (List.mem edge_id kei)
      then Error.build ~loc "Unbound edge identifier \"%s\"" edge_id in

    match ast_command with
      | (Ast.Del_edge_expl (act_i, act_j, lab), loc) ->
          check_act_id loc act_i kai;
          check_act_id loc act_j kai;
	        let edge = G_edge.make ~loc ~locals lab in
	        ((DEL_EDGE_EXPL (pid_of_act_id loc act_i, pid_of_act_id loc act_j, edge), loc), (kai, kei))

      | (Ast.Del_edge_name id, loc) ->
          check_edge loc id kei;
          (DEL_EDGE_NAME id, loc), (kai, List_.rm id kei)

      | (Ast.Add_edge (act_i, act_j, lab), loc) ->
          check_act_id loc act_i kai;
          check_act_id loc act_j kai;
	        let edge = G_edge.make ~loc ~locals lab in
        	((ADD_EDGE (pid_of_act_id loc act_i, pid_of_act_id loc act_j, edge), loc), (kai, kei))

      | (Ast.Shift_edge (act_i, act_j), loc) ->
          check_act_id loc act_i kai;
          check_act_id loc act_j kai;
	        ((SHIFT_EDGE (pid_of_act_id loc act_i, pid_of_act_id loc act_j), loc), (kai, kei))

      | (Ast.Shift_in (act_i, act_j), loc) ->
          check_act_id loc act_i kai;
          check_act_id loc act_j kai;
	        ((SHIFT_IN (pid_of_act_id loc act_i, pid_of_act_id loc act_j), loc), (kai, kei))

      | (Ast.Shift_out (act_i, act_j), loc) ->
          check_act_id loc act_i kai;
          check_act_id loc act_j kai;
	        ((SHIFT_OUT (pid_of_act_id loc act_i, pid_of_act_id loc act_j), loc), (kai, kei))

      | (Ast.Merge_node (act_i, act_j), loc) ->
          check_act_id loc act_i kai;
          check_act_id loc act_j kai;
	        ((MERGE_NODE (pid_of_act_id loc act_i, pid_of_act_id loc act_j), loc), (List_.rm act_i kai, kei))

      | (Ast.New_neighbour (new_id, ancestor, label), loc) ->
          check_act_id loc ancestor kai;
          if List.mem (Ast.No_sharp new_id) kai
          then Error.build ~loc "Node identifier \"%s\" is already used" new_id;

          let edge = G_edge.make ~loc ~locals label in
	        begin
	          try
            (
              (NEW_NEIGHBOUR
                 (new_id,
                  edge,
                  Pid.Pos (Id.build ~loc (Ast.base_command_node_ident ancestor) table)
                 ), loc),
              ((Ast.No_sharp new_id)::kai, kei)
            )
	          with not_found ->
	            Log.fcritical "[GRS] tries to build a command New_neighbour (%s) on node %s which is not in the pattern %s"
	             (G_edge.to_string edge)
	             (Ast.base_command_node_ident ancestor)
	             (Loc.to_string loc)
	        end

      | (Ast.Activate act_n, loc) ->
        begin
          match act_n with
          | Ast.No_sharp _ -> Error.build ~loc "Cannot activate a pattern node"
          | Ast.Sharp (src, suffix) ->
            check_act_id loc (Ast.No_sharp src) kai;
            if not (List.mem suffix suffixes) then Error.build ~loc "Undefined suffix \"%s\"" suffix;
            ((ACT_NODE (pid_of_act_id loc act_n), loc), (act_n :: kai, kei))
        end

      | (Ast.Del_node act_n, loc) ->
          check_act_id loc act_n kai;
          ((DEL_NODE (pid_of_act_id loc act_n), loc), (List_.rm act_n kai, kei))

      | (Ast.Del_feat (act_id, feat_name), loc) ->
          if feat_name = "position"
          then Error.build ~loc "Illegal del_feat command: the 'position' feature cannot be deleted";
          check_act_id loc act_id kai;
          Domain.check_feature_name ~loc feat_name;
          ((DEL_FEAT (pid_of_act_id loc act_id, feat_name), loc), (kai, kei))

      | (Ast.Update_feat ((act_id, feat_name), ast_items), loc) ->
          check_act_id loc act_id kai;
          let items = List.map
            (function
              | Ast.Qfn_item (node_id,feature_name) ->
                check_node_id loc node_id kai;
                Domain.check_feature_name ~loc feature_name;
                Feat (pid_of_node_id loc node_id, feature_name)
              | Ast.String_item s -> String s
              | Ast.Param_item var ->
                match param with
                  | None -> Error.build ~loc "Unknown command variable '%s'" var
                  | Some (par,cmd) ->
                    match (List_.pos var par, List_.pos var cmd) with
                      | (_,Some index) -> Param_out index
                      | (Some index,_) -> Param_in index
                      | _ -> Error.build ~loc "Unknown command variable '%s'" var
            ) ast_items in
            (* check for consistency *)
            (match items with
              | _ when Domain.is_open feat_name -> ()
              | [Param_out _] -> () (* TODO: check that lexical parameters are compatible with the feature domain *)
              | [String s] -> Domain.check_feature ~loc feat_name s
              | [Feat (_,fn)] -> ()
              | _ -> Error.build ~loc "[Update_feat] Only open features can be modified with the concat operator '+' but \"%s\" is not declared as an open feature" feat_name);
          ((UPDATE_FEAT (pid_of_act_id loc act_id, feat_name, items), loc), (kai, kei))
end (* module Command *)
