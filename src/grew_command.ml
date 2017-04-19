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
open Grew_domain
open Grew_ast
open Grew_edge
open Grew_fs

(* ================================================================================ *)
module Command  = struct
  type command_node =        (* a command node is either: *)
    | Pat of Pid.t           (* a node identified in the pattern *)
    | New of string          (* a node introduced by a new_neighbour *) (* TODO: remove *)

  let command_node_to_json = function
    | Pat pid -> `String (Pid.to_string pid)
    | New s -> `String s

  (* [item] is a element of the RHS of an update_feat command *)
  type item =
    | Feat of (command_node * string)
    | String of string
    | Param_in of int
    | Param_out of int

  let item_to_json = function
  | Feat (cn, feature_name) -> `Assoc [("copy_feat",
        `Assoc [
          ("node",command_node_to_json cn);
          ("feature_name", `String feature_name);
        ]
      )]
  | String s -> `Assoc [("string", `String s)]
  | Param_in i -> `Assoc [("param_in", `Int i)]
  | Param_out i -> `Assoc [("param_out", `Int i)]

  (* the command in pattern *)
  type p =
    | DEL_NODE of command_node
    | DEL_EDGE_EXPL of (command_node * command_node * G_edge.t)
    | DEL_EDGE_NAME of string
    | ADD_EDGE of (command_node * command_node * G_edge.t)
    | DEL_FEAT of (command_node * string)
    | UPDATE_FEAT of (command_node * string * item list)

    | NEW_NODE of string
    | NEW_BEFORE of (string * command_node)
    | NEW_AFTER of (string * command_node)

    | SHIFT_EDGE of (command_node * command_node * Label_cst.t)
    | SHIFT_IN of (command_node * command_node * Label_cst.t)
    | SHIFT_OUT of (command_node * command_node * Label_cst.t)
    | MERGE_NODE of (command_node * command_node)

  type t = p * Loc.t  (* remember command location to be able to localize a command failure *)

  let to_json ?domain (p, _) = match p with
  | DEL_NODE cn -> `Assoc [("del_node", command_node_to_json cn)]
  | DEL_EDGE_EXPL (src,tar,edge) ->
    `Assoc [("del_edge_expl",
      `Assoc [
        ("src",command_node_to_json src);
        ("tar",command_node_to_json tar);
        ("edge", G_edge.to_json ?domain edge);
      ]
    )]
  | DEL_EDGE_NAME edge_name -> `Assoc [("del_edge_name", `String edge_name)]
  | ADD_EDGE (src,tar,edge) ->
    `Assoc [("add_edge",
      `Assoc [
        ("src",command_node_to_json src);
        ("tar",command_node_to_json tar);
        ("edge", G_edge.to_json ?domain edge);
      ]
    )]

  | DEL_FEAT (cn, feature_name) ->
    `Assoc [("del_feat",
      `Assoc [
        ("node",command_node_to_json cn);
        ("feature_name", `String feature_name);
      ]
    )]

  | UPDATE_FEAT (cn, feature_name, items) ->
    `Assoc [("update_feat",
      `Assoc [
        ("node",command_node_to_json cn);
        ("feature_name", `String feature_name);
        ("items", `List (List.map item_to_json items));
      ]
    )]

  | NEW_NODE name -> `Assoc [("new_node", `String name)]
  | NEW_BEFORE (name, cn) ->
    `Assoc [("new_before",
      `Assoc [
        ("name", `String name);
        ("node", command_node_to_json cn);
      ]
    )]
  | NEW_AFTER (name, cn) ->
    `Assoc [("new_after",
      `Assoc [
        ("name", `String name);
        ("node", command_node_to_json cn);
      ]
    )]

  | SHIFT_EDGE (src,tar,label_cst) ->
      `Assoc [("shift_edge",
        `Assoc [
          ("src",command_node_to_json src);
          ("tar",command_node_to_json tar);
          ("label_cst", Label_cst.to_json ?domain label_cst);
        ]
      )]
  | SHIFT_IN (src,tar,label_cst) ->
      `Assoc [("shift_in",
        `Assoc [
          ("src",command_node_to_json src);
          ("tar",command_node_to_json tar);
          ("label_cst", Label_cst.to_json ?domain label_cst);
        ]
      )]
  | SHIFT_OUT (src,tar,label_cst) ->
      `Assoc [("shift_out",
        `Assoc [
          ("src",command_node_to_json src);
          ("tar",command_node_to_json tar);
          ("label_cst", Label_cst.to_json ?domain label_cst);
        ]
      )]
  | MERGE_NODE (src,tar) ->
      `Assoc [("merge",
        `Assoc [
          ("src",command_node_to_json src);
          ("tar",command_node_to_json tar);
        ]
      )]

  (* a item in the command history: command applied to a graph *)
  type h =
    | H_DEL_NODE of Gid.t
    | H_DEL_EDGE_EXPL of (Gid.t * Gid.t * G_edge.t)
    | H_DEL_EDGE_NAME of string
    | H_ADD_EDGE of (Gid.t * Gid.t * G_edge.t)
    | H_DEL_FEAT of (Gid.t * string)
    | H_UPDATE_FEAT of (Gid.t * string * string)

    | H_NEW_NODE of string
    | H_NEW_BEFORE of (string * Gid.t)
    | H_NEW_AFTER of (string * Gid.t)

    | H_SHIFT_EDGE of (Gid.t * Gid.t)
    | H_SHIFT_IN of (Gid.t * Gid.t)
    | H_SHIFT_OUT of (Gid.t * Gid.t)
    | H_MERGE_NODE of (Gid.t * Gid.t)


  let build ?domain ?param (kai, kei) table locals ast_command =
    (* kai stands for "known act ident", kei for "known edge ident" *)

    let pid_of_act_id loc node_name =
          try  (* TODO: remove with activate *)
            Pat (Pid.Pos (Id.build ~loc node_name table))
          with _ -> New node_name in

    let pid_of_node_id loc node_id = Pat (Pid.Pos (Id.build ~loc node_id table)) in

    let check_node_id loc node_id kai =
      if not (List.mem node_id kai)
      then Error.build ~loc "Unbound node identifier \"%s\"" node_id in

    (* check that the edge_id is defined in the pattern *)
    let check_edge loc edge_id kei =
      if not (List.mem edge_id kei)
      then Error.build ~loc "Unbound edge identifier \"%s\"" edge_id in

    match ast_command with
      | (Ast.Del_edge_expl (act_i, act_j, lab), loc) ->
          check_node_id loc act_i kai;
          check_node_id loc act_j kai;
          let edge = G_edge.make ~loc ?domain lab in
          ((DEL_EDGE_EXPL (pid_of_act_id loc act_i, pid_of_act_id loc act_j, edge), loc), (kai, kei))

      | (Ast.Del_edge_name id, loc) ->
          check_edge loc id kei;
          (DEL_EDGE_NAME id, loc), (kai, List_.rm id kei)

      | (Ast.Add_edge (act_i, act_j, lab), loc) ->
          check_node_id loc act_i kai;
          check_node_id loc act_j kai;
          let edge = G_edge.make ~loc ?domain lab in
          ((ADD_EDGE (pid_of_act_id loc act_i, pid_of_act_id loc act_j, edge), loc), (kai, kei))

      | (Ast.Shift_edge (act_i, act_j, label_cst), loc) ->
          check_node_id loc act_i kai;
          check_node_id loc act_j kai;
          ((SHIFT_EDGE (pid_of_act_id loc act_i, pid_of_act_id loc act_j, Label_cst.build ~loc ?domain label_cst), loc), (kai, kei))

      | (Ast.Shift_in (act_i, act_j, label_cst), loc) ->
          check_node_id loc act_i kai;
          check_node_id loc act_j kai;
          ((SHIFT_IN (pid_of_act_id loc act_i, pid_of_act_id loc act_j, Label_cst.build ?domain ~loc label_cst), loc), (kai, kei))

      | (Ast.Shift_out (act_i, act_j, label_cst), loc) ->
          check_node_id loc act_i kai;
          check_node_id loc act_j kai;
          ((SHIFT_OUT (pid_of_act_id loc act_i, pid_of_act_id loc act_j, Label_cst.build ?domain ~loc label_cst), loc), (kai, kei))

      | (Ast.Merge_node (act_i, act_j), loc) ->
          check_node_id loc act_i kai;
          check_node_id loc act_j kai;
          ((MERGE_NODE (pid_of_act_id loc act_i, pid_of_act_id loc act_j), loc), (List_.rm act_i kai, kei))

      | (Ast.New_node new_id, loc) ->
          if List.mem new_id kai
          then Error.build ~loc "Node identifier \"%s\" is already used" new_id;
          (((NEW_NODE new_id), loc),(new_id::kai, kei))

      | (Ast.New_before (new_id, old_id), loc) ->
          check_node_id loc old_id kai;
          if List.mem new_id kai
          then Error.build ~loc "Node identifier \"%s\" is already used" new_id;
          ((NEW_BEFORE (new_id,pid_of_act_id loc old_id), loc),(new_id::kai, kei))

      | (Ast.New_after (new_id, old_id), loc) ->
          check_node_id loc old_id kai;
          if List.mem new_id kai
          then Error.build ~loc "Node identifier \"%s\" is already used" new_id;
          ((NEW_AFTER (new_id,pid_of_act_id loc old_id), loc),(new_id::kai, kei))

      | (Ast.Del_node act_n, loc) ->
          check_node_id loc act_n kai;
          ((DEL_NODE (pid_of_act_id loc act_n), loc), (List_.rm act_n kai, kei))

      | (Ast.Del_feat (act_id, feat_name), loc) ->
          if feat_name = "position"
          then Error.build ~loc "Illegal del_feat command: the 'position' feature cannot be deleted";
          check_node_id loc act_id kai;
          Domain.check_feature_name ~loc ?domain feat_name;
          ((DEL_FEAT (pid_of_act_id loc act_id, feat_name), loc), (kai, kei))

      | (Ast.Update_feat ((act_id, feat_name), ast_items), loc) ->
          check_node_id loc act_id kai;
          let items = List.map
            (function
              | Ast.Qfn_item (node_id,feature_name) ->
                check_node_id loc node_id kai;
                Domain.check_feature_name ~loc ?domain feature_name;
                Feat (pid_of_node_id loc node_id, feature_name)
              | Ast.String_item s -> String s
              | Ast.Param_item var ->
                match param with
                  | None -> Error.build ~loc "Unknown command variable '%s'" var
                  | Some (par,cmd) ->
                    match (List_.index var par, List_.index var cmd) with
                      | (_,Some index) -> Param_out index
                      | (Some index,_) -> Param_in index
                      | _ -> Error.build ~loc "Unknown command variable '%s'" var
            ) ast_items in
            (* check for consistency *)
            (match items with
              | _ when Domain.is_open_feature ?domain feat_name -> ()
              | [Param_out _] -> () (* TODO: check that lexical parameters are compatible with the feature domain *)
              | [String s] -> Domain.check_feature ~loc ?domain feat_name s
              | [Feat (_,fn)] -> ()
              | _ -> Error.build ~loc "[Update_feat] Only open features can be modified with the concat operator '+' but \"%s\" is not declared as an open feature" feat_name);
          ((UPDATE_FEAT (pid_of_act_id loc act_id, feat_name, items), loc), (kai, kei))

      | (Ast.New_neighbour _, _) -> Error.bug "New_neighbour command must be replaced"

end (* module Command *)
