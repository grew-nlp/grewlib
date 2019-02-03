(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2018 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: http://grew.fr                                                     *)
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
  type command_node =   (* a command node is either: *)
    | Pat of Pid.t      (* a node identified in the pattern *)
    | New of string     (* a node introduced by a add_node *)

  let command_node_to_json = function
    | Pat pid -> `String (Pid.to_string pid)
    | New s -> `String s

  (* [item] is a element of the RHS of an update_feat command *)
  type item =
    | Feat of (command_node * string)
    | String of string
    | Lexical_field of (string * string)

  let item_to_json = function
  | Feat (cn, feature_name) -> `Assoc [("copy_feat",
        `Assoc [
          ("node",command_node_to_json cn);
          ("feature_name", `String feature_name);
        ]
      )]
  | String s -> `Assoc [("string", `String s)]
  | Lexical_field (lex,field) -> `Assoc [("lexical_filed", `String (lex ^ "." ^ field))]

  (* the command in pattern *)
  type p =
    | DEL_NODE of command_node
    | DEL_EDGE_EXPL of (command_node * command_node * G_edge.t)
    | DEL_EDGE_NAME of string
    | ADD_EDGE of (command_node * command_node * G_edge.t)
    | ADD_EDGE_EXPL of (command_node * command_node * string)
    | DEL_FEAT of (command_node * string)
    | UPDATE_FEAT of (command_node * string * item list)
    (* *)
    | NEW_NODE of string
    | NEW_BEFORE of (string * command_node)
    | NEW_AFTER of (string * command_node)
    (* *)
    | SHIFT_EDGE of (command_node * command_node * Label_cst.t)
    | SHIFT_IN of (command_node * command_node * Label_cst.t)
    | SHIFT_OUT of (command_node * command_node * Label_cst.t)

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

    | ADD_EDGE_EXPL (src,tar,name) ->
      `Assoc [("add_edge",
        `Assoc [
          ("src",command_node_to_json src);
          ("tar",command_node_to_json tar);
          ("name", `String name);
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

  let build ?domain lexicons (kni, kei) table ast_command =
    (* kni stands for "known node idents", kei for "known edge idents" *)

    let cn_of_node_id node_id =
      match Id.build_opt node_id table with
      | Some x -> Pat (Pid.Pos x)
      | None   -> New node_id in

    let check_node_id_msg loc msg node_id kni =
      if not (List.mem node_id kni)
      then Error.build ~loc "%s \"%s\"" msg node_id in

    let check_node_id loc node_id kni = check_node_id_msg loc "Unbound node identifier" node_id kni in

    (* check that the edge_id is defined in the pattern *)
    let check_edge loc edge_id kei =
      if not (List.mem edge_id kei)
      then Error.build ~loc "Unbound edge identifier \"%s\"" edge_id in

    match ast_command with
      | (Ast.Del_edge_expl (node_i, node_j, lab), loc) ->
          check_node_id loc node_i kni;
          check_node_id loc node_j kni;
          let edge = G_edge.from_string ~loc ?domain lab in
          ((DEL_EDGE_EXPL (cn_of_node_id node_i, cn_of_node_id node_j, edge), loc), (kni, kei))

      | (Ast.Del_edge_name id, loc) ->
          check_edge loc id kei;
          (DEL_EDGE_NAME id, loc), (kni, List_.rm id kei)

      | (Ast.Add_edge (node_i, node_j, lab), loc) ->
          check_node_id loc node_i kni;
          check_node_id loc node_j kni;
          let edge = G_edge.from_string ~loc ?domain lab in
          ((ADD_EDGE (cn_of_node_id node_i, cn_of_node_id node_j, edge), loc), (kni, kei))

      | (Ast.Add_edge_expl (node_i, node_j, name), loc) ->
          check_node_id loc node_i kni;
          check_node_id loc node_j kni;
          ((ADD_EDGE_EXPL (cn_of_node_id node_i, cn_of_node_id node_j, name), loc), (kni, kei))

      | (Ast.Shift_edge (node_i, node_j, label_cst), loc) ->
          check_node_id loc node_i kni;
          check_node_id loc node_j kni;
          ((SHIFT_EDGE (cn_of_node_id node_i, cn_of_node_id node_j, Label_cst.build ~loc ?domain label_cst), loc), (kni, kei))

      | (Ast.Shift_in (node_i, node_j, label_cst), loc) ->
          check_node_id loc node_i kni;
          check_node_id loc node_j kni;
          ((SHIFT_IN (cn_of_node_id node_i, cn_of_node_id node_j, Label_cst.build ?domain ~loc label_cst), loc), (kni, kei))

      | (Ast.Shift_out (node_i, node_j, label_cst), loc) ->
          check_node_id loc node_i kni;
          check_node_id loc node_j kni;
          ((SHIFT_OUT (cn_of_node_id node_i, cn_of_node_id node_j, Label_cst.build ?domain ~loc label_cst), loc), (kni, kei))

      | (Ast.New_node new_id, loc) ->
          if List.mem new_id kni
          then Error.build ~loc "Node identifier \"%s\" is already used" new_id;
          (((NEW_NODE new_id), loc),(new_id::kni, kei))

      | (Ast.New_before (new_id, old_id), loc) ->
          check_node_id loc old_id kni;
          if List.mem new_id kni
          then Error.build ~loc "Node identifier \"%s\" is already used" new_id;
          ((NEW_BEFORE (new_id,cn_of_node_id old_id), loc),(new_id::kni, kei))

      | (Ast.New_after (new_id, old_id), loc) ->
          check_node_id loc old_id kni;
          if List.mem new_id kni
          then Error.build ~loc "Node identifier \"%s\" is already used" new_id;
          ((NEW_AFTER (new_id,cn_of_node_id old_id), loc),(new_id::kni, kei))

      | (Ast.Del_node node_n, loc) ->
          check_node_id loc node_n kni;
          ((DEL_NODE (cn_of_node_id node_n), loc), (List_.rm node_n kni, kei))

      | (Ast.Del_feat (node_id, feat_name), loc) ->
          if feat_name = "position"
          then Error.build ~loc "Illegal del_feat command: the 'position' feature cannot be deleted";
          check_node_id loc node_id kni;
          Domain.check_feature_name ~loc ?domain feat_name;
          ((DEL_FEAT (cn_of_node_id node_id, feat_name), loc), (kni, kei))

      | (Ast.Update_feat ((node_id, feat_name), ast_items), loc) ->
          check_node_id loc node_id kni;
          let items = List.map
            (function
              | Ast.Qfn_or_lex_item (node_id_or_lex,feature_name_or_lex_field) ->
                if List.mem_assoc node_id_or_lex lexicons
                then
                  begin
                    Lexicons.check ~loc node_id_or_lex feature_name_or_lex_field lexicons;
                    Lexical_field (node_id_or_lex, feature_name_or_lex_field)
                  end
                else
                  begin
                    check_node_id_msg loc ("Unbound identifier (neither a node nor a lexicon):") node_id_or_lex kni;
                    Domain.check_feature_name ~loc ?domain feature_name_or_lex_field;
                    Feat (cn_of_node_id node_id_or_lex, feature_name_or_lex_field)
                  end
              | Ast.String_item s -> String s
            ) ast_items in
            (* check for consistency *)
            (match items with
              | _ when Domain.is_open_feature ?domain feat_name -> ()
              | [String s] -> Domain.check_feature ~loc ?domain feat_name s
              | _ -> ());
          ((UPDATE_FEAT (cn_of_node_id node_id, feat_name, items), loc), (kni, kei))

end (* module Command *)
