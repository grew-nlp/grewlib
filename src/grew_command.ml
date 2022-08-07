(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2022 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: https://grew.fr                                                    *)
(*    License: CeCILL (see LICENSE folder or "http://cecill.info/")               *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open Printf

open Grew_types
open Grew_utils
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
    | Node_feat of (command_node * string)
    | Edge_feat of (string * string)
    | String_item of string
    | Lexical_field of Ast.pointed

  let json_of_item = function
    | Node_feat (cn, feature_name) -> 
      `Assoc 
        [("copy_feat",
          `Assoc [
            ("node",command_node_to_json cn);
            ("feature_name", `String feature_name);
          ]
         )]
    | Edge_feat (edge_id, feat_name) -> 
      `Assoc [("edge_id", `String edge_id); ("feat_name", `String feat_name)]
    | String_item s -> `Assoc [("string", `String s)]
    | Lexical_field (lex,field) -> `Assoc [("lexical_field", `String (lex ^ "." ^ field))]

  type ranged_item = item * Range.t

  let json_of_ranged_item (item, range) = 
    `Assoc [("item", json_of_item item); ("range", Range.to_json range)]

  (* the command in pattern *)
  type p =
    | DEL_NODE of command_node
    | DEL_EDGE_EXPL of (command_node * command_node * G_edge.t)
    | DEL_EDGE_NAME of string (* edge identifier *)
    | ADD_EDGE of (command_node * command_node * G_edge.t)
    | ADD_EDGE_EXPL of (command_node * command_node * string)
    | ADD_EDGE_ITEMS of (command_node * command_node * (string * string) list)
    | DEL_FEAT of (command_node * string)
    | DEL_EDGE_FEAT of (string * string) (* (edge identifier, feature_name) *)
    | UPDATE_FEAT of (command_node * string * ranged_item list)
    | UPDATE_EDGE_FEAT of (string * string * ranged_item list) (* edge identifier, feat_name, new_value *)
    (* *)
    | NEW_NODE of string
    | NEW_BEFORE of (string * command_node)
    | NEW_AFTER of (string * command_node)
    (* *)
    | SHIFT_EDGE of (command_node * command_node * Label_cst.t)
    | SHIFT_IN of (command_node * command_node * Label_cst.t)
    | SHIFT_OUT of (command_node * command_node * Label_cst.t)
    | CONCAT_FEATS of (Ast.side * command_node * command_node * string * string)
    | UNORDER of command_node
    | INSERT_BEFORE of (command_node * command_node)
    | INSERT_AFTER of (command_node * command_node)

  type t = p * Loc.t  (* remember command location to be able to localize a command failure *)

  let to_json_python ~config (p, _) = match p with
    | DEL_NODE cn -> `Assoc [("del_node", command_node_to_json cn)]
    | DEL_EDGE_EXPL (src,tar,edge) ->
      `Assoc [("del_edge_expl",
               `Assoc [
                 ("src",command_node_to_json src);
                 ("tar",command_node_to_json tar);
                 ("edge", G_edge.to_json edge);
               ]
              )]
    | DEL_EDGE_NAME edge_name -> `Assoc [("del_edge_name", `String edge_name)]
    | ADD_EDGE (src,tar,edge) ->
      `Assoc [("add_edge",
               `Assoc [
                 ("src",command_node_to_json src);
                 ("tar",command_node_to_json tar);
                 ("edge", G_edge.to_json edge);
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

    | ADD_EDGE_ITEMS (src, tar, items) ->
      `Assoc [("add_edge_items",
               `Assoc [
                 ("src",command_node_to_json src);
                 ("tar",command_node_to_json tar);
                 ("items", `List (List.map (fun (efn,efv) -> `Assoc [("edge_feature_name",`String efn); ("edge_feature_value", `String efv)]) items));
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
                 ("items", `List (List.map json_of_ranged_item items));
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
                 ("label_cst", Label_cst.to_json_python ~config label_cst);
               ]
              )]
    | SHIFT_IN (src,tar,label_cst) ->
      `Assoc [("shift_in",
               `Assoc [
                 ("src",command_node_to_json src);
                 ("tar",command_node_to_json tar);
                 ("label_cst", Label_cst.to_json_python ~config label_cst);
               ]
              )]
    | SHIFT_OUT (src,tar,label_cst) ->
      `Assoc [("shift_out",
               `Assoc [
                 ("src",command_node_to_json src);
                 ("tar",command_node_to_json tar);
                 ("label_cst", Label_cst.to_json_python ~config label_cst);
               ]
              )]
    | UPDATE_EDGE_FEAT (edge_id, feat_name, items) ->
      `Assoc [("update_edge_feat",
               `Assoc [
                 ("edge_id", `String edge_id);
                 ("feat_name", `String feat_name);
                 ("items", `List (List.map json_of_ranged_item items));
               ]
              )]
    | DEL_EDGE_FEAT (edge_id, feat_name) ->
      `Assoc [("del_edge_feat",
               `Assoc [
                 ("edge_id", `String edge_id);
                 ("feat_name", `String feat_name)
               ]
              )]
    | CONCAT_FEATS (side, src, tar, regexp, separator) ->
      `Assoc [((match side with Append -> "appen_feats" | Prepend -> "prepend"),
               `Assoc [
                 ("src", command_node_to_json src);
                 ("tar", command_node_to_json tar);
                 ("regexp", `String regexp);
                 ("separator", `String separator)
               ]
        )]
    | UNORDER cn -> `Assoc [("unorder", command_node_to_json cn)]
    | INSERT_BEFORE (cn1,cn2) -> `Assoc [("insert_before", `Assoc [("inserted", command_node_to_json cn1); ("site", command_node_to_json cn2)])]
    | INSERT_AFTER (cn1,cn2) -> `Assoc [("insert_after", `Assoc [("inserted", command_node_to_json cn1); ("site", command_node_to_json cn2)])]



  let of_ast ~config lexicons (kni, kei) table ast_command =
    (* kni stands for "known node idents", kei for "known edge idents" *)

    let cn_of_node_id node_id =
      match Id.build_opt node_id table with
      | Some x -> Pat (Pid.Ker x)
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
      let edge = G_edge.from_string ~config lab in
      ((DEL_EDGE_EXPL (cn_of_node_id node_i, cn_of_node_id node_j, edge), loc), (kni, kei))

    | (Ast.Del_edge_name id, loc) ->
      check_edge loc id kei;
      (DEL_EDGE_NAME id, loc), (kni, List_.remove id kei)

    | (Ast.Add_edge (node_i, node_j, lab), loc) ->
      check_node_id loc node_i kni;
      check_node_id loc node_j kni;
      let edge = G_edge.from_string ~config lab in
      ((ADD_EDGE (cn_of_node_id node_i, cn_of_node_id node_j, edge), loc), (kni, kei))

    | (Ast.Add_edge_expl (node_i, node_j, name), loc) ->
      check_node_id loc node_i kni;
      check_node_id loc node_j kni;
      ((ADD_EDGE_EXPL (cn_of_node_id node_i, cn_of_node_id node_j, name), loc), (kni, name::kei))

    | (Ast.Add_edge_items (node_i, node_j, items), loc) ->
      check_node_id loc node_i kni;
      check_node_id loc node_j kni;
      ((ADD_EDGE_ITEMS (cn_of_node_id node_i, cn_of_node_id node_j, items), loc), (kni, kei))

    | (Ast.Shift_edge (node_i, node_j, label_cst), loc) ->
      check_node_id loc node_i kni;
      check_node_id loc node_j kni;
      ((SHIFT_EDGE (cn_of_node_id node_i, cn_of_node_id node_j, Label_cst.of_ast ~loc ~config label_cst), loc), (kni, kei))

    | (Ast.Shift_in (node_i, node_j, label_cst), loc) ->
      check_node_id loc node_i kni;
      check_node_id loc node_j kni;
      ((SHIFT_IN (cn_of_node_id node_i, cn_of_node_id node_j, Label_cst.of_ast ~loc ~config label_cst), loc), (kni, kei))

    | (Ast.Shift_out (node_i, node_j, label_cst), loc) ->
      check_node_id loc node_i kni;
      check_node_id loc node_j kni;
      ((SHIFT_OUT (cn_of_node_id node_i, cn_of_node_id node_j, Label_cst.of_ast ~loc ~config label_cst), loc), (kni, kei))

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
      ((DEL_NODE (cn_of_node_id node_n), loc), (List_.remove node_n kni, kei))

    | (Ast.Del_feat (node_or_edge_id, feat_name), loc) ->
      begin
        match (List.mem node_or_edge_id kni, List.mem node_or_edge_id kei) with
        | (true, false) ->
          ((DEL_FEAT (cn_of_node_id node_or_edge_id, feat_name), loc), (kni, kei))
        | (false, true) ->
          ((DEL_EDGE_FEAT (node_or_edge_id, feat_name), loc), (kni, kei))
        | _ -> Error.build ~loc "Unknwon identifier \"%s\"" node_or_edge_id
      end

    | (Ast.Concat_feats ((side, src_id, tar_id, regexp, separator)), loc) ->
      check_node_id loc src_id kni;
      check_node_id loc tar_id kni;
      ((CONCAT_FEATS (side, cn_of_node_id src_id, cn_of_node_id tar_id, regexp, separator), loc), (kni, kei))

    | (Ast.Unorder node_n, loc) ->
      check_node_id loc node_n kni;
      ((UNORDER (cn_of_node_id node_n), loc), (kni, kei))

    | (Ast.Insert_before (id1, id2), loc) ->
      check_node_id loc id1 kni;
      check_node_id loc id2 kni;
      ((INSERT_BEFORE (cn_of_node_id id1,cn_of_node_id id2), loc), (kni, kei))
    | (Ast.Insert_after (id1, id2), loc) ->
      check_node_id loc id1 kni;
      check_node_id loc id2 kni;
      ((INSERT_AFTER (cn_of_node_id id1,cn_of_node_id id2), loc), (kni, kei))

    | (Ast.Update_feat ((node_or_edge_id, feat_name), ast_items), loc) ->
      let of_ast_item = function
        | Ast.Qfn_or_lex_item ((id_or_lex,feature_name_or_lex_field), range) ->
          if List.mem_assoc id_or_lex lexicons
          then
            begin
              Lexicons.check ~loc id_or_lex feature_name_or_lex_field lexicons;
              (Lexical_field (id_or_lex, feature_name_or_lex_field), range)
            end
          else if List.mem id_or_lex kni
          then
            begin
              (Node_feat (cn_of_node_id id_or_lex, feature_name_or_lex_field), range)
            end
          else if List.mem id_or_lex kei
          then (Edge_feat (id_or_lex, feature_name_or_lex_field), range)
          else Error.build ~loc "Unknown identifier \"%s\"" id_or_lex
        | Ast.String_item (s, range) -> (String_item s, range) in

      begin
        match (List.mem node_or_edge_id kni, List.mem node_or_edge_id kei) with

        (* [node_or_edge_id] is a node id *)
        | (true, false) when feat_name = "__id__" -> Error.build ~loc "The node feature name \"__id__\" is reserved and cannot be used in commands"
        | (true, false) ->
          let items = List.map of_ast_item ast_items in
          ((UPDATE_FEAT (cn_of_node_id node_or_edge_id, feat_name, items), loc), (kni, kei))

        (* [node_or_edge_id] is a edge id *)
        | (false, true) when List.mem feat_name ["length"; "delta"] ->
          Error.build ~loc "The edge feature name \"%s\" is reserved and cannot be used in commands" feat_name
        | (false, true) ->
          let items = List.map of_ast_item ast_items in
          ((UPDATE_EDGE_FEAT (node_or_edge_id, feat_name, items), loc), (kni, kei))

        (* other cases *)
        | (true,true) -> Error.build ~loc "Identifier conflict: \"%s\" is used both for a node and an edeg" node_or_edge_id
        | (false, false) -> Error.build ~loc "[#2] Unknwon identifier \"%s\"" node_or_edge_id
      end

end (* module Command *)
