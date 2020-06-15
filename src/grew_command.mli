(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2018 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: http://grew.fr                                                     *)
(*    License: CeCILL (see LICENSE folder or "http://www.cecill.info")            *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open Conllx
open Grew_ast
open Grew_base
open Grew_types
open Grew_domain
open Grew_edge

(* ================================================================================ *)
module Command : sig
  type command_node =       (* a command node is either: *)
    | Pat of Pid.t          (* a node identified in the pattern *)
    | New of string         (* a node introduced by a new_neighbour *)

  type item =
    | Node_feat of (command_node * string)
    | Edge_feat of (string * string)
    | String_item of string
    | Lexical_field of (string * string)

  type p =
    | DEL_NODE of command_node
    | DEL_EDGE_EXPL of (command_node * command_node *G_edge.t)
    | DEL_EDGE_NAME of string
    | ADD_EDGE of (command_node * command_node * G_edge.t)
    | ADD_EDGE_EXPL of (command_node * command_node * string)
    | ADD_EDGE_ITEMS of (command_node * command_node * (string * string) list)
    | DEL_FEAT of (command_node * string)
    | DEL_EDGE_FEAT of (string * string) (* (edge identifier, feature_name) *)
    | UPDATE_FEAT of (command_node * string * item list)
    | UPDATE_EDGE_FEAT of (string * string * item) (* edge identifier, feat_name, new_value *)
    (* *)
    | NEW_NODE of string
    | NEW_BEFORE of (string * command_node)
    | NEW_AFTER of (string * command_node)
    (* *)
    | SHIFT_EDGE of (command_node * command_node * Label_cst.t)
    | SHIFT_IN of (command_node * command_node * Label_cst.t)
    | SHIFT_OUT of (command_node * command_node * Label_cst.t)
    | APPEND_FEATS of (command_node * command_node * string * string)
    | UNORDER of command_node

  type t = (p * Loc.t)
  val to_json: ?domain:Domain.t -> config:Conllx_config.t -> t -> Yojson.Basic.t


  val build:
    ?domain: Domain.t ->
    config:Conllx_config.t ->
    Lexicons.t ->
    (Id.name list * string list) ->
    Id.table ->
    Ast.command ->
    t * (Id.name list * string list)
end (* module Command *)
