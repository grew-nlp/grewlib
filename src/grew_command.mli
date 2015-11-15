(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2013 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: http://grew.loria.fr                                               *)
(*    License: CeCILL (see LICENSE folder or "http://www.cecill.info")            *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open Grew_ast
open Grew_base
open Grew_types

open Grew_edge

(* ================================================================================ *)
module Command : sig
  type command_node =       (* a command node is either: *)
    | Pat of Pid.t          (* a node identified in the pattern *)
    | New of string         (* a node introduced by a new_neighbour *)
    | Act of Pid.t * string (* a node introduced by a activate *)

  type item =
    | Feat of (command_node * string)
    | String of string
    | Param_in of int
    | Param_out of int

  type p =
    | DEL_NODE of command_node
    | DEL_EDGE_EXPL of (command_node * command_node *G_edge.t)
    | DEL_EDGE_NAME of string
    | ADD_EDGE of (command_node * command_node * G_edge.t)
    | DEL_FEAT of (command_node * string)
    | UPDATE_FEAT of (command_node * string * item list)
    | NEW_NEIGHBOUR of (string * G_edge.t * Pid.t)
    | SHIFT_EDGE of (command_node * command_node * Label_cst.t)
    | SHIFT_IN of (command_node * command_node * Label_cst.t)
    | SHIFT_OUT of (command_node * command_node * Label_cst.t)
    | MERGE_NODE of (command_node * command_node)
    | ACT_NODE of command_node


  type t = (p * Loc.t)
  type h =
    | H_DEL_NODE of Gid.t
    | H_DEL_EDGE_EXPL of (Gid.t * Gid.t *G_edge.t)
    | H_DEL_EDGE_NAME of string
    | H_ADD_EDGE of (Gid.t * Gid.t * G_edge.t)
    | H_DEL_FEAT of (Gid.t *string)
    | H_UPDATE_FEAT of (Gid.t * string * string)
    | H_NEW_NEIGHBOUR of (string * G_edge.t * Gid.t)
    | H_SHIFT_EDGE of (Gid.t * Gid.t)
    | H_SHIFT_IN of (Gid.t * Gid.t)
    | H_SHIFT_OUT of (Gid.t * Gid.t)
    | H_MERGE_NODE of (Gid.t * Gid.t)
    | H_ACT_NODE of (Gid.t * string)

  val build:
      Feature_domain.t ->
      Label_domain.t ->
      ?param: (string list * string list) ->
      (Ast.command_node_ident list * string list) ->
      Id.table ->
      Label_domain.decl array ->
      suffix list ->
      Ast.command ->
        t * (Ast.command_node_ident list * string list)
end (* module Command *)
