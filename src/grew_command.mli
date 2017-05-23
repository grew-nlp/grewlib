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
open Grew_domain
open Grew_edge

(* ================================================================================ *)
module Command : sig
  type command_node =       (* a command node is either: *)
    | Pat of Pid.t          (* a node identified in the pattern *)
    | New of string         (* a node introduced by a new_neighbour *)

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
    | ADD_EDGE_EXPL of (command_node * command_node * string)
    | DEL_FEAT of (command_node * string)
    | UPDATE_FEAT of (command_node * string * item list)

    | NEW_NODE of string
    | NEW_BEFORE of (string * command_node)
    | NEW_AFTER of (string * command_node)

    | SHIFT_EDGE of (command_node * command_node * Label_cst.t)
    | SHIFT_IN of (command_node * command_node * Label_cst.t)
    | SHIFT_OUT of (command_node * command_node * Label_cst.t)

  type t = (p * Loc.t)
  val to_json: ?domain:Domain.t -> t -> Yojson.Basic.json

  type h =
    | H_DEL_NODE of Gid.t
    | H_DEL_EDGE_EXPL of (Gid.t * Gid.t *G_edge.t)
    | H_DEL_EDGE_NAME of string
    | H_ADD_EDGE of (Gid.t * Gid.t * G_edge.t)
    | H_ADD_EDGE_EXPL of (Gid.t * Gid.t * string)
    | H_DEL_FEAT of (Gid.t *string)
    | H_UPDATE_FEAT of (Gid.t * string * string)

    | H_NEW_NODE of string
    | H_NEW_BEFORE of (string * Gid.t)
    | H_NEW_AFTER of (string * Gid.t)

    | H_SHIFT_EDGE of (Gid.t * Gid.t)
    | H_SHIFT_IN of (Gid.t * Gid.t)
    | H_SHIFT_OUT of (Gid.t * Gid.t)

  val build:
      ?domain: Domain.t ->
      ?param: (string list * string list) ->
      (Id.name list * string list) ->
      Id.table ->
      Label_domain.decl array ->
      Ast.command ->
        t * (Id.name list * string list)
end (* module Command *)
