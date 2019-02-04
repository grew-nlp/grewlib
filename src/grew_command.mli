(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2018 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: http://grew.fr                                                     *)
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
    | Lexical_field of (string * string)

  type p =
    | DEL_NODE of command_node
    | DEL_EDGE_EXPL of (command_node * command_node *G_edge.t)
    | DEL_EDGE_NAME of string
    | ADD_EDGE of (command_node * command_node * G_edge.t)
    | ADD_EDGE_EXPL of (command_node * command_node * string)
    | ADD_EDGE_ITEMS of (command_node * command_node * (string * string) list)
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


  val build:
      ?domain: Domain.t ->
      Lexicons.t ->
      (Id.name list * string list) ->
      Id.table ->
      Ast.command ->
        t * (Id.name list * string list)
end (* module Command *)
