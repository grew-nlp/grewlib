(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2021 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: https://grew.fr                                                     *)
(*    License: CeCILL (see LICENSE folder or "http://cecill.info/")            *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open Conll
open Conllx

open Grew_base
open Grew_types
open Grew_fs
open Grew_edge
open Grew_ast

(* ================================================================================ *)
module G_node: sig
  type t

  val empty: t

  val compare: t -> t -> int

  val get_name: Gid.t -> t -> string
  val set_name: string -> t -> t

  val get_fs: t -> G_fs.t
  val set_fs: G_fs.t -> t -> t

  val get_next: t -> G_edge.t Massoc_gid.t
  val get_next_without_pred_succ_enhanced: t -> G_edge.t Massoc_gid.t
  val set_next: G_edge.t Massoc_gid.t -> t -> t

  val get_pred_opt: t -> Gid.t option

  val get_succ_opt: t -> Gid.t option

  val get_position_opt: t -> int option
  val set_position: int -> t -> t
  val unset_position: t -> t

  val is_eud_empty: t -> bool

  val dump: config:Conllx_config.t -> t -> string
  val to_gr: t -> string

  val of_ast: ?position:int -> Ast.node -> t
  val build_pst_leaf: ?loc:Loc.t -> string -> t
  val build_pst_node: ?loc:Loc.t -> string -> t


  val remove_edge_opt: Gid.t -> G_edge.t -> t -> t option
  val del_edge_feature_opt: Gid.t -> G_edge.t -> string -> t -> (t * G_edge.t) option

  val remove_key: Gid.t -> t -> t

  val add_edge: G_edge.t -> Gid.t -> t -> t

  (* None is returned if the edge already exists *)
  val add_edge_opt: G_edge.t -> Gid.t -> t -> t option

  val rename: (Gid.t * Gid.t) list -> t -> t

  val append_feats_opt: ?loc:Loc.t -> t -> t -> string -> string -> (t * (string * feature_value) list) option
  val shift: string -> int -> t -> t
  val unshift: string -> t -> t

end (* module G_node *)

(* ================================================================================ *)
module P_node: sig
  type t

  val empty: t

  val get_name: t -> Id.name

  val get_fs: t -> P_fs.t

  val get_next: t -> P_edge.t Massoc_pid.t

  val of_ast: Lexicons.t -> Ast.node -> (Id.name * t)

  val to_json_python: config:Conllx_config.t -> t -> Yojson.Basic.t

  (** [unif_fs fs t] replaces the feature structure of the node
      by the unification of [t.fs] and [fs].
      It raises [P_fs.Fail_unif] exception in case of Failure. *)
  val unif_fs: P_fs.t -> t -> t

  val add_edge_opt: P_edge.t -> Pid.t -> t -> t option

  val match_: ?lexicons:Lexicons.t -> t -> G_node.t -> Lexicons.t

  val compare_pos: t -> t -> int
end (* module P_node *)
