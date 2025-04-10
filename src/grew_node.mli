(**********************************************************************************)
(*    grewlib • a Graph Rewriting library dedicated to NLP applications           *)
(*                                                                                *)
(*    Copyright 2011-2025 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: https://grew.fr                                                    *)
(*    License: CeCILL (see LICENSE folder or "http://cecill.info/")               *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)


open Grew_types
open Grew_utils
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

  val get_next: t -> G_edge.t Gid_massoc.t
  val get_next_basic: t -> G_edge.t Gid_massoc.t
  val set_next: G_edge.t Gid_massoc.t -> t -> t

  val get_pred_opt: t -> Gid.t option

  val get_succ_opt: t -> Gid.t option

  val get_position_opt: t -> int option
  val set_position: int -> t -> t
  val unset_position: t -> t

  val is_conll_zero: t -> bool
  val is_eud_empty: t -> bool

  val out_edges: t -> int


  val build_pst_leaf: ?loc:Loc.t -> string -> t
  val build_pst_node: ?loc:Loc.t -> string -> t


  val remove_edge_opt: Gid.t -> G_edge.t -> t -> t option

  val del_edge_feature_opt: Gid.t -> G_edge.t -> string -> t -> (t * G_edge.t * bool) option

  val remove_key: Gid.t -> t -> t

  val add_edge: G_edge.t -> Gid.t -> t -> t

  (* None is returned if the edge already exists *)
  val add_edge_opt: G_edge.t -> Gid.t -> t -> t option

  val rename: (Gid.t * Gid.t) list -> t -> t

  val concat_feats_opt: ?loc:Loc.t -> Ast.side -> t -> t -> string -> Regexp.t -> (t * (string * Feature_value.t) list) option
  val shift: string -> int -> t -> t
  val unshift: string -> t -> t

  val append_in_ag_lex: (string list) -> t -> int Clustered.t -> int Clustered.t

end (* module G_node *)

(* ================================================================================ *)
module P_node: sig
  type t

  val empty: t

  val is_empty: t -> bool

  val get_name: t -> Id.name

  val get_fs_disj: t -> P_fs.t list

  val get_next: t -> P_edge.t Pid_massoc.t

  val of_ast: Lexicons.t -> Ast.node -> (Id.name * t)

  (** [unif_fs_disj fs_disj t] replaces the feature structure disjunction of the p_node
      by the unification of [t.fs_disj] and [fs_disj].
      It raises [P_fs.Fail_unif] exception in case of Failure (empty disjunction).
  *)
  val unif_fs_disj: P_fs.t list -> t -> t

  val add_edge_opt: P_edge.t -> Pid.t -> t -> t option

  (* The bool returned is [true] iff the lexicons was changed during the matching *)
  val match_: ?lexicons:Lexicons.t -> t -> G_node.t -> ((bool * Lexicons.t) * int)

  val compare_loc: t -> t -> int
end (* module P_node *)

