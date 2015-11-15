(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2013 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: http://grew.loria.fr                                               *)
(*    License: CeCILL (see LICENSE folder or "http://www.cecill.info")            *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open Grew_base
open Grew_types
open Grew_fs
open Grew_edge
open Grew_ast

(* ================================================================================ *)
module G_node: sig
  type t

  val empty: t

  val to_string: Label.domain -> t -> string
  val to_gr: t -> string

  val get_fs: t -> G_fs.t
  val get_next: t -> G_edge.t Massoc_gid.t

  val set_fs: G_fs.t -> t -> t
  val set_position: float -> t -> t
  val set_next: G_edge.t Massoc_gid.t -> t -> t

  val is_conll_root: t -> bool

  val remove: Gid.t -> G_edge.t -> t -> t

  val remove_key: Gid.t -> t -> t

  val merge_key: ?strict:bool -> Gid.t -> Gid.t -> t -> t option
  val shift_out: ?strict:bool -> t -> t -> t option

  val rm_out_edges: t -> t

  val add_edge: G_edge.t -> Gid.t -> t -> t option
  val build: Domain.t -> ?def_position: float -> Ast.node -> (Id.name * t)
  val of_conll: ?loc:Loc.t -> Domain.t -> Conll.line -> t

  val get_position: t -> float

  val position_comp: t -> t -> int

  (** [get_annot_info node] searches for a feature with name starting with "__".
      It returns the feature_name without the prefix "__"
      raise an [Build] exception if there is more than one such feature. *)
  val get_annot_info: t -> string option

  val build_neighbour: t -> t
  val build_new: t -> t

  val rename: (Gid.t * Gid.t) list -> t -> t
end
(* ================================================================================ *)

(* ================================================================================ *)
module P_node: sig
  type t

  val empty: t

  val get_name: t -> Id.name
  val get_fs: t -> P_fs.t
  val get_next: t -> P_edge.t Massoc_pid.t

  (** [unif_fs fs t] replaces the feature structure of the node
      by the unification of [node.fs] ] and [fs].
      It raises (Error.Build msg) exception in case of Failure.
  *)
  val unif_fs: P_fs.t -> t -> t

  val build: Domain.t -> ?pat_vars: string list -> Ast.node -> (Id.name * t)

  val add_edge: P_edge.t -> Pid.t -> t -> t option

  val match_: ?param: Lex_par.t -> t -> G_node.t -> Lex_par.t option

  val compare_pos: t -> t -> int
end
(* ================================================================================ *)
