(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2018 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: http://grew.fr                                                     *)
(*    License: CeCILL (see LICENSE folder or "http://www.cecill.info")            *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open Conll

open Grew_base
open Grew_types
open Grew_domain
open Grew_fs
open Grew_edge
open Grew_ast

(* ================================================================================ *)
module G_node: sig
  type t

  val empty: t

  val to_string: ?domain:Domain.t -> t -> string
  val to_gr: t -> string

  val get_fs: t -> G_fs.t
  val get_next: t -> G_edge.t Massoc_gid.t

  val get_succ: t -> Gid.t option
  val get_prec: t -> Gid.t option

  val set_prec: Gid.t -> t -> t
  val set_succ: Gid.t -> t -> t

  val remove_prec: t -> t
  val remove_succ: t -> t

  val set_fs: G_fs.t -> t -> t
  val set_position: float -> t -> t
  val get_position: t -> float option
  (* val get_float: t -> float *)

  val shift: string -> int -> t -> t
  val unshift: string -> t -> t

  val is_empty: t -> bool

  val set_next: G_edge.t Massoc_gid.t -> t -> t

  val get_name: Gid.t -> t -> string

  val get_efs: t -> (string * string) list
  val string_efs: t -> string
  val is_conll_root: t -> bool

  val remove_edge: Gid.t -> G_edge.t -> t -> t option
  val update_edge: Gid.t -> G_edge.t -> string -> string -> t -> (t * G_edge.t) option
  val del_edge_feature: Gid.t -> G_edge.t -> string -> t -> (t * G_edge.t) option

  val remove_key: Gid.t -> t -> t


  val rm_out_edges: t -> t

  (* None is returned if the edge already exists *)
  val add_edge: G_edge.t -> Gid.t -> t -> t option


  val build: ?domain:Domain.t -> ?prec:Gid.t -> ?succ:Gid.t -> ?position:float -> Ast.node -> t
  val of_conll: ?loc:Loc.t -> ?prec:Gid.t -> ?succ:Gid.t -> ?domain:Domain.t -> Conll.line -> t
  val pst_leaf: ?loc:Loc.t -> ?domain:Domain.t -> string -> int -> t
  val pst_node: ?loc:Loc.t -> ?domain:Domain.t -> string -> int -> t

  val fresh: ?prec:Gid.t -> ?succ:Gid.t -> float -> t

  val position_comp: t -> t -> int

  (* val build_neighbour: t -> t
  val build_new: t -> t *)

  val rename: (Gid.t * Gid.t) list -> t -> t

end (* module G_node *)

(* ================================================================================ *)
module P_node: sig
  type t

  val empty: t

  val to_json: ?domain:Domain.t -> t -> Yojson.Basic.t

  val get_name: t -> Id.name
  val get_fs: t -> P_fs.t
  val get_next: t -> P_edge.t Massoc_pid.t

  (** [unif_fs fs t] replaces the feature structure of the node
      by the unification of [t.fs] and [fs].
      It raises [P_fs.Fail_unif] exception in case of Failure. *)
  val unif_fs: P_fs.t -> t -> t

  val build: ?domain:Domain.t -> Lexicons.t -> Ast.node -> (Id.name * t)

  val add_edge: P_edge.t -> Pid.t -> t -> t option

  val match_: ?lexicons:Lexicons.t -> t -> G_node.t -> Lexicons.t

  val compare_pos: t -> t -> int
end (* module P_node *)
