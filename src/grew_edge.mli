(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2018 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: http://grew.fr                                                     *)
(*    License: CeCILL (see LICENSE folder or "http://www.cecill.info")            *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open Grew_base
open Grew_types
open Grew_ast
open Grew_domain

(* ================================================================================ *)
(** The module [G_edge] defines the type of Graph label edges: atomic edges *)
module G_edge: sig
  type t

  val empty: t

  val from_items: (string * feature_value) list -> t

  val get_sub_opt: string -> t -> feature_value option

  val update: string -> feature_value -> t -> t

  val remove_feat_opt: string -> t -> t option

  val to_short_opt: t -> string option

  val to_string_opt: t -> string option

  val to_conllx_opt: t -> Yojson.Basic.t option

  (* robust printing: to be used only in error reporting *)
  val dump: t -> string

  val to_dep_opt: ?domain: Domain.t -> ?deco:bool -> t -> string option

  val to_dot_opt: ?domain: Domain.t -> ?deco:bool -> t -> string option

  val from_string: string -> t

  val to_json: t -> Yojson.Basic.t

  val sub: t
  val pred: t
  val succ: t

  val build: Ast.edge -> t

  val ordering: t -> bool
  val enhanced: t -> bool
end (* module G_edge *)


(* ================================================================================ *)
(** The module [Label_cst] defines contraints on label edges *)
module Label_cst : sig
  type t

  val to_string: ?domain:Domain.t -> t -> string
  val to_json: ?domain:Domain.t -> t -> Yojson.Basic.t
  val all: t
  val match_: ?domain:Domain.t -> t -> G_edge.t -> bool
  val build: ?loc:Loc.t -> ?domain:Domain.t -> Ast.edge_label_cst -> t
end (* module Label_cst *)


(* ================================================================================ *)
(** The module [P_edge] defines the type of pattern label edges: atomic edges *)
module P_edge: sig
  type t

  val pred: t
  val succ: t

  val to_json: ?domain:Domain.t -> t -> Yojson.Basic.t

  val get_id_opt: t -> string option

  val to_string: ?domain:Domain.t -> t -> string

  val build: ?domain:Domain.t -> Ast.edge -> t

  type edge_matcher =
    | Fail
    | Pass
    | Binds of string * G_edge.t list

  val match_: ?domain:Domain.t -> t -> G_edge.t -> edge_matcher

  val match_list: ?domain:Domain.t -> t -> G_edge.t list -> edge_matcher
end (* module P_edge *)
