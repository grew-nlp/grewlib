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

  val from_items: (string * string) list -> t

  val to_string: ?domain:Domain.t -> t -> string

  val to_conll: ?domain:Domain.t -> t -> string

  val get_sub: string -> t -> string option

  val to_dep: ?domain: Domain.t -> ?deco:bool -> t -> string

  val to_dot: ?domain: Domain.t -> ?deco:bool -> t -> string

  val from_string: ?loc:Loc.t -> ?domain: Domain.t -> string -> t

  val to_json: ?domain:Domain.t -> t -> Yojson.Basic.t

  val sub: t

  val build: ?domain:Domain.t -> Ast.edge -> t

  (** apply a list of updates of the shape (feat_name, new_feat_value). *)
  val update: (string * string) list -> t -> t
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

  (* [all] is the joker pattern edge *)
  val all: t

  val to_json: ?domain:Domain.t -> t -> Yojson.Basic.t

  val get_id: t -> string

  val to_string: ?domain:Domain.t -> t -> string

  val build: ?domain:Domain.t -> Ast.edge -> t

  type edge_matcher =
    | Fail
    | Binds of string * G_edge.t list

  val match_: ?domain:Domain.t -> t -> G_edge.t -> edge_matcher

  val match_list: ?domain:Domain.t -> t -> G_edge.t list -> edge_matcher
end (* module P_edge *)
