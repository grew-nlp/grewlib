(**********************************************************************************)
(*    grewlib • a Graph Rewriting library dedicated to NLP applications           *)
(*                                                                                *)
(*    Copyright 2011-2025 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: https://grew.fr                                                    *)
(*    License: CeCILL (see LICENSE folder or "http://cecill.info/")               *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open Conll

open Grew_utils
open Grew_ast

(* ================================================================================ *)
(** The module [G_edge] defines the type of Graph label edges: atomic edges *)
module G_edge: sig
  type t

  val empty: t
  val sub: t
  val pred: t
  val succ: t

  (** [is_real_link t] returns [true] iff the edge is a regular link (i.e. not a special link encoding linear order). *)
  val is_real_link: t -> bool

  (** [is_basic t] returns [true] iff the edge is a regular fs without the [enhanced=Yes] feature.
      Used to get the subset of edges to take into account in graph structure checking. *)
  val is_basic: t -> bool

  val from_items: (string * Feature_value.t) list -> t

  (**  WARNING: [s] is supposed to be a compact representation. *)
  val from_string: config:Conll_config.t -> string -> t

  (** [get_sub_opt f t] returns Some v if (f,v) is defined in t.
      [Error.run] is raise if [t] is a special edge *)
  val get_sub_opt: string -> t -> Feature_value.t option

  (** [update f v t] updates of adds [(f,v)] in [t].
      [Error.run] is raise if [t] is a special edge *)
  val update: string -> Feature_value.t -> t -> t

  (** [remove_feat_opt f t] returns [Some t'] if [f] is defined in t, [None] else.
      [Error.run] is raise if [t] is a special edge *)
  val remove_feat_opt: string -> t -> t option

  (** [to_string_opt t] returns [Some s] where [s] is the compact string if possible.
      [None] is returned if [t] is a special edge *)
  val to_string_opt: config:Conll_config.t -> t -> string option

  (* robust printing: to be used only in error reporting *)
  val to_string: ?config:Conll_config.t -> t -> string

  (** [to_json_opt t] returns [Some js] where [js] is the json data usable in Conll
      [None] is returned if [t] is a special edge *)
  val to_json_opt: t -> Yojson.Basic.t option

  (** Temp hardcoded conversion from t to dep2pict string *)
  val to_dep_opt: ?deco:bool -> config:Conll_config.t -> t -> string option

  (** Temp hardcoded conversion from t to graphviz string *)
  val to_dot_opt: ?deco:bool -> config:Conll_config.t -> t -> string option

  val to_json: t -> Yojson.Basic.t

  val build: config:Conll_config.t -> Ast.edge -> t

end (* module G_edge *)


(* ================================================================================ *)
(** The module [Label_cst] defines contraints on label edges *)
module Label_cst : sig
  type t

  val to_string: config:Conll_config.t -> t -> string
  val all: t
  val match_: config:Conll_config.t -> t -> G_edge.t -> bool
  val of_ast: ?loc:Loc.t -> config:Conll_config.t -> Ast.edge_label_cst -> t
end (* module Label_cst *)


(* ================================================================================ *)
(** The module [P_edge] defines the type of request label edges: atomic edges *)
module P_edge: sig
  type t

  val pred: t
  val succ: t

  val get_id_opt: t -> string option

  val to_string: config:Conll_config.t -> t -> string

  val to_id_opt_and_string: config:Conll_config.t -> t -> (string option * string)
  val of_ast: config:Conll_config.t -> Ast.edge -> t

  type edge_matcher =
    | Fail
    | Pass
    | Binds of string * G_edge.t list

  val match_: config:Conll_config.t -> t -> G_edge.t -> edge_matcher

  val match_list: config:Conll_config.t -> t -> G_edge.t list -> edge_matcher
end (* module P_edge *)
