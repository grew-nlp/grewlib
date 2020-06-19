(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2018 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: http://grew.fr                                                     *)
(*    License: CeCILL (see LICENSE folder or "http://www.cecill.info")            *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open Conllx

open Grew_base
open Grew_types
open Grew_ast
open Grew_domain


(* ================================================================================ *)
(** The module [G_edge] defines the type of Graph label edges: atomic edges *)
module G_edge: sig
  type t

  val empty: t
  val sub: t
  val pred: t
  val succ: t

  val ordering: t -> bool
  val enhanced: t -> bool

  val from_items: (string * feature_value) list -> t

  (**  WARNING: [s] is supposed to be a compact representation. *)
  val from_string: config:Conllx_config.t -> string -> t

  (** [get_sub_opt f t] returns Some v if (f,v) is defined in t.
      [Error.run] is raise if [t] is a special edge *)
  val get_sub_opt: string -> t -> feature_value option

  (** [update f v t] updates of adds [(f,v)] in [t].
      [Error.run] is raise if [t] is a special edge *)
  val update: string -> feature_value -> t -> t

  (** [remove_feat_opt f t] returns [Some t'] if [f] is defined in t, [None] else.
      [Error.run] is raise if [t] is a special edge *)
  val remove_feat_opt: string -> t -> t option

  (** [to_string_opt t] returns [Some s] where [s] is the compact string if possible.
      [None] is returned if [t] is a special edge *)
  val to_string_opt: config:Conllx_config.t -> t -> string option

  (* robust printing: to be used only in error reporting *)
  val dump: ?config:Conllx_config.t -> t -> string

  (** [to_conllx_opt t] returns [Some js] where [js] is the json data usable in Conllx
      [None] is returned if [t] is a special edge *)
  val to_conllx_opt: t -> Yojson.Basic.t option

  (** Temp hardcoded conversion from t to dep2pict string (domain is unused) *)
  val to_dep_opt: ?domain: Domain.t -> ?deco:bool -> config:Conllx_config.t -> t -> string option

  (** Temp hardcoded conversion from t to graphviz string (domain is unused) *)
  val to_dot_opt: ?domain: Domain.t -> ?deco:bool -> config:Conllx_config.t -> t -> string option

  (** TODO: check usage of this function *)
  val to_json: t -> Yojson.Basic.t

  val build: config:Conllx_config.t -> Ast.edge -> t

end (* module G_edge *)


(* ================================================================================ *)
(** The module [Label_cst] defines contraints on label edges *)
module Label_cst : sig
  type t

  val to_string: ?domain:Domain.t -> config:Conllx_config.t -> t -> string
  val to_json: ?domain:Domain.t -> config:Conllx_config.t -> t -> Yojson.Basic.t
  val all: t
  val match_: ?domain:Domain.t -> config:Conllx_config.t -> t -> G_edge.t -> bool
  val of_ast: ?loc:Loc.t -> ?domain:Domain.t -> config:Conllx_config.t -> Ast.edge_label_cst -> t
end (* module Label_cst *)


(* ================================================================================ *)
(** The module [P_edge] defines the type of pattern label edges: atomic edges *)
module P_edge: sig
  type t

  val pred: t
  val succ: t

  val to_json: ?domain:Domain.t -> config:Conllx_config.t -> t -> Yojson.Basic.t

  val get_id_opt: t -> string option

  val to_string: ?domain:Domain.t -> config:Conllx_config.t -> t -> string

  val of_ast: ?domain:Domain.t -> config:Conllx_config.t -> Ast.edge -> t

  type edge_matcher =
    | Fail
    | Pass
    | Binds of string * G_edge.t list

  val match_: ?domain:Domain.t -> config:Conllx_config.t -> t -> G_edge.t -> edge_matcher

  val match_list: ?domain:Domain.t -> config:Conllx_config.t -> t -> G_edge.t list -> edge_matcher
end (* module P_edge *)
