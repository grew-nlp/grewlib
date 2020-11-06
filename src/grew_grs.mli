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
open Grew_graph
open Grew_rule


(* ================================================================================ *)
module Grs : sig
  type t

  val empty: t

  val load: config:Conllx_config.t -> string  -> t
  val parse: config:Conllx_config.t -> string  -> t

  val dump: t -> unit

  val to_json_python: config:Conllx_config.t -> t -> Yojson.Basic.t
  val domain_build: Ast.domain -> Domain.t

  val get_strat_list: t -> string list
  val at_least_one: t -> string -> bool
  val at_most_one: t -> string -> bool

  val simple_rewrite: config:Conllx_config.t -> t -> string -> G_graph.t -> G_graph.t list

  val onf_rewrite_opt: config:Conllx_config.t -> t -> string -> G_graph.t -> G_graph.t option

  val wrd_rewrite: config:Conllx_config.t -> t -> string -> G_graph.t -> Libgrew_types.rew_display

  (* [apply grs_name t] apply a deterministic GRS of the given [name]
     [Error.Run] is raised if the name in unknown or the GRS application not deterministic *)
  val apply: config:Conllx_config.t -> string -> G_graph.t -> G_graph.t
end (* module Grs *)
