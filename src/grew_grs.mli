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
open Grew_graph
open Grew_rule


(* ================================================================================ *)
module Grs : sig
  type t

  val load: string  -> t

  val dump: t -> unit

  val to_json: t -> Yojson.Basic.json
  val domain: t -> Domain.t option
  val domain_build: Ast.domain -> Domain.t


  val get_strat_list: t -> string list
  val at_least_one: t -> string -> bool
  val at_most_one: t -> string -> bool

  val gwh_simple_rewrite: t -> string -> G_graph.t -> G_graph.t list
  val wrd_rewrite: t -> string -> G_graph.t -> Libgrew_types.rew_display
end
