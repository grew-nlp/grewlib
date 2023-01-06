(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2022 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: https://grew.fr                                                    *)
(*    License: CeCILL (see LICENSE folder or "http://cecill.info/")               *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open Conll

open Grew_types
open Grew_utils
open Grew_ast
open Grew_graph
open Grew_rule


(* ================================================================================ *)
module Grs : sig
  type t

  val empty: t

  val load: config:Conll_config.t -> string  -> t
  val parse: config:Conll_config.t -> string  -> t


  val to_json: config:Conll_config.t -> t -> Yojson.Basic.t
  val dump: t -> unit

  val get_strat_list: t -> string list
  val get_strat_lists: t -> (string list * string list)
  val get_package_list: t -> string list
  val get_rule_list: t -> string list

  val simple_rewrite: config:Conll_config.t -> t -> string -> G_graph.t -> G_graph.t list

  val onf_rewrite_opt: config:Conll_config.t -> t -> string -> G_graph.t -> G_graph.t option

  (* [apply grs_name t] apply a deterministic GRS of the given [name]
     [Error.Run] is raised if the name in unknown or the GRS application not deterministic *)
  val apply: config:Conll_config.t -> string -> G_graph.t -> G_graph.t


  val of_json: config:Conll_config.t -> Yojson.Basic.t -> t
end (* module Grs *)
