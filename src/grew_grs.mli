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
open Grew_ast
open Grew_domain
open Grew_graph
open Grew_rule

(* ================================================================================ *)
module Rewrite_history: sig
  type t = {
      instance: Instance.t;
      module_name: string;
      good_nf: t list;
    }

  val get_graphs: t -> G_graph.t list

  val is_empty: t -> bool

  val num_sol: t -> int

  (** [save_gr base_name t] saves one gr_file for each normal form defined in [t].
      Output files are named according to [base_name] and the Gorn adress in the rewriting tree. *)
  val save_gr: string -> t -> unit
  val save_conll: string -> t -> unit

  (** [save_full_conll base_name t] saves one conll_file for each normal form defined in [t].
      Output files are named according to [base_name] and a secondary index after "__".
      The number of conll file produced is returned. *)
  val save_full_conll: string -> t -> int

  (** [save_det_gr base_name t] supposes that the current GRS is deterministic.
      It writes exactly one output file named [base_name].gr with the unique normal form. *)
  val save_det_gr: string -> t -> unit
  val save_det_conll: ?header:string -> string -> t -> unit

  val det_dep_string: t -> string option

  val conll_dep_string: ?keep_empty_rh:bool -> t -> string option
end (* module Rewrite_history *)


(* ================================================================================ *)
module Grs : sig
  type t

  val load: string  -> t

  val dump: t -> unit

  val to_json: t -> Yojson.Basic.json
  val domain: t -> Domain.t option
  val domain_build: Ast.domain -> Domain.t

  val simple_rewrite: t -> string -> G_graph.t -> G_graph.t list
  val det_rew_display: t -> string -> G_graph.t -> Libgrew_types.rew_display

  val get_strat_list: t -> string list
  val at_least_one: t -> string -> bool
  val at_most_one: t -> string -> bool

  val gwh_simple_rewrite: t -> string -> G_graph.t -> G_graph.t list
  val wrd_rewrite: t -> string -> G_graph.t -> Libgrew_types.rew_display
end
