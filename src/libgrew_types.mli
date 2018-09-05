(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2018 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: http://grew.fr                                                     *)
(*    License: CeCILL (see LICENSE folder or "http://www.cecill.info")            *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

(** {2 Types fo grew} *)

open Grew_graph

type step_name = string

type rule_app = {
    rule_name: string;
    up: G_deco.t;
    down: G_deco.t;
}

type big_step = {
    first: rule_app;
    small_step: (G_graph.t * rule_app) list;
}

val swap : big_step -> big_step

(** the main type for display the result of a rewriting *)
type rew_display =
  | Empty (* pour les besoin du dev *)
  | Leaf of G_graph.t
  | Local_normal_form of G_graph.t * step_name * rew_display
  | Node of G_graph.t * step_name * (big_step * rew_display) list

val rew_display_size: rew_display -> int