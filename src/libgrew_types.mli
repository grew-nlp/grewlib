(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2013 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: http://grew.loria.fr                                               *)
(*    License: CeCILL (see LICENSE folder or "http://www.cecill.info")            *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

(** {2 Types fo grew} *)

open Grew_graph

(**/**)
type graph = G_graph.t
type deco = G_deco.t
(**/**)

type module_name = string

(** Rule app *)
type rule_app = {
    rule_name: string;
    up: deco;
    down: deco;
}

(** the type for big edges which correspond the a module *)
type big_step = {
    first: rule_app;
    small_step: (graph * rule_app) list;
}

val swap : big_step -> big_step

(** the main type for display the result of a rewriting *)
type rew_display =
  | Empty (* pour les besoin du dev *)
  | Leaf of graph
  | Local_normal_form of graph * module_name * rew_display
  | Node of graph * module_name * (big_step * rew_display) list

val rew_display_size: rew_display -> int