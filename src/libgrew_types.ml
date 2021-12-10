(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2021 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: https://grew.fr                                                     *)
(*    License: CeCILL (see LICENSE folder or "http://cecill.info/")            *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

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

let swap bs = {bs with small_step = List.rev bs.small_step}
