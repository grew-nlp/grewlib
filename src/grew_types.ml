open Grew_graph

type graph = G_graph.t
type deco = G_deco.t

type module_name = string

type rule_app = {
    rule_name: string;
    up: G_deco.t;
    down: G_deco.t;
  }

(* the main type for display the result of a rewriting *)
type rew_display =
  | Empty (* pour les besoin du dev *)
  | Leaf of G_graph.t
  | Local_normal_form of G_graph.t * module_name * rew_display
  | Node of G_graph.t * module_name * (big_step * rew_display) list

(* the type for big edges which correspond to a module *)
and big_step = {
    first: rule_app;
    small_step: (G_graph.t * rule_app) list;
  }
