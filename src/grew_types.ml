open Grew_graph

type graph = G_graph.t
type deco = Deco.t

type module_name = string

type rule_app = {
    rule_name: string;
    up: Deco.t;
    down: Deco.t;
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

let to_dot_graph ?main_feat ?(deco=Deco.empty) graph = G_graph.to_dot ?main_feat graph ~deco
let to_dep_graph ?main_feat ?(deco=Deco.empty) graph = G_graph.to_dep ?main_feat ~deco graph
let to_gr_graph graph = G_graph.to_gr graph 
