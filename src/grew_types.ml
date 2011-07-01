open Graph

type graph = Graph.t
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
  | Leaf of Graph.t
  | Local_normal_form of Graph.t * module_name * rew_display 
  | Node of Graph.t * module_name * (big_step * rew_display) list

(* the type for big edges which correspond the a module *)
and big_step = {
    first: rule_app;
    small_step: (Graph.t * rule_app) list;
  }

let to_dot_graph ?(deco=Deco.empty) graph = Graph.to_dot graph ~deco
let to_dep_graph ?main_feat ?(deco=Deco.empty) graph = Graph.to_dep ?main_feat ~deco graph 
