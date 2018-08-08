(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2018 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: http://grew.fr                                                     *)
(*    License: CeCILL (see LICENSE folder or "http://www.cecill.info")            *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open Grew_graph

type graph = G_graph.t
type deco = G_deco.t

type module_name = string

type rule_app = {
    rule_name: string;
    up: G_deco.t;
    down: G_deco.t;
  }

(* the type for big edges which correspond to a module *)
type big_step = {
    first: rule_app;
    small_step: (G_graph.t * rule_app) list;
  }

let swap bs = {bs with small_step = List.rev bs.small_step}

(* the main type for display the result of a rewriting *)
type rew_display =
  | Empty (* pour les besoin du dev *)
  | Leaf of G_graph.t
  | Local_normal_form of G_graph.t * module_name * rew_display
  | Node of G_graph.t * module_name * (big_step * rew_display) list

let rec rew_display_size = function
  | Empty -> 0
  | Leaf _ -> 1
  | Local_normal_form (_,_,rd) -> rew_display_size rd
  | Node (_,_,l) -> List.fold_left (fun acc (_,rd) -> acc+(rew_display_size rd)) 0 l
