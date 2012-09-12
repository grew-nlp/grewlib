open Grew_utils
open Grew_graph
open Grew_rule
open Grew_ast

module Rewrite_history: sig
  type t = {
      instance: Instance.t;
      module_name: string; 
      good_nf: t list;
      bad_nf: Instance.t list;
    }

  val is_empty: t -> bool

  val num_sol: t -> int
    
  val error_html: 
    ?main_feat: string -> 
    ?dot: bool ->
    ?init_graph:bool -> 
    ?header:string -> 
    string -> 
    string -> 
    Instance.t option -> 
    unit

  val save_html: 
    ?main_feat: string -> 
    ?dot: bool ->
    ?init_graph:bool -> 
    ?out_gr:bool -> 
    ?header:string -> 
    graph_file:string -> 
    string -> 
    t -> 
    unit

  val save_gr: string -> t -> unit
end

module Modul: sig
  type t = {
    name: string;
    local_labels: (string * string option) array;
    rules: Rule.t list;
    filters: Rule.t list;
    confluent: bool;
    loc: Loc.t;
  }
end



module Grs: sig
  type t

  val empty:t
  
  val get_modules: t -> Modul.t list

  val sequence_names: t -> string list

  val build: Ast.grs -> t

  val rewrite: t -> string -> Instance.t -> Rewrite_history.t

  (* only externeal strucutre is returned, each edge contains a "dummy" big_step *)
  val build_rew_display: t -> string -> Instance.t -> Grew_types.rew_display

  val rule_iter: (string -> Rule.t -> unit) -> t -> unit
  val filter_iter: (string -> Rule.t -> unit) -> t -> unit

  val modules_of_sequence: t -> string -> Modul.t list
end
