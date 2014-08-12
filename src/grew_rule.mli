(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2013 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: http://grew.loria.fr                                               *)
(*    License: CeCILL (see LICENSE folder or "http://www.cecill.info")            *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open Libgrew_utils
open Grew_graph
open Grew_command
open Grew_edge
open Grew_fs
open Grew_ast

(* ================================================================================ *)
module Instance : sig
  type t = {
    graph: G_graph.t;
    history: Command.h list;
    rules: string list;
    big_step: Grew_types.big_step option;
    free_index: int;
    activated_node: Gid.t list;
  }

  (** [from_graph graph] return a fresh instance based on the input [graph]. *)
  val from_graph: G_graph.t -> t

  (** [rev_steps t] reverses the small step list: during rewriting, the last rule
      is in the head of the list and the reverse is needed for display. *)
  val rev_steps: t -> t

  (** [flatten inst] returns a fresh representation of the graph where gid created by node
      activation are map to basic gid. Graphs are flattened after each module. *)
  val flatten: t -> t

  (** [to_gr t] returns a string which contains the "gr" code of the current graph *)
  val to_gr: t -> string

  (** [to_conll t] returns a string which contains the "conll" code of the current graph *)
  val to_conll: t -> string

  (** [save_dep_png base t] writes a file "base.png" with the dep representation of [t].
      NB: if the Dep2pict is not available, nothing is done *)
  val save_dep_png: ?filter: string list -> ?main_feat: string -> string -> t -> float option
  val save_dep_svg: ?filter: string list -> ?main_feat: string -> string -> t -> float option

  (** [save_dot_png base t] writes a file "base.png" with the dot representation of [t] *)
  val save_dot_png: ?filter: string list -> ?main_feat: string -> string -> t -> unit
end (* module Instance *)


(* ================================================================================ *)
module Instance_set : Set.S with type elt = Instance.t

(* ================================================================================ *)
module Rule : sig
  type t

  (** [get_name t] returns the name of the rule [t]. *)
  val get_name: t -> string

  (** [get_loc t] returns the file location of the rule [t]. *)
  val get_loc: t -> Loc.t

  (** [is_filter t] returns [true] iff the rule [t] is a filter rule. *)
  val is_filter: t -> bool

  (** [to_dep t] returns a string in the [dep] language describing the pattern. *)
  val to_dep: t -> string

  (** [build ?local dir ast_rule] returns the Rule.t value corresponding to [ast_rule].
      [dir] is used for localisation of lp files *)
  val build: ?locals:Label.decl array -> string -> Ast.rule -> t

  (** [normalize module_name ?confluent rule_list filter_list instance] returns two sets of good normal forms and bad normal forms *)
  (* raise Stop if some command fails to apply *)
  val normalize:
    string -> (* module name *)
    ?confluent:bool ->
    t list -> (* rule list *)
    t list -> (* filter list *)
    Instance.t ->
      Instance_set.t * Instance_set.t

  type matching
  val match_in_graph: t -> G_graph.t -> matching list

end (* module Rule *)
