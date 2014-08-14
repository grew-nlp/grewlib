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
open Grew_ast

(* ================================================================================ *)
(** The module [Label] defines the type of atomic label edges *)

module Label : sig
  (* [decl] is the type for a label declaration: the name and a list of diplay options *)
  type decl = string * string list

  type t

  val init: decl list -> unit
	
  val to_string: ?locals:decl array -> t -> string

  val to_int: t -> int option

  val from_string: ?loc:Loc.t -> ?locals:decl array -> string -> t
end (* module Label *)



(* ================================================================================ *)
(** The module [G_edge] defines the type of Graph label edges: atomic edges *)
module G_edge: sig
  type t = Label.t

  val to_string: ?locals:Label.decl array -> t -> string

  val root: t

  val make: ?loc:Loc.t -> ?locals:Label.decl array -> string -> t

  val build: ?locals:Label.decl array -> Ast.edge -> t

  val to_dot: ?deco:bool -> t -> string
  val to_dep: ?deco:bool -> t -> string
end (* module G_edge *)


(* ================================================================================ *)
(** The module [G_edge] defines the type of Graph label edges: atomic edges *)
module P_edge: sig
  type t

  (* [all] is the joker pattern edge *)
  val all: t

  val get_id: t -> string option
  val to_string: t -> string

  val build: ?locals:Label.decl array -> Ast.edge -> t

  val make:
    ?loc:Loc.t ->
    ?id: string option ->
    ?neg:bool ->
    ?locals:Label.decl array ->
    string list ->
    t

  val compatible: t -> G_edge.t -> bool

  type edge_matcher =
    | Fail
    | Ok of Label.t
    | Binds of string * Label.t list

  val match_: t -> G_edge.t -> edge_matcher

  val match_list: t -> G_edge.t list -> edge_matcher
end (* module P_edge *)
