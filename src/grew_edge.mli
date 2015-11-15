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
open Grew_types

open Grew_ast

(* ================================================================================ *)
(** The module [G_edge] defines the type of Graph label edges: atomic edges *)
module G_edge: sig
  type t = Label.t

  val to_string: Label_domain.t -> ?locals:Label_domain.decl array -> t -> string

  val make: ?loc:Loc.t -> Label_domain.t -> ?locals:Label_domain.decl array -> string -> t

  val build: Label_domain.t -> ?locals:Label_domain.decl array -> Ast.edge -> t

  val to_dot: Label_domain.t -> ?deco:bool -> t -> string
  val to_dep: Label_domain.t -> ?deco:bool -> t -> string
end (* module G_edge *)

(* ================================================================================ *)
(** The module [G_edge] defines the type of Graph label edges: atomic edges *)
module P_edge: sig
  type t

  (* [all] is the joker pattern edge *)
  val all: t

  val get_id: t -> string option

  val to_string: Label_domain.t -> t -> string

  val build: Label_domain.t -> ?locals:Label_domain.decl array -> Ast.edge -> t

  type edge_matcher =
    | Fail
    | Ok of Label.t
    | Binds of string * Label.t list

  val match_: Label_domain.t -> t -> G_edge.t -> edge_matcher

  val match_list: Label_domain.t -> t -> G_edge.t list -> edge_matcher
end (* module P_edge *)
