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
(** The module [Label_cst] defines contraints on label edges *)
module Label_cst : sig
  type t =
  | Pos of Label.t list
  | Neg of Label.t list
  | Regexp of (Str.regexp * string)

  val to_string: Domain.t -> t -> string
  val all: t
  val match_: Domain.t -> t -> Label.t -> bool
  val build: ?loc:Loc.t -> Domain.t -> Ast.edge_label_cst -> t
end (* module Label_cst *)


(* ================================================================================ *)
(** The module [G_edge] defines the type of Graph label edges: atomic edges *)
module G_edge: sig
  type t = Label.t

  val to_string: Domain.t -> t -> string

  val make: ?loc:Loc.t -> Domain.t -> string -> t

  val build: Domain.t -> Ast.edge -> t

  val to_dot: Domain.t -> ?deco:bool -> t -> string
  val to_dep: Domain.t -> ?deco:bool -> t -> string
end (* module G_edge *)

(* ================================================================================ *)
(** The module [G_edge] defines the type of Graph label edges: atomic edges *)
module P_edge: sig
  type t

  (* [all] is the joker pattern edge *)
  val all: t

  val get_id: t -> string option

  val to_string: Domain.t -> t -> string

  val build: Domain.t -> Ast.edge -> t

  type edge_matcher =
    | Fail
    | Ok of Label.t
    | Binds of string * Label.t list

  val match_: Domain.t -> t -> G_edge.t -> edge_matcher

  val match_list: Domain.t -> t -> G_edge.t list -> edge_matcher
end (* module P_edge *)
