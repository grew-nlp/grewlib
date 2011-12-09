open Grew_utils
open Grew_ast

(* ================================================================================ *)
(** The module [Label] defines the type of atomic label edges *)

module Label : sig
  (* a label declaration: (the label,an optionnal color) *)
  type decl = string * string option

  type t

  val init: decl list -> unit
	
  val to_string: t -> string
  val to_int: t -> int

  val from_string: ?loc:Loc.t -> ?locals:decl array -> string -> t      
end



(* ================================================================================ *)
(** The module [G_edge] defines the type of Graph label edges: atomic edges *)
module G_edge: sig
  type t = Label.t

  val to_string:t -> string

  val make: ?locals:Label.decl array -> string -> t

  val build: ?locals:Label.decl array -> Ast.edge -> t

  val to_dot: ?deco:bool -> t -> string
  val to_dep: ?deco:bool -> t -> string
end
(* ================================================================================ *)


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


end
(* ================================================================================ *)
