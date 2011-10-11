open Grew_utils
open Grew_ast

module Label : sig
  (* a label declaration: (the label,an optionnal color) *)
  type decl = string * string option

  type t

  val init: decl list -> unit
	
  val to_string:t -> string
  val to_int: t -> int

  val from_string:  ?loc:Loc.t ->  ?locals:decl array -> string -> t
      
end

module Edge :  sig
  type t
  val as_label: t -> Label.t
  val of_label: Label.t -> t

  val get_id: t -> string option 

  (* [all] is the joker pattern edge *)   
  val all: t

  val make:
      ?id: string option ->
      ?neg:bool ->
      ?locals:Label.decl array ->
      string list ->
  	t

  val build: ?locals:Label.decl array -> Ast.edge -> t

  val compare: 'a -> 'a -> int
  val build_edge: string -> int * int * t
  val to_string: t -> string
  val to_dot: ?deco:bool -> t -> string
  val to_dep: ?deco:bool -> t -> string

  val compatible : t -> t -> bool

  val is_in : t -> Label.t list -> bool


  type edge_matcher = 
    | Fail 
    | Ok of Label.t
    | Binds of string * Label.t list


  val match_: t -> t -> edge_matcher

  val match_list: t -> t list -> edge_matcher

end
