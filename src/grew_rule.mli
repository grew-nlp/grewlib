open Grew_utils
open Grew_graph
open Grew_command
open Grew_edge
open Grew_fs
open Grew_ast

module Instance : sig
  type t = {
      graph: Graph.t;
      commands: Command.h list;
      rules: string list;
      big_step: Grew_types.big_step option; 
    }
        
  val empty:t

  val build: Ast.gr -> t	

  (* rev_steps reverse the small step list: during rewriting, the last rule is in the head of the list and the reverse is needed for display *) 
  val rev_steps: t -> t

  val clear: t -> t 
  val from_graph: Graph.t -> t
  val get_graph: t -> Graph.t

IFDEF DEP2PICT THEN
  (* [save_dep_png base t] writes a file "base.png" with the dep representation of [t] *)
  val save_dep_png: ?main_feat: string -> string -> t -> unit
ENDIF
end

module Instance_set : Set.S with type elt = Instance.t

module Rule : sig
  type t

  val get_name: t -> string

  val get_loc: t -> Loc.t

  val build: ?domain:Ast.domain -> ?locals:Label.decl array -> Ast.rule -> t

  (* raise Stop if some command fails to apply *)
  val normalize: 
    ?confluent:bool -> 
    t list ->
    (Instance_set.elt -> bool) ->
    Instance.t ->
      Instance_set.t * Instance_set.t
end
