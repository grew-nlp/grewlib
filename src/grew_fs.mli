open Utils

module Feature: sig
  val check: ?domain:Ast.domain -> Loc.t -> string -> string list -> unit
end

module Feature_structure: sig
  type t

      val build: ?domain:Ast.domain -> Ast.feature list -> t
      val get: string -> t -> string list
      val empty: t
      val to_string: t -> string
      val to_dep: ?main_feat: string -> t -> string
      val set_feat:  string -> string list -> t ->  t
      val del_feat:  string -> t ->  t
      val compatible: t -> t -> bool


      (** [unif t1 t2] returns [Some t] if [t] is the unification of two graph feature structures
	  [None] is returned if the two feature structures cannot be unified
	  [Bug_unif <msg>] is raised if inputs are not correct graph feature structures *)
      val unif: t -> t -> t option

      val unifiable: t -> t -> bool
      val filter: t -> t -> bool
    end
