open Grew_utils
open Grew_ast

module Feature: sig
  val check: ?domain:Ast.domain -> Loc.t -> string -> string list -> unit
end

module Feature_structure: sig
  type t

  val build: ?pat_vars: string list -> ?domain:Ast.domain -> Ast.feature list -> t
      
  val of_conll: Conll.line -> t

  val get: string -> t -> string list option
      
  val get_atom: string -> t -> string option

      val empty: t
      val to_string: t -> string
      val to_dot: ?main_feat: string -> t -> string
      val to_gr: t -> string
      val to_dep: ?main_feat: string -> t -> string

      (** [set_feat feature_name atoms t] adds the feature ([feature_name],[atoms]) in [t].
          If [t] already contains a feature named [feature_name], the old value is erased by the new one. *)
      val set_feat:  string -> string list -> t ->  t

      (** [del_feat feature_name t] remove the feature with name [feature_name] in [t].
          If [t] does not contain such a feature, [t] is returned unchanged. *)
      val del_feat:  string -> t ->  t

      val compatible: t -> t -> bool

  val compatible_param: (string list * string list) list -> t -> t -> (string list * string list) list

      (** [unif t1 t2] returns [Some t] if [t] is the unification of two graph feature structures
	  [None] is returned if the two feature structures cannot be unified
	  [Bug_unif <msg>] is raised if inputs are not correct graph feature structures *)
      val unif: t -> t -> t option

      val unifiable: t -> t -> bool
      val filter: t -> t -> bool
    end
