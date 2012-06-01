open Grew_utils
open Grew_ast

module Domain: sig
  val reset: unit -> unit

  val init: Ast.domain -> unit
end

(* [G_fs] define the ferute srtuctures that are used in graphs *)
module G_fs: sig
  type t

  val empty: t
  
  (** [set_feat feature_name atom t] adds the feature ([feature_name],[atom]) in [t].
      If [t] already contains a feature named [feature_name], the old value is erased by the new one. *)
  val set_feat: ?loc:Loc.t -> string -> string -> t ->  t

  (** [del_feat feature_name t] remove the feature with name [feature_name] in [t].
      If [t] does not contain such a feature, [t] is returned unchanged. *)
  val del_feat:  string -> t ->  t

  (** [get_atom f t] returns [Some v] if the fs [t] contains the feature (f,v).
      It returns [None] if there is no feature named [f] in [t] *)
  val get_atom: string -> t -> string option

  val to_gr: t -> string
  val to_dot: ?main_feat: string -> t -> string
  val to_word: ?main_feat: string -> t -> string
  val to_dep: ?main_feat: string -> t -> string

  val to_string: t -> string

  val build: Ast.feature list -> t

  val of_conll: Conll.line -> t

  (** [unif t1 t2] returns [Some t] if [t] is the unification of two graph feature structures
      [None] is returned if the two feature structures cannot be unified. *)
  val unif: t -> t -> t option
end
  
module P_fs: sig
  type t

  val empty: t
  
  val build: ?pat_vars: string list -> Ast.feature list -> t
  
  val to_string: t -> string

  val to_dep: (string list * string list) -> t -> string

  val to_dot: t -> string

  exception Fail

  (** [match_ ?param t gfs] tries to match the pattern fs [pfs] with the graph fs [gfs] 
      If [param] is [None], it returns [None] if matching succeeds and else raise [Fail].
      If [param] is [Some p], it returns [Some p'] if matching succeeds and else raise [Fail].
   *) 
  val match_: ?param:Lex_par.t -> t -> G_fs.t -> Lex_par.t option

  val filter: t -> G_fs.t -> bool

  val unif: t -> t -> t
end
