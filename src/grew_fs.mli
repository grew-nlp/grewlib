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
(* module [G_fs] defines the feature structures that are used in graphs *)
module G_fs: sig
  type t

  val empty: t

  (** [set_feat domain feature_name atom t] adds the feature ([feature_name],[atom]) in [t].
      If [t] already contains a feature named [feature_name], the old value is erased by the new one. *)
  val set_feat: ?loc:Loc.t -> Feature_domain.t -> feature_name -> string -> t ->  t

  (** [del_feat feature_name t] remove the feature with name [feature_name] in [t].
      If [t] does not contain such a feature, [t] is returned unchanged. *)
  val del_feat:  string -> t ->  t

  (** [get_string_atom f t] returns [Some v] if the fs [t] contains the feature (f,v).
      It returns [None] if there is no feature named [f] in [t] *)
  val get_string_atom: string -> t -> string option

  val get_float_feat: string -> t -> float option
  val to_gr: t -> string
  val to_dot: ?decorated_feat:(string * string list) -> ?main_feat: string -> t -> string
  val to_word: ?main_feat: string -> t -> string
  val to_dep: ?decorated_feat:(string * string list) -> ?position:float -> ?main_feat: string -> ?filter: string list -> t -> string
  val to_raw: t -> (string * string) list
  val to_conll: ?exclude: string list -> t -> string

  (** [get_annot_info fs] searches for a feature with name starting with "__".
      It returns the feature_name without the prefix "__"
      raise an [Build] exception if there is more than one such feature. *)
  val get_annot_info: t -> string option

  val to_string: t -> string

  val build: Feature_domain.t -> Ast.feature list -> t

  val of_conll: ?loc:Loc.t -> Feature_domain.t -> Conll.line -> t

  (** [unif t1 t2] returns [Some t] if [t] is the unification of two graph feature structures
      [None] is returned if the two feature structures cannot be unified. *)
  val unif: t -> t -> t option
end (* module G_fs *)

(* ================================================================================ *)
(* module [P_fs] defines the feature structures that are used in patterns *)
module P_fs: sig
  type t

  val empty: t

  val build: Feature_domain.t -> ?pat_vars: string list -> Ast.feature list -> t

  val to_string: t -> string

  val to_dep: ?filter: string list -> (string list * string list) -> t -> string

  val to_dot: t -> string

  val feat_list: t -> string list

  exception Fail

  (** [match_ ?param p_fs g_fs] tries to match the pattern fs [p_fs] with the graph fs [g_fs].
      If [param] is [None], it returns [None] if matching succeeds and else raise [Fail].
      If [param] is [Some p], it returns [Some p'] if matching succeeds and else raise [Fail].
   *)
  val match_: ?param:Lex_par.t -> t -> G_fs.t -> Lex_par.t option

  (** [check_position ?parma position pfs] checks wheter [pfs] is compatible with a node at [position].
      It returns [true] iff [pfs] has no requirement about position ok if the requirement is satisfied. *)
  val check_position: ?param:Lex_par.t -> float -> t -> bool

  exception Fail_unif
  (** [unif fs1 fs2] returns the unification of the two feature structures.
      It raises [Fail_unif] exception in case of Failure.
  *)
  val unif: t -> t -> t
end (* module P_fs *)
