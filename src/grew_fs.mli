(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2018 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: http://grew.fr                                                     *)
(*    License: CeCILL (see LICENSE folder or "http://www.cecill.info")            *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open Conll

open Grew_base
open Grew_types
open Grew_ast
open Grew_domain

(* ================================================================================ *)
module Feature_value: sig
  val build_disj: ?loc:Loc.t -> ?domain: Domain.t -> feature_name -> feature_atom list -> value list

  val build_value: ?loc:Loc.t -> ?domain: Domain.t -> feature_name -> feature_atom -> value
end (* module Feature_value *)


(* ================================================================================ *)
(* module [G_fs] defines the feature structures that are used in graphs *)
module G_fs: sig
  type t

  val empty: t

  (** [set_atom domain feature_name atom t] adds the feature ([feature_name],[atom]) in [t].
      If [t] already contains a feature named [feature_name], the old value is erased by the new one. *)
  val set_atom: ?loc:Loc.t -> ?domain:Domain.t -> feature_name -> string -> t ->  t

  val set_value: ?loc:Loc.t -> ?domain:Domain.t -> feature_name -> value -> t ->  t

  (** [del_feat_opt feature_name t] remove the feature with name [feature_name] in [t].
      If [t] does not contain such a feature, None is returned. *)
  val del_feat_opt:  string -> t ->  t option

  val get_value_opt: string -> t -> value option

  val to_gr: t -> string
  val to_dot: ?decorated_feat:(string * (string * string option) list) -> ?main_feat: string -> t -> string
  val to_word_opt: t -> string option

  val to_dep:
    ?decorated_feat:(string * (string * string option) list) ->
    ?tail:string list ->
    ?main_feat: string ->
    ?filter: (string -> bool) ->
    t ->
    string

  val to_conll: ?exclude: string list -> t -> (string * string) list
  val to_json: t -> Yojson.Basic.t
  val to_string: t -> string

  val build: ?domain:Domain.t -> Ast.feature list -> t

  val of_conll: ?loc:Loc.t -> ?domain:Domain.t -> Conll.line -> t

  val pst_leaf: ?loc:Loc.t -> ?domain:Domain.t -> string -> t
  val pst_node: ?loc:Loc.t -> ?domain:Domain.t -> string -> t

  val append_feats_opt: ?loc:Loc.t -> t -> t -> string -> string -> (t * (string * value) list) option

end (* module G_fs *)

(* ================================================================================ *)
(* module [P_fs] defines the feature structures that are used in patterns *)
module P_fs: sig
  type t

  val to_json: ?domain:Domain.t -> t -> Yojson.Basic.t

  val empty: t

  val build: ?domain:Domain.t -> Lexicons.t -> Ast.feature list -> t

  val to_string: t -> string

  val to_dep: ?filter: (string -> bool) -> t -> string

  val to_dot: t -> string

  val feat_list: t -> (string * string option) list

  exception Fail

  (** [match_ ?lexicons p_fs g_fs] tries to match the pattern fs [p_fs] with the graph fs [g_fs]. *)
  val match_: ?lexicons:Lexicons.t -> t -> G_fs.t -> Lexicons.t

  exception Fail_unif

  (** [unif fs1 fs2] returns the unification of the two feature structures.
      It raises [Fail_unif] exception in case of Failure. *)
  val unif: t -> t -> t

end (* module P_fs *)
