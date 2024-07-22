(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2024 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: https://grew.fr                                                    *)
(*    License: CeCILL (see LICENSE folder or "http://cecill.info/")               *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open Grew_types
open Grew_utils
open Grew_ast

(* ================================================================================ *)
(* module [G_fs] defines the feature structures that are used in graphs *)
module G_fs: sig
  type t

  val empty: t

  val get_features: t -> String_set.t

  (** [set_atom feature_name atom t] adds the feature ([feature_name],[atom]) in [t].
      If [t] already contains a feature named [feature_name], the old value is erased by the new one. *)
  val set_atom: ?loc:Loc.t -> string -> string -> t ->  t

  val set_value: string -> Feature_value.t -> t ->  t

  (** [del_feat_opt feature_name t] remove the feature with name [feature_name] in [t].
      If [t] does not contain such a feature, None is returned. *)
  val del_feat_opt:  string -> t -> t option

  (** [del_feat_opt feature_name t] remove the feature with name [feature_name] in [t].
      If [t] does not contain such a feature, [t] is returned unchanged. *)
  val del_feat:  string -> t -> t

  val get_value_opt: string -> t -> Feature_value.t option

  val to_dot: ?decorated_feat:(string * (string * string option) list) -> ?main_feat: string -> t -> string
  val to_word_opt: t -> string option

  val to_json: t -> Yojson.Basic.t

  val to_dep:
    ?decorated_feat:(string * (string * string option) list) ->
    ?tail:string list ->
    ?main_feat: string ->
    ?filter: (string -> bool) ->
    t ->
    string

  val to_string: t -> string

  val of_ast: Ast.feature list -> t

  val of_items: (string * string) list -> t

  val pst_leaf: ?loc:Loc.t -> string -> t
  val pst_node: ?loc:Loc.t -> string -> t

  val concat_feats_opt: ?loc:Loc.t -> Ast.side -> t -> t -> string -> Regexp.t -> (t * (string * Feature_value.t) list) option

end (* module G_fs *)

(* ================================================================================ *)
(* module [P_fs] defines the feature structures that are used in requests *)
module P_fs: sig
  type t

  val empty: t

  val of_ast: Lexicons.t -> Ast.feature list -> t

  val build_atom: Cmp.t -> string -> Feature_value.t -> t

  val to_string: t -> string

  val to_dep: ?filter: (string -> bool) -> t -> string

  val to_dot: t -> string

  val feat_list: t -> (string * string option) list

  exception Fail

  (** [match_ ?lexicons p_fs g_fs] tries to match the request fs [p_fs] with the graph fs [g_fs].
      The bool returned is [true] iff the lexicons was changed during the matching *)
  val match_: ?lexicons:Lexicons.t -> t -> G_fs.t -> (bool * Lexicons.t)

  exception Fail_unif

  (** [unif_disj fs_disj_1 fs_disj_2] returns the unification of the two feature structures disjunction.
      It raises [Fail_unif] exception in case of Failure (i.e. empty disjunction). *)
  val unif_disj: t list -> t list -> t list

end (* module P_fs *)
