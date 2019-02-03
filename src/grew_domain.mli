(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2018 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: http://grew.fr                                                     *)
(*    License: CeCILL (see LICENSE folder or "http://www.cecill.info")            *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open Grew_base
open Grew_types
open Grew_ast

(* ================================================================================ *)
module Label_domain : sig
  type style
  val parse_option: string -> string list -> style
  val is_void: style -> bool

  val to_dep: ?deco:bool -> style -> string
  val to_dot: ?deco:bool -> style -> string

  val default: style
  type t

  (* [decl] is the type for a label declaration: the name and a list of display options *)
  type decl = string * string list

  val merge: decl list -> decl list -> decl list

  val build: decl list -> t

end

(* ================================================================================ *)
module Feature_domain: sig
  type t

  val build: Ast.feature_spec list -> t

  (** [sub domain fn1 fn2] returns [true] iff the domain of [fn1] is a subset if the domain of [fn2]. *)
  val sub:  t -> feature_name -> feature_name -> bool

  val merge: Ast.feature_spec list -> Ast.feature_spec list -> Ast.feature_spec list

end (* module Feature_domain *)

(* ================================================================================ *)
module Domain : sig
  type t

  val dump: t option -> unit

  val to_json: t -> Yojson.Basic.json

  val build: Label_domain.t -> Feature_domain.t -> t
  val build_features_only: Feature_domain.t -> t
  val build_labels_only: Label_domain.t -> t

  val build_disj : ?loc:Loc.t -> ?domain:t ->
           feature_name ->
           feature_atom list -> value list


  val feature_names: t -> string list

  val get_label_name: ?domain:t -> int -> string option
  val get_label_style: ?domain:t -> int -> Label_domain.style option

  val edge_id_from_string: ?loc:Loc.t -> ?domain:t -> string -> int option

  (** [is_open_feature domain feature_name] returns [true] iff no domain is set or if [feature_name] is defined to be open in the current domain. *)
  val is_open_feature: ?domain: t -> feature_name -> bool

  (** [is_num domain feature_name] returns [true] iff the domain is set and [feature_name] is defined to be numerical *)
  val is_num: ?domain: t -> feature_name -> bool

  (** [check_feature ~loc domain feature_name feature_value] fails iff a domain is set and [feature_name,feature_value] is not defined in the current domain. *)
  val check_feature: ?loc:Loc.t -> ?domain: t -> feature_name -> feature_atom -> unit

  (** [check_feature_name ~loc domain feature_name] fails iff a domain is set and [feature_name] is not defined in the current domain. *)
  val check_feature_name: ?loc:Loc.t -> ?domain:t -> feature_name -> unit
end
