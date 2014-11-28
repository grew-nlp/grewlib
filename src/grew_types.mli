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

type feature_name = string (* cat, num, ... *)
type feature_atom = string (* V, N, inf, ... *)
type feature_value = string (* V, 4, "free text", ... *)
type suffix = string


type value = String of string | Float of float

val string_of_value : value -> string

val conll_string_of_value : value -> string

(* ================================================================================ *)
(* [Pid] describes identifier used in pattern graphs *)
module Pid : sig
  type t = Pos of int | Neg of int
  val compare: t -> t -> int
  val to_id: t -> string
  val to_string: t -> string
end (* module Pid *)

(* ================================================================================ *)
(* [Pid_map] is the map used in pattern graphs *)
module Pid_map : sig
  include Map.S with type key = Pid.t

  val exists: (key -> 'a -> bool) -> 'a t -> bool
end (* module Pid_map *)

(* ================================================================================ *)
(* [Pid_set] *)
module Pid_set : Set.S with type elt = Pid.t

(* ================================================================================ *)
(* [Gid] describes identifier used in full graphs *)
module Gid : sig
  type t =
    | Old of int
    | New of (int * int) (* identifier for "created nodes" *)
    | Act of (int * string)  (* identifier for "activated nodes" *)

  val compare: t -> t -> int

  val to_string: t -> string
end (* module Gid *)

(* ================================================================================ *)
(* [Gid_map] is the map used in full graphs *)
module Gid_map : Map.S with type key = Gid.t

(* ================================================================================ *)
module Massoc_gid : S with type key = Gid.t

(* ================================================================================ *)
module Massoc_pid : S with type key = Pid.t

(* ================================================================================ *)
(** The module [Label] defines the type of atomic label edges *)
module Label : sig
  (* [decl] is the type for a label declaration: the name and a list of display options *)
  type decl = string * string list

  type t

  val init: decl list -> unit

  val to_string: ?locals:decl array -> t -> string

  val to_int: t -> int option

  val to_dep: ?deco:bool -> t -> string

  val to_dot: ?deco:bool -> t -> string

  val from_string: ?loc:Loc.t -> ?locals:decl array -> string -> t
end (* module Label *)

(* ================================================================================ *)
module Domain: sig
  type feature_spec =
    | Closed of feature_name * feature_atom list (* cat:V,N *)
    | Open of feature_name (* phon, lemma, ... *)
    | Num of feature_name (* position *)

  type t = feature_spec list
  val normalize_domain: t -> t

  val reset: unit -> unit

  val init: t -> unit

  val build: ?loc:Loc.t -> feature_name -> feature_atom list -> value list

  val build_one: ?loc:Loc.t -> feature_name -> feature_atom -> value

  val feature_names: unit -> string list option

  (** [check_feature_name ~loc feature_name] fails iff a domain is set and [feature_name] is not defined in the current domain. *)
  val check_feature_name: ?loc:Loc.t -> feature_name -> unit

  (** [check_feature ~loc feature_name feature_value] fails iff a domain is set and [feature_name,feature_value] is not defined in the current domain. *)
  val check_feature: ?loc:Loc.t -> feature_name -> feature_atom -> unit

  (** [is_open feature_name] returns [true] iff no domain is set or if [feature_name] is defined to be open in the current domain. *)
  val is_open: feature_name -> bool

end (* module Domain *)

(* ================================================================================ *)
module Conll: sig
  type line = {
    line_num: int;
    num: string;
    phon: string;
    lemma: string;
    pos1: string;
    pos2: string;
    morph: (string * string) list;
    deps: (string * string ) list;
  }

  val line_to_string: line -> string

  val root:line

  val load: string -> line list

  val parse: string -> (int * string) list -> line list

  val compare: line -> line -> int
end (* module Conll *)

(* ================================================================================ *)
(** module for rules that are lexically parametrized *)
module Lex_par: sig
  type t

  val empty:t
  val append: t -> t -> t

  val dump: t -> unit

  (** [from_lines filename nb_pattern_var nb_command_var strings] *)
  val from_lines: ?loc: Loc.t -> int -> int -> string list -> t

  (** [load ?loc local_dir_name nb_pattern_var nb_command_var file] *)
  val load: ?loc: Loc.t -> string -> int -> int -> string -> t

  (** [filter index atom t] returns the subset of [t] which contains only entries
      which refers to [atom] at the [index]^th pattern_var.
      [None] is returnes if no such entry s founded.
   *)
  val filter: int -> string -> t -> t option

  (** [get_param_value index t] returns the [index]^th param_var. *)
  val get_param_value: int -> t -> string

  (** [get_command_value index t] supposes that [t] contains iny one element.
      It returns the [index]^th command_var. *)
  val get_command_value: int -> t -> string
end (* module Lex_par *)

(* ================================================================================ *)
module Concat_item : sig
  type t =
  | Feat of (Gid.t * feature_name)
  | String of string
end (* module Concat_item *)



