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

type value = String of string | Float of float

val string_of_value : value -> string

val conll_string_of_value : value -> string

type disjunction = value list

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
  type t = int

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
(** module for rules that are lexically parametrized *)
module Lex_par: sig
  type t

  val append: t -> t -> t

  val dump: t -> unit

  val size: t -> int

  (** [signature t] returns (number of pattern parameters, number of lexical parameters) *)
  val signature: t -> (int * int)

  (** [from_lines filename nb_pattern_var nb_command_var strings] *)
  val from_lines: ?loc: Loc.t -> int -> int -> string list -> t

  (** [load ?loc local_dir_name nb_pattern_var nb_command_var file] *)
  val load: ?loc: Loc.t -> string -> int -> int -> string -> t

  (** [select index atom t] returns the subset of [t] which contains only entries
      which refers to [atom] at the [index]^th pattern_var.
      [None] is returned if no such entry s founded.
   *)
  val select: int -> string -> t -> t option

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
