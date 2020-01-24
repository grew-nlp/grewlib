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

type feature_name = string (* cat, num, ... *)
type feature_atom = string (* V, N, inf, ... *)
type feature_value = string (* V, 4, "free text", ... *)

type value =
  | String of string
  | Float of float

val string_of_value : value -> string

val conll_string_of_value : value -> string

type disjunction = value list

val to_uname: feature_name -> feature_name

(* ================================================================================ *)
(* [Pid] describes identifier used in pattern graphs *)
module Pid : sig
  type t = Pos of int | Neg of int
  val compare: t -> t -> int
  val to_id: t -> string
  val to_string: t -> string
end (* module Pid *)

(* ================================================================================ *)
(* [Pid_set] *)
module Pid_set : Set.S with type elt = Pid.t

(* ================================================================================ *)
(* [Pid_map] is the map used in pattern graphs *)
module Pid_map : sig
  include Map.S with type key = Pid.t

  val exists: (key -> 'a -> bool) -> 'a t -> bool
end (* module Pid_map *)


(* ================================================================================ *)
(* [Gid] describes identifier used in full graphs *)
module Gid : sig
  type t = int

  val compare: t -> t -> int

  val to_string: t -> string
end (* module Gid *)

(* ================================================================================ *)
(* [Gid_map] is the map used in full graphs *)
module Gid_map : sig
  include Map.S with type key = Gid.t

  val map_key_value: (key -> key) -> ('a -> 'a) -> 'a t -> 'a t

  (* return the first key where the test is true *)
  val search_key: ('a -> bool) -> 'a t -> key option
end

module Gid_set : Set.S with type elt = Gid.t

(* ================================================================================ *)
module Massoc_gid : S with type key = Gid.t

(* ================================================================================ *)
module Massoc_pid : S with type key = Pid.t

(* ================================================================================ *)
module Massoc_string : S with type key = string

(* ================================================================================ *)
module Lexicon : sig
  type t

  (** [build loc items] build a lexicon from a list.
      The first list is interpreted as the column headers.
      All other lines are lexicon items.
      It is supposed that all sublist have the same length *)
  val build: Loc.t -> (int * string) list -> t

  (** [load file] build a lexicon from a file.
      The file should contain same data than the ones in the build function
      in separate lines, each line used tabulation as separator *)
  val load: Loc.t -> string -> t

  (** [reduce headers lexicon] build a smaller lexicon restricted to a subset of columns (defined in [headers]) *)
  val reduce: string list -> t -> t

  (** [union lex1 lex2] returns the union of two lexicons
      It supposed that the two lexicons define the same columns *)
  val union: t -> t -> t

  (** [select head value] returns the sublexicon with only items where the [head] column is equal to [value] if any, else returns None *)
  val select: string -> string -> t -> t option

  (** [unselect head value] returns the sublexicon with only items where the [head] column is different to [value] if any, else returns None *)
  val unselect: string -> string -> t -> t option

  exception Not_functional_lexicon

  (** [read head lexicon] return [value] if all items have in the [head] column equals to [value]
      * raise [Not_functional_lexicon] if several values are defined *)
  val read: string -> t -> string

  (** [read head lexicon] return the list of [value] of all items having in the [head] column equals to [value] *)
  val read_all: string -> t -> string list

  (** [get head lexicon] return [value] if one items have the [value] in the [head] field *)
  val get: string -> t -> string

  (** [read_multi head lexicon] returns "v_1/…/v_k" where v_i are the values of the [head] column *)
  val read_multi: string -> t -> string
end (* module Lexicon *)

(* ================================================================================ *)
module Lexicons : sig
  type t = (string * Lexicon.t) list

  val check: loc:Loc.t -> string -> string -> t -> unit
end (* module Lexicons *)

(* ================================================================================ *)
module Concat_item : sig
  type t =
    | Feat of (Gid.t * feature_name)
    | String of string
end (* module Concat_item *)
