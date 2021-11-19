(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2021 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: https://grew.fr                                                     *)
(*    License: CeCILL (see LICENSE folder or "http://cecill.info/")            *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open Grew_base
open Grew_ast

(* ================================================================================ *)
(* [Pid] describes identifier used in pattern graphs *)
module Pid : sig
  type t = Ker of int | Ext of int
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
module Gid_map : Map.S with type key = Gid.t

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

  (** [load file] build a lexicon from a file.
      The file should contain same data than the ones in the build function
      in separate lines, each line used tabulation as separator *)
  val load: ?loc:Loc.t -> string -> t

  (** [reduce headers lexicon] build a smaller lexicon restricted to a subset of columns (defined in [headers]) *)
  (* val reduce: string list -> t -> t *)

  (** [union lex1 lex2] returns the union of two lexicons
      It supposed that the two lexicons define the same columns *)
  val union: t -> t -> t

  (** [filter_opt head value] returns the sublexicon with only items where the [head] column is match (Eq or Neq) to [value] if any, else returns None *)
  val filter_opt: cmp -> string -> string -> t -> t option

  (** [read head lexicon] return the list of [value] of all items having in the [head] column equals to [value] *)
  val read_all: string -> t -> string list

  (** [get_opt head lexicon] return [value] if one items have the [value] in the [head] field *)
  val get_opt: string -> t -> string option

  (** [read_multi head lexicon] returns "v_1/…/v_k" where v_i are the values of the [head] column *)
  val read_multi: string -> t -> string

  val of_ast: ?loc:Loc.t -> string option -> Ast.lexicon -> t
end (* module Lexicon *)

(* ================================================================================ *)
module Lexicons : sig
  type t = (string * Lexicon.t) list

  val check: ?loc:Loc.t -> string -> string -> t -> unit
end (* module Lexicons *)

(* ================================================================================ *)
module Projection : sig
  type t

  val empty: t

  val cardinal: t -> int
  
  val insert: string option list -> t -> t
  
  val prune_unambiguous: int -> t -> t

  val to_json: string list -> t -> Yojson.Basic.t
end (* module Projection *)
