(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2024 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: https://grew.fr                                                    *)
(*    License: CeCILL (see LICENSE folder or "http://cecill.info/")               *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

(* ================================================================================ *)
module Cmp: sig
  (** This module defines a two values type for Equality / Disequality *)
  type t = Eq | Neq

  val to_string: t -> string

  val fct: t -> ('a -> 'a -> bool)
  (** [fct t] return a function of type 'a -> 'a -> bool which corresponds either to equality or disequality *)
end


(* ================================================================================ *)
(* [Loc] general module to describe errors location: (file name, line number in file) *)
module Loc: sig
  type t

  val empty: t

  val file_opt_line: string option -> int -> t
  val file_opt_line_opt: string option -> int option -> t
  val file: string -> t

  val set_line: int -> t -> t

  val get_line_opt: t -> int option

  val to_string: t -> string
end

(* ================================================================================ *)
module Error: sig
  exception Build of (string * Loc.t option)
  val build: ?loc: Loc.t -> ('a, unit, string, 'b) format4 -> 'a

  exception Run of (string * Loc.t option)
  val run: ?loc: Loc.t -> ('a, unit, string, 'b) format4 -> 'a

  exception Bug of (string * Loc.t option)
  val bug: ?loc: Loc.t -> ('a, unit, string, 'b) format4 -> 'a

  exception Parse of (string * Loc.t option)
  val parse: ?loc: Loc.t -> ('a, unit, string, 'b) format4 -> 'a

  val warning: ?loc: Loc.t -> ('a, unit, string, unit) format4 -> 'a

  val info: ?loc: Loc.t -> ('a, unit, string, unit) format4 -> 'a
end

(* ================================================================================ *)
module Range: sig
  type t = (int option * int option)

  val to_string: t -> string

  (** Python like substring extraction
     [extract (init_opt, final_opt) s] return the python output of s[init_opt:final_opt]
     NB: indexes correspond to UTF-8 chars. ex: [extract (None, Some (-1)) "été"] ==> "ét"
  *)
  val extract: t -> string -> string

end

(* ================================================================================ *)
module String_: sig
  (* [re_match regexp s] returns true iff the full string [s] matches with [regexp] *)
  val re_match: Str.regexp -> string -> bool

  (* [rm_first_char s] returns the string [s] without the first charater if s is not empty.
     If [s] in empty, the empty string is returned  *)
  val rm_first_char: string -> string

  (** [get_suffix_opt file_name] returns the suffix in [file_name].
      "x.y.z" -> Some ".z"
      "xyz" -> None  *)
  val get_suffix_opt: string -> string option
end

(* ================================================================================ *)
(* [Array_] contains additional functions on the caml [array] type. *)
module Array_: sig
  (* [dicho_mem elt array] returns true iff [elt] belongs to [array].
     Warning:  the array MUST be sorted and without duplicates. *)
  val dicho_mem: 'a -> 'a array -> bool

  (* [dicho_find elt array] returns the index of the position where [elt] is found in the [array].
     [Not found] is raised if [elt] is not in [array].
     Warning: the array MUST be sorted and without duplicates. *)
  val dicho_find: 'a -> 'a array -> int

  (* [dicho_find_assoc key array] returns the value associated with [key] in the assoc [array].
     [Not found] is raised if [key] is not defined in [array].
     Warning: the array MUST be sorted (with respect to the first component) and without duplicates. *)
  val dicho_find_assoc: 'a -> ('a * 'b) array -> int

  (** [shuffle_N n] builds a array which contains 0...n-1 in a random order *)
  val shuffle_N: int -> int array
end (* module Array_ *)

(* ================================================================================ *)
(* [List_] contains additional functions on the caml [list] type. *)
module List_: sig

  (** [index elt list] return [Some index] if [index] is the smallest position in the [list] equals to [elt].
      None is returned if [elt] is not in the [list] *)
  val index_opt: 'a -> 'a list -> int option

  val try_map: exn -> ('a -> 'b) -> 'a list -> 'b list

  (** [remove elt list] remove the first occurence od [elt] in [list].
      raise Not_found if [elt] is not in [list] *)
  val remove: 'a -> 'a list -> 'a list

  (* list intersection. Not efficient, do not use on large list *)
  val intersect: 'a list -> 'a list -> 'a list

  (** [disjoint_list] returns true iff the two strictly ordered list are disjoint *)
  val sort_disjoint: 'a list -> 'a list -> bool

  val sort_mem: 'a -> 'a list -> bool

  (* Insert an element in a sorted list. *)
  val sort_insert: 'a -> 'a list -> 'a list

  (* may raise [Not_found] *)
  val usort_remove: 'a -> 'a list -> 'a list

  (* Insert an element in a usort list. Return Some l or None if the element is already in the list *)
  val usort_insert_opt: ?compare:('a -> 'a -> int) -> 'a -> 'a list -> 'a list option

  val sort_is_empty_inter: 'a list -> 'a list -> bool
  val sort_inter: 'a list -> 'a list -> 'a list
  val sort_union: 'a list -> 'a list -> 'a list
  val sort_disjoint_union: ?compare:('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
  val sort_include: 'a list -> 'a list -> bool
  val sort_included_diff: 'a list -> 'a list -> 'a list
  val sort_diff: 'a list -> 'a list -> 'a list

  val sort_assoc_opt: 'a -> ('a * 'b) list -> 'b option
  val sort_mem_assoc: 'a -> ('a * 'b) list -> bool

  val sort_update_assoc: 'a -> 'b -> ('a * 'b) list -> ('a * 'b) list

  (* [sort_remove_assoc k ass_list] returns the input list without the [key] element,
     if [key] not found, the unchanged input list is returned *)
  val sort_remove_assoc: 'a -> ('a * 'b) list -> ('a * 'b) list

  val sort_remove_assoc_opt: 'a -> ('a * 'b) list -> ('a * 'b) list option

  val prev_next_iter: (?prev:'a -> ?next:'a -> 'a -> unit) -> 'a list -> unit
end

(* ================================================================================ *)
module type S =
sig
  type key

  type +'a t

  val empty: 'a t

  (* an empty list returned if the key is undefined *)
  val assoc: key -> 'a t -> 'a list

  val is_empty: 'a t -> bool

  val to_string: ('a -> string) -> 'a t -> string

  val iter: (key -> 'a -> unit) -> 'a t -> unit

  val add_opt: key -> 'a -> 'a t -> 'a t option

  val replace: key -> 'a list -> 'a t -> 'a t

  val fold: ('b -> key -> 'a -> 'b) -> 'b -> 'a t -> 'b

  val fold_on_list: ('b -> key -> 'a list -> 'b) -> 'b -> 'a t -> 'b

  (* raise Not_found if no (key,elt) *)
  val remove: key -> 'a -> 'a t -> 'a t
  val remove_opt: key -> 'a -> 'a t -> 'a t option

  (* raise Not_found if no (key,elt) *)
  val remove_key: key -> 'a t -> 'a t

  (* [mem key value t ] test if the couple (key, value) is in the massoc [t]. *)
  val mem: key -> 'a -> 'a t -> bool

  (* mem_key key t] tests is [key] is associated to at least one value in [t]. *)
  val mem_key: key -> 'a t -> bool

  exception Not_disjoint
  val disjoint_union: 'a t -> 'a t -> 'a t

  val exists: (key -> 'a -> bool) -> 'a t -> bool

  val rename: (key * key) list -> 'a t -> 'a t

  val map_key: (key -> key) -> 'a t -> 'a t

  val find_opt: (key -> 'a -> bool) -> 'a t -> key option

  val filter: ('a -> bool) -> 'a t -> 'a t

  val filter_key: (key -> bool) -> 'a t -> 'a t

end

(* ================================================================================ *)
module Massoc_make (Ord : Set.OrderedType) : S with type key = Ord.t


(* ================================================================================ *)
module Id: sig
  type t = int

  type name = string
  type table = string array

  (* [Error.build] is raised if [string] is not in [table] *)
  val build: ?loc:Loc.t -> name -> table -> t

  val build_opt: name -> table -> t option

end

(* ================================================================================ *)
module Timeout: sig
  exception Stop of float

  val timeout: float option ref
  val start: unit -> unit
  val stop: unit -> unit

  val check: unit -> unit
  
  val get_duration: unit -> float
end

(* ================================================================================ *)
module Global: sig
  val new_file: string -> unit
  val new_string: unit -> unit
  val new_line: unit -> unit

  val get_loc: unit -> Loc.t
  val get_line_opt: unit -> int option
  val loc_string: unit -> string
  val label_flag: bool ref

  val debug: bool ref
  val safe_commands: bool ref
  val track_rules: bool ref
  val track_history: bool ref
  val track_impact: bool ref

  val reset_grs_timestamp: unit -> unit
  val update_grs_timestamp: string -> unit
  val get_grs_timestamp: unit -> float
end

(* ================================================================================ *)
module Dependencies : sig
  (* [is_projective arcs] returns [true] iff the structure is projective.
     Input: a list of arcs represented by couples (smallest position, highest position) and lexicographically ordered *)
  val is_projective: (int * int) list -> bool
end

(* ================================================================================ *)
(* [Pid] describes identifier used in request graphs *)
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
(* [Pid_map] is the map used in request graphs *)
module Pid_map : sig
  include Map.S with type key = Pid.t

  val exists: (key -> 'a -> bool) -> 'a t -> bool
end (* module Pid_map *)

(* ================================================================================ *)
module Pid_massoc : S with type key = Pid.t

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
module Gid_set : Set.S with type elt = Gid.t

(* ================================================================================ *)
module Gid_massoc : S with type key = Gid.t

(* ================================================================================ *)
module Feature_value: sig
  (** Feature values can be string or a number *)
  type t =
    | String of string
    | Float of float

  (** [parse feature_name feature_value] return a feature_value with type t above
      NB: Typing float/string for feature value is hardcoded (this should evolve with a new config implementation) 
      See ml file for numeric feature list
      [Error.run] is raised if one tries to buils a numeric feature value with a non-numeric value *)
  val parse: ?loc:Loc.t -> string -> string -> t

  (** [to_string t] returns a string for the feature value
      TODO: more about quote escaping *)
  val to_string: ?quote:bool -> t -> string

  (** [to_json t] returns a JSON encoding of f as a JSON string (even for numeric values) *)
   val to_json: t -> Yojson.Basic.t

  (* val Feature_value.extract_range: Range.t -> feature_value -> feature_value *)
  val extract_range: ?loc:Loc.t -> Range.t -> t -> t

  (* val Feature_value.concat: ?loc:Loc.t -> feature_value list -> feature_value *)
  val concat: ?loc:Loc.t -> t list -> t

end (* module Feature_value *)

(* ================================================================================ *)
module Sbn: sig
  val to_json: string -> Yojson.Basic.t
end