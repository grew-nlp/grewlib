(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2021 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: https://grew.fr                                                     *)
(*    License: CeCILL (see LICENSE folder or "http://cecill.info/")            *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

module String_set : Set.S with type elt = string
module String_map : Map.S with type key = string
module String_opt_map : Map.S with type key = string option

module Int_set : Set.S with type elt = int
module Int_map : Map.S with type key = int

val ( << ) : ('a -> 'b) -> ('c -> 'a) -> ('c -> 'b)

type cmp = Eq | Neq
val string_of_cmp: cmp -> string
val cmp_fct: cmp -> ('a -> 'a -> bool)

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
  val to_json: t ->  Yojson.Basic.t
end

(* ================================================================================ *)
module String_: sig
  (* [re_match regexp s] returns true iff the full string [s] matches with [regexp] *)
  val re_match: Str.regexp -> string -> bool

  (* [rm_first_char s] returns the string [s] without the first charater if s is not empty.
     If [s] in empty, the empty string is returned  *)
  val rm_first_char: string -> string

  val rev_concat: string -> string list -> string
  
  (** Pyhton like substring extraction
     [get_range (init_opt, final_opt) s] return the python output of s[init_opt:final_opt]
     NB: indexes correspond to UTF-8 chars. ex: [get_range (None, Some (-1)) "été"] ==> "ét"
  *)
  val get_range: Range.t -> string -> string
end (* module String_ *)

(* ================================================================================ *)
(* [File] functions to read/write file *)
module File: sig
  (** [write data file_name] write [data] in [file_name] *)
  val write: string -> string -> unit

  (** [read file_name] read the content of [file_name] line by line.
      Blanks lines (empty or only with spaces and tabs) are ignored.
      Lines with '%' as the first char are ignored. *)
  val read: string -> string list

  (** [read_ln file_name] read the content of [file_name] line by line.
      Blanks lines (empty or only with spaces and tabs) are ignored.
      Lines with '%' as the first char are ignored.
      Each line is returned with its position in the original file. *)
  val read_ln: string -> (int * string) list

  (** [load file_name] load the content of [file_name] as a string. *)
  val load: string -> string

  (** [get_suffix_opt file_name] returns the suffix in [file_name].
      "x.y.z" -> Some ".z"
      "xyz" -> None  *)
  val get_suffix_opt: string -> string option
end

(* ================================================================================ *)
(* [Array_] contains additional functions on the caml [array] type. *)
module Array_: sig
  (* [dicho_mem elt array] returns true iff [elt] belongs to [array].
     Warning: the array MUST be sorted and without duplicates. *)
  val dicho_mem: 'a -> 'a array -> bool

  (* [dicho_find elt array] returns the index of the position where [elt] is found in the [array].
     [Not found] is raised if [elt] is not in [array].
     Warning: the array MUST be sorted and without duplicates. *)
  val dicho_find: 'a -> 'a array -> int

  (* [dicho_find_assoc key array] returns the value associated with [key] in the assoc [array].
     [Not found] is raised if [key] is not defined in [array].
     Warning: the array MUST be sorted (with respect to the first component) and without duplicates. *)
  val dicho_find_assoc: 'a -> ('a * 'b) array -> int
end (* module Array_ *)



(* ================================================================================ *)
module Id: sig
  type t = int

  type 'a gtable = 'a array * ('a -> string)

  (* [Stop] is raised if [string] is not in [gtable] *)
  val gbuild: ?loc:Loc.t -> 'a -> 'a gtable -> t

  val gbuild_opt: 'a -> 'a gtable -> t option

  type name = string
  type table = string array

  (* [Stop] is raised if [string] is not in [table] *)
  val build: ?loc:Loc.t -> name -> table -> t

  val build_opt: name -> table -> t option

  (* [get_pos id] returns Some v (float) iff id is "Wv" else None *)
  val get_pos_opt: name -> float option
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
end

(* ================================================================================ *)
module Dependencies : sig
  (* [is_projective arcs] returns [true] iff the structure is projective.
     Input: a list of arcs represented by couples (smallest position, highest position) and lexicographically ordered *)
  val is_projective: (int * int) list -> bool
end



