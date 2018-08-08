(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2013 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: http://grew.loria.fr                                               *)
(*    License: CeCILL (see LICENSE folder or "http://www.cecill.info")            *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

module String_set : Set.S with type elt = string
module String_map : Map.S with type key = string

module Int_set : Set.S with type elt = int
module Int_map : Map.S with type key = int

(* ================================================================================ *)
(* [Loc] general module to describe errors location: (file name, line number in file) *)
module Loc: sig
  type t

  val empty: t

  val file_line: string -> int -> t
  val file_opt_line: string option -> int -> t
  val file_opt_line_opt: string option -> int option -> t
  val file: string -> t

  val set_line: int -> t -> t

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
end

(* ================================================================================ *)
(* [Pid_set] *)
module String_: sig
  (* [to_float]: robust conversion of string to float whatever is the locale *)
  val to_float: string -> float

  (* [to_float]: robust conversion of float to string whatever is the locale *)
  val of_float: float -> string

  val re_match: Str.regexp -> string -> bool

  (* [rm_first_char s] returns the string [s] without the first charater if s is not empty.
     If s in empty, the empty string is returned  *)
  val rm_first_char: string -> string

  (* [rm_peripheral_white s] returns the string [s] without any white space ot tab
    at the beginning or at the end of the string. *)
  val rm_peripheral_white: string -> string
end


(* ================================================================================ *)
(* [Dot] function to manipulate the dot format *)
module Dot: sig
  val to_png_file: string -> string -> unit
end

(* ================================================================================ *)
(* [File] functions to read/write file *)
module File: sig
  (** [write data file_name] write [data] in file named [file_name] *)
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

  (** [get_suffix file_name] returns the suffix in [file_name].
      "x.y.z" -> Some ".z"
      "xyz" -> None  *)
  val get_suffix: string -> string option
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
  val dicho_find_assoc: 'a -> ('a*'b) array -> int
end

(* ================================================================================ *)
(* [List_] contains additional functions on the caml [list] type. *)
module List_: sig
  (** [rm elt list] removes the first occurence of [elt] in [list]. [Not_found] can be raised. *)
  val rm: 'a -> 'a list -> 'a list
  val opt: 'a option list -> 'a list

  val set: int -> 'a -> 'a list -> 'a list

  (** [cut size list] returns a list with the [size] first elements of [list].
      If [list] contains less than [size] elements, the input list is returned *)
  val cut: int -> 'a list -> 'a list

  (** [index elt list] return [Some index] if [index] is the smallest position in the [list] equals to [elt].
      None is returned if [elt] is not in the [list] *)
  val index: 'a -> 'a list -> int option

  val opt_map: ('a -> 'b option) -> 'a list -> 'b list

  val try_map: exn -> ('a -> 'b) -> 'a list -> 'b list

  val opt_mapi: (int -> 'a -> 'b option) -> 'a list -> 'b list

  val flat_map: ('a -> 'b list) -> 'a list -> 'b list
  (* remove [elt] from [list]. raise Not_found if [elt] is not in [list] *)
  val remove: 'a -> 'a list -> 'a list

  val foldi_left: (int -> 'a -> 'b -> 'a) -> 'a -> 'b list -> 'a

  (** [disjoint_list] returns true iff the two strictly ordered list are disjoint *)
  val sort_disjoint: 'a list -> 'a list -> bool

  val to_string: ('a -> string) -> string -> 'a list -> string
  val rev_to_string: ('a -> string) -> string -> 'a list -> string

  val sort_mem: 'a -> 'a list -> bool

  (* Insert an element in a sorted list. *)
  val sort_insert: 'a -> 'a list -> 'a list

  (* may raise [Not_found] *)
  val usort_remove: 'a -> 'a list -> 'a list

  (* Insert an element in a usort list. Return Some l or None if the element is already in the list *)
  val usort_insert: ?compare:('a -> 'a -> int) -> 'a -> 'a list -> 'a list option

  val sort_is_empty_inter: 'a list -> 'a list -> bool
  val sort_inter: 'a list -> 'a list -> 'a list
  val sort_union: 'a list -> 'a list -> 'a list
  val sort_disjoint_union: ?compare:('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
  val sort_include: 'a list -> 'a list -> bool
  val sort_included_diff: 'a list -> 'a list -> 'a list
  val sort_diff: 'a list -> 'a list -> 'a list

  val sort_assoc: 'a -> ('a * 'b) list -> 'b option

   (* [sort_remove_assoc k ass_list] returns the input list without the [key] element,
      if [key] not found, the unchanged input list is returned *)
  val sort_remove_assoc: 'a -> ('a * 'b) list -> ('a * 'b) list

  val sort_remove_assoc_opt: 'a -> ('a * 'b) list -> ('a * 'b) list option

  val foldi_left: (int -> 'a -> 'b -> 'a) -> 'a -> 'b list -> 'a

  val prev_next_iter: (?prev:'a -> ?next:'a -> 'a -> unit) -> 'a list -> unit
end

(* ================================================================================ *)
module type OrderedType =
  sig
    type t
      (** The type of the map keys. *)
    val compare : t -> t -> int
      (** A total ordering function over the keys.
          This is a two-argument function [f] such that
          [f e1 e2] is zero if the keys [e1] and [e2] are equal,
          [f e1 e2] is strictly negative if [e1] is smaller than [e2],
          and [f e1 e2] is strictly positive if [e1] is greater than [e2].
          Example: a suitable ordering function is the generic structural
          comparison function {!Pervasives.compare}. *)
  end
(** Input signature of the functor {!Map.Make}. *)

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
  end

(* ================================================================================ *)
module Massoc_make (Ord : OrderedType) : S with type key = Ord.t


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

  val grewpy_compare: name -> name -> int
end

(* ================================================================================ *)
module Timeout: sig
  exception Stop

  val timeout: float option ref
  val start: unit -> unit

  val check: unit -> unit
end

(* ================================================================================ *)
module Global: sig
  val new_file: string -> unit
  val new_string: unit -> unit
  val new_line: unit -> unit

  val get_loc: unit -> Loc.t
  val get_line: unit -> int option
  val get_dir: unit -> string
  val loc_string: unit -> string
  val label_flag: bool ref

  val debug: bool ref
  val safe_commands: bool ref
end
