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
(* [List_] contains additional functions on the caml [list] type. *)
module List_: sig
  val set: int -> 'a -> 'a list -> 'a list

  (** [cut size list] returns a list with the [size] first elements of [list].
      If [list] contains less than [size] elements, the input list is returned *)
  val cut: int -> 'a list -> 'a list

  (** [index elt list] return [Some index] if [index] is the smallest position in the [list] equals to [elt].
      None is returned if [elt] is not in the [list] *)
  val index_opt: 'a -> 'a list -> int option

  val opt_map: ('a -> 'b option) -> 'a list -> 'b list

  val try_map: exn -> ('a -> 'b) -> 'a list -> 'b list

  val opt_mapi: (int -> 'a -> 'b option) -> 'a list -> 'b list

  val flat_map: ('a -> 'b list) -> 'a list -> 'b list

  (** [remove elt list] remove the first occurence od [elt] in [list].
      raise Not_found if [elt] is not in [list] *)
  val remove: 'a -> 'a list -> 'a list

  val foldi_left: (int -> 'a -> 'b -> 'a) -> 'a -> 'b list -> 'a

  (* list intersection. Not efficient, do not use on large list *)
  val intersect: 'a list -> 'a list -> 'a list

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

  val foldi_left: (int -> 'a -> 'b -> 'a) -> 'a -> 'b list -> 'a

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
module Massoc_gid : S with type key = Gid.t

(* ================================================================================ *)
module Massoc_pid : S with type key = Pid.t

(* ================================================================================ *)
module Massoc_string : S with type key = string


(* ================================================================================ *)
module Projection : sig
  type t

  val empty: t

  val cardinal: t -> int
  
  val insert: string option list -> t -> t
  
  val prune_unambiguous: int -> t -> t

  val to_json: string list -> t -> Yojson.Basic.t
end (* module Projection *)
