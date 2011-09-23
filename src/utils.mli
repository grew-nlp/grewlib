
module IntSet : Set.S with type elt = int

module IntMap : sig
  include Map.S with type key = int
  exception MatchNotInjective
  val exists: (key -> 'a -> bool) -> 'a t -> bool
  val union_if: int t -> int t -> int t
  end

module StringMap : Map.S with type key = string
module StringSet : Set.S with type elt = string

module Loc: sig
  type t = string * int 

  val to_string: t -> string
end



module File: sig 
  val write: string -> string -> unit

  val read: string -> string list
end

module Array_: sig
  val dicho_mem: 'a -> 'a array -> bool

  (* dichotomic search: the array MUST be sorted and without duplicates. Not found can be raised *)
  val dicho_find: 'a -> 'a array -> int
  val dicho_find_assoc: 'a -> ('a*'b) array -> int
end

module List_: sig
  (** [rm elt list] removes the first occurence of [elt] in [list]. [Not_found] can be raised. *)
  val rm: 'a -> 'a list -> 'a list
  val opt: 'a option list -> 'a list

  val opt_map: ('a -> 'b option) -> 'a list -> 'b list

  val flat_map: ('a -> 'b list) -> 'a list -> 'b list
  (* remove [elt] from [list]. raise Not_found if [elt] is not in [list] *)      
  val remove: 'a -> 'a list -> 'a list

  val foldi_left: (int -> 'a -> 'b -> 'a) -> 'a -> 'b list -> 'a

  (** [disjoint_list] returns true iff the two strictly ordered list are disjoint *)
  val sort_disjoint: 'a list -> 'a list -> bool

  val to_string: ('a -> string) -> string -> 'a list -> string

  val iteri: (int -> 'a -> unit) -> 'a list -> unit

  val mapi: (int -> 'a -> 'b) -> 'a list -> 'b list

  (* Insert an element in a sorted list. *)
  val sort_insert: 'a -> 'a list -> 'a list

  (* Insert an element in a usort list. Return Some l or None if the element is already in the list *)
  val usort_insert: ?compare:('a -> 'a -> int) -> 'a -> 'a list -> 'a list option

  val sort_is_empty_inter: 'a list -> 'a list -> bool
  val sort_inter: 'a list -> 'a list -> 'a list
  val sort_disjoint_union: ?compare:('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
  val sort_include: 'a list -> 'a list -> bool
  val sort_included_diff: 'a list -> 'a list -> 'a list
  val sort_diff: 'a list -> 'a list -> 'a list

end

(* module Massoc implements multi-association data: keys are int and the same key can be
associated with a set of values *)
module Massoc: sig
  type 'a t

  val empty: 'a t

  (* an empty list returned if the key is undefined *) 
  val assoc: int -> 'a t -> 'a list

  val is_empty: 'a t -> bool

  val to_string: ('a -> string) -> 'a t -> string

  val iter: (int -> 'a -> unit) -> 'a t -> unit

  val add: int -> 'a -> 'a t -> 'a t option

  val fold_left: ('b -> int -> 'a -> 'b) -> 'b -> 'a t -> 'b

  (* raise Not_found if no (key,elt) *)
  val remove: int -> 'a -> 'a t -> 'a t
      
  (* raise Not_found if no (key,elt) *)
  val remove_key: int -> 'a t -> 'a t

  (* [mem key value t ] test if the couple (key, value) is in the massoc [t]. *)
  val mem: int -> 'a -> 'a t -> bool

  (* mem_key key t] tests is [key] is associated to at least one value in [t]. *)
  val mem_key: int -> 'a t -> bool

  exception Not_disjoint
  val disjoint_union: 'a t -> 'a t -> 'a t 

  exception Duplicate
  val merge_key: int -> int -> 'a t -> 'a t

  val exists: (int -> 'a -> bool) -> 'a t -> bool
end
    
exception Build of (string * Loc.t option)
exception Run of (string * Loc.t option)
exception Bug of (string * Loc.t option)


module Error: sig
  val build: ?loc: Loc.t -> ('a, unit, string, 'b) format4 -> 'a
  val run: ?loc: Loc.t -> ('a, unit, string, 'b) format4 -> 'a
  val bug: ?loc: Loc.t -> ('a, unit, string, 'b) format4 -> 'a
end




module Id: sig
  type name = string
  type t = int

  type table = name array

  (* [Stop] is raised if [string] is not in [table] *)
  val build: ?loc:Loc.t -> name -> table -> t

  val build_opt: name -> table -> t option 
end

module Html: sig
  val enter: out_channel -> ?title: string -> ?header: string -> string -> unit
  val leave: out_channel -> unit
end
