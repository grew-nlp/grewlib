
module StringMap : Map.S with type key = string
module StringSet : Set.S with type elt = string

module IntSet : Set.S with type elt = int
module IntMap : Map.S with type key = int


(* ================================================================================ *)
(* [Loc] general module to describe errors location: (file name, line number in file) *)
module Loc: sig
  type t = string * int 

  val to_string: t -> string
end


(* ================================================================================ *)
(* [File] basic functions to read/write file *)
module File: sig
  (** [write data file_name] write [data] in file named [file_name] *)
  val write: string -> string -> unit

  (** [read file_name] read the content of [file_name] line by line. 
     Blanks lines (empty or only with spaces and tabs) are ignored.
     Lines with '%' as the first char are ignores
   *)
  val read: string -> string list
end


(* ================================================================================ *)
(* [Pid] describes identifier used in pattern graphs *)
module Pid : sig type t = int end

(* ================================================================================ *)
(* [Pid_map] is the map used in pattern graphs *)
module Pid_map : sig
  include Map.S with type key = int
  exception MatchNotInjective
  val exists: (key -> 'a -> bool) -> 'a t -> bool
  val union_if: int t -> int t -> int t
  end


module Gid : sig type t = int end

module Gid_map : Map.S with type key = Gid.t 


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
      
  (** [pos elt list] return [Some index] if [index] is the smallest position in the [list] equals to [elt]. None is returned if [elt] is not in the [list] *)
  val pos: 'a -> 'a list -> int option

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

  val sort_mem: 'a -> 'a list -> bool

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

  val sort_assoc: 'a -> ('a * 'b) list -> 'b option

   (* [sort_remove_assoc k ass_list] returns the input list without the [key] element, 
      if [key] not found, the unchanged input list is returned *)
  val sort_remove_assoc: 'a -> ('a * 'b) list -> ('a * 'b) list

  val foldi_left: (int -> 'a -> 'b -> 'a) -> 'a -> 'b list -> 'a
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
    


module Error: sig
  exception Build of (string * Loc.t option)
  exception Run of (string * Loc.t option)
  exception Bug of (string * Loc.t option)

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

module Conll: sig
  type line = {
      num: int;
      phon: string;
      lemma: string;
      pos1: string;
      pos2: string;
      morph: (string * string) list;
      gov: int;
      dep_lab: string;
    }
        
  val parse: string -> line
end

(** module for rule that are lexically parametrized *)
module Lex_par: sig
  type t

  (** [load ?loc local_dir_name nb_pattern_var nb_command_var file] *)
  val load: ?loc: Loc.t -> string -> int -> int -> string -> t

  (** [filter index atom t] returns the subset of [t] which contains only entries 
      which refers to [atom] at the [index]^th pattern_var.
      [None] is returnes if no such entry s founded.
   *)
  val filter: int -> string -> t -> t option

  (** [get_command_value index t] supposes that [t] contains iny one element.
      It returns the [index]^th command_var. *)
  val get_command_value: int -> t -> string
end

module Timeout: sig
  exception Stop

  val timeout: float option ref
  val start: unit -> unit

  val check: unit -> unit
end
