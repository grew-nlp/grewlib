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
module Label_domain : sig
  type t
  
  (* [decl] is the type for a label declaration: the name and a list of display options *)
  type decl = string * string list

  val build: decl list -> t
end

(* ================================================================================ *)
module Feature_domain: sig
  type feature_spec =
    | Closed of feature_name * feature_atom list (* cat:V,N *)
    | Open of feature_name (* phon, lemma, ... *)
    | Num of feature_name (* position *)

  type t

  val build: feature_spec list -> t

  (** [sub domain fn1 fn2] returns [true] iff the domain of [fn1] is a subset if the domain of [fn2]. *)
  val sub:  t -> feature_name -> feature_name -> bool

  val build_closed: feature_name -> feature_atom list -> feature_spec

end (* module Feature_domain *)

(* ================================================================================ *)
module Domain : sig
  type t

  val build: Label_domain.t -> Feature_domain.t -> t

  val feature_names: t -> string list

  (** [is_open_feature domain feature_name] returns [true] iff no domain is set or if [feature_name] is defined to be open in the current domain. *)
  val is_open_feature: ?domain: t -> feature_name -> bool

  (** [check_feature ~loc domain feature_name feature_value] fails iff a domain is set and [feature_name,feature_value] is not defined in the current domain. *)
  val check_feature: ?loc:Loc.t -> ?domain: t -> feature_name -> feature_atom -> unit

  (** [check_feature_name ~loc domain feature_name] fails iff a domain is set and [feature_name] is not defined in the current domain. *)
  val check_feature_name: ?loc:Loc.t -> ?domain:t -> feature_name -> unit
end

(* ================================================================================ *)
(** The module [Label] defines the type of atomic label edges *)
module Label : sig
  type t

  (** [match_list p_label_list g_label] returns [true] iff [g_label] match at least one of the p_label of [p_label_list] *)
  val match_list: t list -> t -> bool

  val to_string: ?domain:Domain.t -> t -> string

  val is_void: ?domain: Domain.t -> t -> bool

  val to_dep: ?domain: Domain.t -> ?deco:bool -> t -> string

  val to_dot: ?domain: Domain.t -> ?deco:bool -> t -> string

  val from_string: ?loc:Loc.t -> ?domain: Domain.t -> ?locals:Label_domain.decl array -> string -> t
end (* module Label *)

(* ================================================================================ *)
module Feature_value: sig
  val build_disj: ?loc:Loc.t -> ?domain: Domain.t -> feature_name -> feature_atom list -> value list

  val build_value: ?loc:Loc.t -> ?domain: Domain.t -> feature_name -> feature_atom -> value
end (* module Feature_domain *)


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

(* ================================================================================ *)
module Strategy : sig

  (* recursive definition of strategies *)
  type def =
    | Ref of string     (* reference to a module name or to another strategy *)
    | Seq of def list   (* a sequence of strategies to apply one after the other *)
    | Plus of def list  (* a set of strategies to apply in parallel *)
    | Star of def       (* a strategy to apply iteratively *)
    | Diamond of def    (* pick one normal form a the given strategy *)
  (* /!\ The list must not be empty in the Seq or Plus constructor *)
    | Sequence of string list (* compatibility mode with old code *)


  (* string dump of a strat *)
  val to_string : def -> string

  (* build an equivalent strategies where embedded Seq in Seq (resp Plus in Plus) are flattened *)
  val flatten : def -> def

  (* a strategy is given by its descrition in the grs file and the 4 fields: *)
  type t = {
    name:string;     (* a unique name of the stratgy *)
    def:def;         (* the definition itself *)
    doc:string list; (* lines of docs (if any in the GRS file) *)
    loc:Loc.t;       (* the location of the [name] of the strategy *)
  }

end (* module Strategy *)

