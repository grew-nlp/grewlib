(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2022 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: https://grew.fr                                                    *)
(*    License: CeCILL (see LICENSE folder or "http://cecill.info/")               *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

module String_set : Set.S with type elt = string
module String_map : Map.S with type key = string
module String_opt_map : Map.S with type key = string option

module Int_set : Set.S with type elt = int
module Int_map : Map.S with type key = int

type cluster_item = Key of string | Whether of string

(* ================================================================================ *)
module Clustered : sig
  type 'a t
  (** the type Clustered.t stores the output of a (mutli) clustering operation.
      It is a recusive dict of homogeneous depth, dict keys are [string option].
      NB: the handling of the homogeneity is left to the user: in each function using a list,
      the length of the list must correspond to the depth of the structure.
  *)

  val empty: 'a -> 'a t
  (** The [empty] structure (the null value should be given) *)

  val get_opt: 'a -> string option list -> 'a t -> 'a
  (** [get_opt null key_list t] return correpsonding the 'a value or [null] if it can not be found *)

  val size: 'a t -> int
  (** Returns the number of element of type 'a stored in the structure *)

  val update: ('a -> 'a) -> string option list -> 'a -> 'a t -> 'a t
  (** [update fct string_option_list null t] update the structure: 
      - if there is already an 'a value [x] at address [string_option_list],
        this value is updatesd to [fct x]
      - else a new entry is added with value [fct null]
  *)

  val fold: (string option list -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  (** [fold fct t init] apply the function [fct] to all leaves *)

  val iter: (string option list -> 'a -> unit) -> 'a t -> unit
  (** [iter fct t] apply the function [fct] to all leaves *)

  val fold_layer: 
    ('a -> 'b) ->                        (* fct on leaves *)
    'c ->                                (* init value for node iteration *)
    (string option -> 'b -> 'c -> 'c) -> (* iteration of element of the node *)
    ('c -> 'b) ->                        (* closure *)
    'a t ->
    'b
  (** [fold fct_leaf null fct_node closure] 
      folds in all the structures (nodes and leaves).
      [fct_leaf] is applied on each leaf;
      on each node, [fct_node] is folded, starting from [null] and finally [closure] is applied to it
  *)

  val prune_unambiguous: int -> 'a t -> 'a t
  (** [prune_unambiguous n t] prunes in input [t] with unambiguous structure at depth [n].
      For instance, if the keys are values for features [form, lemma, upos, Gender, Number],
      [prune_unambiguous 3 t] will keep only clusters entries where there is 
      more than one couple of value for Gender and Number with the same triple (form, lemma, upos).
  *)

  val dump: ('a -> string) -> 'a t -> unit
  (** outputs a raw displat of the structure (to be used only for debug) *)

  val get_all_keys: int -> _ t -> string option list
  
end (* module Clustered *)
