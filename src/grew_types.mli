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
module String_opt_map : CCMap.S with type key = string option

module Int_set : Set.S with type elt = int
module Int_map : Map.S with type key = int

(* ================================================================================ *)
module Clustered : sig
  type 'a t
  (** the type Clustered.t stores the output of a (mutli) clustering operation.
      It is a recusive dict of homogeneous depth, dict keys are [string option].
      NB: the handling of the homogeneity is left to the user: in each function using a list,
      the length of the list must correspond to the depth of the structure.
  *)

  val depth: _ t -> int

  val empty: 'a -> 'a t
  (** The [empty] structure (the null value should be given) *)

  val get_opt: 'a -> string option list -> 'a t -> 'a
  (** [get_opt null key_list t] returns the corresponding 'a value or [null] if it cannot be found *)

  val nb_clusters: 'a t -> int
  (** Returns the number of element of type 'a stored in the structure *)

  val cardinal: ('a -> int) -> 'a t -> int
  (** Returns the number of element of type 'a stored in the structure *)

  val sizes: ('a -> int) -> 'a t -> int String_opt_map.t list
  (** [sizes f t] returns a list of map (the length of the list is the depth of [t]).
      each element of the list sums the usage of the key in all sub structs *)

  val build_layer: ('b -> 'a t) -> ('b -> string option) -> 'a -> 'b list -> 'a t
  (** [build_layer sub_fct key_fct null item_list] builds a structure which first layer
      associates from each [item] in [item_list], [key_fct item] to [sub_fct item] *)

  val update: ('a -> 'a) -> string option list -> 'a -> 'a t -> 'a t
  (** [update fct string_option_list null t] update the structure: 
      - if there is already an 'a value [x] at address [string_option_list],
        this value is updated to [fct x]
      - else a new entry is added with value [fct null]
  *)

  val map: ('a -> 'b) -> 'a t -> 'b t
  (** [map fct t] apply the function [fct] to all leaves *)

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

  val merge_keys: string option -> ('a -> 'a -> 'a) -> 'a -> (string option -> bool) list -> 'a t -> 'a t
  (** [merge_keys new_key merge_cell_fct null filter_functions input] 
      builds a new Clustered.t with the same data but where some keys are merged.
      [new_key] is the name of the new key used for merged keys (__*__ in grew-match grids)
      [merge_cell_fct] on merged cells;
      [filter_functions] is a list of boolean functions [f](one for each depth):
        * if [f key] is true, the key is kept
        * if [f key] is false, the corresponding data is merge into the [new_key] data 
  *)

  val prune_unambiguous: int -> 'a t -> 'a t
  (** [prune_unambiguous n t] prunes in input [t] with unambiguous structure at depth [n].
      For instance, if the keys are values for features [form, lemma, upos, Gender, Number],
      [prune_unambiguous 3 t] will keep only clusters entries where there is 
      more than one couple of value for Gender and Number with the same triple (form, lemma, upos).
  *)

  val dump: ('a -> string) -> 'a t -> unit
  (** outputs a raw display of the structure (to be used only for debug) *)

  val get_all_keys: int -> _ t -> string option list
  
end (* module Clustered *)
