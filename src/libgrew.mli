(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2013 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: http://grew.loria.fr                                               *)
(*    License: CeCILL (see LICENSE folder or "http://www.cecill.info")            *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

(** {2 Grew's core} *)
open Libgrew_types
open Grew_base
open Grew_types
open Grew_graph
open Grew_rule
open Grew_grs


(* -------------------------------------------------------------------------------- *)
(** {2 Location} *)
type loc
val line_of_loc: loc -> int
val string_of_loc: loc -> string

(* -------------------------------------------------------------------------------- *)
(** {2 Exceptions} *)
exception File_dont_exists of string

exception Parsing_err of string * loc option

(** raised when a Gr/Grs structure fails to build *)
exception Build of string * loc option

(** raised during rewriting when a command is undefined *)
exception Run of string * loc option

exception Bug of string * loc option

(* -------------------------------------------------------------------------------- *)
(** {2 Domain} *)
type domain

val load_domain: string -> domain

(* -------------------------------------------------------------------------------- *)
(** {2 Graph} *)

(** get a graph from a file either in 'gr' or 'conll' format.
    File extension should be '.gr' or '.conll'.
    @raise Parsing_err if libgrew can't parse the file
    @raise File_dont_exists if the file doesn't exists. *)
val load_graph: domain -> string -> graph

(** [of_conll filename line_list] *)
val of_conll: domain -> string -> (int * string) list -> graph

val of_brown: domain -> ?sentid:string -> string -> graph

val to_sentence: ?main_feat:string -> graph -> string

(** [raw_graph instance] returns all graph information with a triple of elementary caml types:
    - the meta data
    - the list of node (node is a list of feature (feature is string * string))
    - the list of edge (src, label, tar) where src and tar refers to the position in the node list
*)
val raw_graph: domain -> graph ->
    (string * string) list *
    (string * string) list list *
    (int * string * int) list

val to_dot_graph : domain -> ?main_feat:string -> ?deco:deco -> graph -> string

val to_dep_graph : domain -> ?filter: string list -> ?main_feat:string -> ?deco:deco -> graph -> string

val to_gr_graph: domain -> graph -> string

val to_conll_graph: domain -> graph -> string


(* -------------------------------------------------------------------------------- *)
(** {2 Graph Rewriting System} *)
type grs
val empty_grs: grs

(** [load_grs filename] loads a graph rewriting system from [filename]
    @raise Parsing_err if libgrew can't parse the file
    @raise File_dont_exists if the file doesn't exists *)
val load_grs: string -> grs

(** [get_sequence_names grs] returns the list of sequence names defined in a GRS *)
val get_sequence_names: grs -> string list

(** [build_html_doc ?corpus directory grs]
    @[corpus] is a flag (default is [false]) for complete html doc with corpus sentence. *)
val build_html_doc: ?corpus:bool -> string -> grs -> unit

val feature_names: domain -> string list option

(* -------------------------------------------------------------------------------- *)
(** {2 rew_display: data for the GUI } *)
(** [display gr grs seq] builds the [rew_display] given by
    the rewriting of graph [gr] with the sequence [seq] of [grs].
    @param gr the grapth to rewrite
    @param grs the graph rewriting system
    @param seq the name of the sequence to apply *)
val display: gr:graph -> grs:grs -> seq:string -> rew_display

(* -------------------------------------------------------------------------------- *)
(** {2 Rewrite history} *)
type rewrite_history

val set_timeout: float option -> unit

val rewrite: gr:graph -> grs:grs -> seq:string -> rewrite_history

val is_empty: rewrite_history -> bool

val num_sol: rewrite_history -> int

val write_stat: string -> rewrite_history -> unit

val save_gr: domain -> string -> rewrite_history -> unit

val save_conll: domain -> string -> rewrite_history -> unit

(** [save_full_conll base_name rh] saves one conll_file for each normal form defined in [rh].
    Output files are named according to [base_name] and a secondary index after "__".
    The number of conll file produced is returned. *)
val save_full_conll: domain -> string -> rewrite_history -> int

val save_det_gr: domain -> string -> rewrite_history -> unit

val save_det_conll: domain -> ?header:string -> string -> rewrite_history -> unit

val det_dep_string: domain -> rewrite_history -> string option

val conll_dep_string: domain -> ?keep_empty_rh:bool -> rewrite_history -> string option

val save_index: dirname:string -> base_names: string list -> unit

val write_annot: domain -> title:string -> string -> string -> (string * rewrite_history) list -> unit

val write_html:
    domain ->
    ?no_init: bool ->
    ?out_gr: bool ->
    ?filter: string list ->
    ?main_feat: string ->
    ?dot: bool ->
    header: string ->
    ?graph_file: string ->
    rewrite_history ->
    string -> 
    unit

val error_html:
    domain ->
    ?no_init:bool ->
    ?main_feat:string ->
    ?dot: bool ->
    header: string ->
    string ->
    ?init:graph ->
    string ->
    unit

val make_index:
    title: string ->
    grs_file: string ->
    html: bool ->
    grs: grs ->
    seq: string ->
    input_dir: string ->
    output_dir: string ->
    base_names: string list ->
    unit

val html_sentences: title:string -> string -> (bool * string * int * string) list -> unit

(* -------------------------------------------------------------------------------- *)
(** {2 Patterns} *)

(* type and function added for the grep mode of grew *)
type pattern
type matching

(** [load_pattern filename] returns the pattern described in the file *)
val load_pattern: domain -> string -> pattern

(** [match_in_graph pattern graph] returns the list of the possible matching og [pattern] in [graph] *)
val match_in_graph: domain -> pattern -> graph -> matching list

(** [match_deco pattern matching] returns the deco to be used in the graphical representation.
    WARNING: the function supposes that [matching] was find with the given [pattern]! *)
val match_deco: pattern -> matching -> deco
