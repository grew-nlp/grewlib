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
open Grew_base
open Grew_graph
open Grew_rule
open Grew_grs

val css_file: string

exception Parsing_err of string
exception File_dont_exists of string

(** raised when a Gr/Grs structure fails to build *)
exception Build of string * (string * int) option

(** raised during rewriting when a command is undefined *)
exception Run of string * (string * int) option

exception Bug of string * (string * int) option

val set_timeout: float option -> unit

val rewrite: gr:Instance.t -> grs:Grs.t -> seq:string -> Rewrite_history.t

val is_empty: Rewrite_history.t -> bool

val num_sol: Rewrite_history.t -> int

(** display a gr with a grs in a rew_display
@param gr the grapth to rewrite
@param grs the graph rewriting system
@param seq the name of the sequence to apply
@return a structure {b {i easily}} displayable *)
val display: gr:Instance.t -> grs:Grs.t -> seq:string -> rew_display

val write_stat: string -> Rewrite_history.t -> unit

val empty_grs: Grs.t

(** get a graph rewriting system from a file
@return a graph rewriting system
@raise Parsing_err if libgrew can't parse the file
@raise File_dont_exists if the file doesn't exists
*)
val load_grs: string -> Grs.t

(** [build_html_doc directory grs ] *)
val build_html_doc: ?corpus:bool -> string -> Grs.t -> unit

(** give the list of sequence names defined in a GRS
@return a string list
*)
val get_sequence_names: Grs.t -> string list

val to_sentence: ?main_feat:string -> Instance.t -> string

val save_graph_conll: string -> Instance.t -> unit

val save_gr: string -> Rewrite_history.t -> unit

val save_conll: string -> Rewrite_history.t -> unit

val save_det_gr: string -> Rewrite_history.t -> unit

val save_det_conll: ?header:string -> string -> Rewrite_history.t -> unit

val det_dep_string: Rewrite_history.t -> string option

val conll_dep_string: ?keep_empty_rh:bool -> Rewrite_history.t -> string option

(** get a graph from a file either in 'gr' or 'conll' format.
File extension should be '.gr' or '.conll'.
@raise Parsing_err if libgrew can't parse the file
@raise File_dont_exists if the file doesn't exists
*)
val load_graph: string -> Instance.t

val of_conll: string -> (int * string) list -> Instance.t

val xml_graph: Xml.xml -> Instance.t

(** [raw_graph instance] returns all graph information with a triple of elementary caml types:
    - the meta data
    - the list of node (node is a list of feature (feature is string * string))
    - the list of edge (src, label, tar) where src and tar refers to the position in the node list
*)
val raw_graph: Instance.t ->
    (string * string) list *
    (string * string) list list *
    (int * string * int) list

val save_index: dirname:string -> base_names: string list -> unit

val write_annot: title:string -> string -> string -> (string * Rewrite_history.t) list -> unit

val write_html:
    ?no_init: bool ->
    ?out_gr: bool ->
    ?filter: string list ->
    ?main_feat: string ->
    ?dot: bool ->
    header: string ->
    ?graph_file: string ->
    Rewrite_history.t -> string -> unit

val error_html:
    ?no_init:bool ->
    ?main_feat:string ->
    ?dot: bool ->
    header: string ->
    string ->
    ?init:Instance.t ->
    string ->
    unit

val make_index:
    title: string ->
    grs_file: string ->
    html: bool ->
    grs: Grs.t ->
    seq: string ->
    input_dir: string ->
    output_dir: string ->
    base_names: string list ->
      unit

val html_sentences: title:string -> string -> (bool * string * int * string) list -> unit

val graph_of_instance: Instance.t -> G_graph.t

val feature_names: unit -> string list option

val to_dot_graph : ?main_feat:string -> ?deco:deco -> graph -> string
val to_dep_graph : ?filter: string list -> ?main_feat:string -> ?deco:deco -> graph -> string
val to_gr_graph: graph -> string
val to_conll_graph: graph -> string

