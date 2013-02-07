

(** {2 Grew's core} *)
open Grew_utils
open Grew_graph
open Grew_rule
open Grew_grs

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
val load_grs : ?doc_output_dir:string -> string -> Grs.t

(** give the list of sequence names defined in a GRS 
@return a string list
*)
val get_sequence_names: Grs.t -> string list

val to_sentence: ?main_feat:string -> Instance.t -> string

val save_gr: string -> Rewrite_history.t -> unit

val save_det_gr: string -> Rewrite_history.t -> unit

(** get a graph from a file either in 'gr' or 'conll' format.
File extension should be '.gr' or '.conll'.
@raise Parsing_err if libgrew can't parse the file
@raise File_dont_exists if the file doesn't exists
*)
val load_graph: string -> Instance.t

(** [raw_graph instance] returns all graph information with a triple of basic caml types:
    - the meta data
    - the list of node (node is a list of feature (feature is string * string))
    - the list of edge (src, label, tar) where src and tar refers to the position in the node list
*)
val raw_graph: Instance.t ->
    (string * string) list *
    (string * string) list list *
    (int * string * int) list

val save_index: dirname:string -> base_names: string list -> unit

val write_html: 
    ?no_init: bool ->
    ?out_gr: bool ->
    ?main_feat: string -> 
    ?dot: bool ->
    header: string ->
    graph_file: string ->
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

val get_css_file: string

val graph_of_instance: Instance.t -> G_graph.t

val feature_names: unit -> string list option
