(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2013 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: http://grew.loria.fr                                               *)
(*    License: CeCILL (see LICENSE folder or "http://www.cecill.info")            *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

(* ==================================================================================================== *)
(** {2 Location} *)
(* ==================================================================================================== *)
module Loc : sig
  type t = Grew_base.Loc.t
  val to_line: t -> int
  val to_string: t -> string
end

(* ==================================================================================================== *)
(** {2 Exceptions} *)
(* ==================================================================================================== *)
exception File_dont_exists of string

exception Parsing_err of string * Loc.t option

(** raised when a Gr/Grs structure fails to build *)
exception Build of string * Loc.t option

(** raised during rewriting when a command is undefined *)
exception Run of string * Loc.t option

exception Bug of string * Loc.t option

(* ==================================================================================================== *)
(** {2 Domain} *)
(* ==================================================================================================== *)
module Domain : sig
  type t
  val load: string -> t
  val feature_names: t -> string list option
end

(* ==================================================================================================== *)
(** {2 Patterns} *)
(* ==================================================================================================== *)
module Pattern : sig
  type t

  (** [load_pattern domain filename] returns the pattern described in the file *)
  val load: Domain.t -> string -> t 
end

(* ==================================================================================================== *)
(** {2 Matching} *)
(* ==================================================================================================== *)
module Matching: sig
  type t
end

(* ==================================================================================================== *)
(** {2 Deco} *)
(* ==================================================================================================== *)
module Deco: sig
  type t = Grew_graph.G_deco.t

  (** [build pattern matching] returns the deco to be used in the graphical representation.
      WARNING: the function supposes that [matching] was find with the given [pattern]! *)
  val build: Pattern.t -> Matching.t -> t
end


(* ==================================================================================================== *)
(** {2 Graph} *)
(* ==================================================================================================== *)
module Graph : sig

  type t = Grew_graph.G_graph.t
  (** get a graph from a file either in 'gr' or 'conll' format.
      File extension should be '.gr' or '.conll'.
      @raise Parsing_err if libgrew can't parse the file
      @raise File_dont_exists if the file doesn't exists. *)
  val load: Domain.t -> string -> t

  (** [of_conll filename line_list] *)
  val of_conll: Domain.t -> string -> (int * string) list -> t

  val of_brown: Domain.t -> ?sentid:string -> string -> t

  val to_sentence: ?main_feat:string -> t -> string

  (** [raw_graph instance] returns all graph information with a triple of elementary caml types:
      - the meta data
      - the list of node (node is a list of feature (feature is string * string))
      - the list of edge (src, label, tar) where src and tar refers to the position in the node list
  *)
  val raw: Domain.t -> t ->
      (string * string) list *
      (string * string) list list *
      (int * string * int) list

  val to_dot : Domain.t -> ?main_feat:string -> ?deco:Deco.t -> t -> string

  val to_dep : Domain.t -> ?filter: string list -> ?main_feat:string -> ?deco:Deco.t -> t -> string

  val to_gr: Domain.t -> t -> string

  val to_conll: Domain.t -> t -> string

  (** [search_pattern pattern graph] returns the list of the possible matching of [pattern] in [graph] *)
  val search_pattern: Domain.t -> Pattern.t -> t -> Matching.t list
end

(* ==================================================================================================== *)
(** {2 Graph Rewriting System} *)
(* ==================================================================================================== *)
module Grs: sig

  type t
  val empty: t

  (** [load filename] loads a graph rewriting system from [filename]
      @raise Parsing_err if libgrew can't parse the file
      @raise File_dont_exists if the file doesn't exists *)
  val load: string -> t

  (** [get_sequence_names t] returns the list of sequence names defined in a GRS *)
  val get_sequence_names: t -> string list

  (** [build_html_doc ?corpus directory t]
      @[corpus] is a flag (default is [false]) for complete html doc with corpus sentence. *)
  val build_html_doc: ?corpus:bool -> string -> t -> unit

  val get_domain: t -> Domain.t
end

(* ==================================================================================================== *)
(** {2 Rewrite history} *)
(* ==================================================================================================== *)
module Rewrite: sig
  
  type display = Libgrew_types.rew_display
  type history

  (** [display gr grs seq] builds the [display] (datatype used by the GUI) given by
      the rewriting of graph [gr] with the sequence [seq] of [grs].
      @param gr the grapth to rewrite
      @param grs the graph rewriting system
      @param seq the name of the sequence to apply *)
  val display: gr:Graph.t -> grs:Grs.t -> seq:string -> display

  val set_timeout: float option -> unit

  val rewrite: gr:Graph.t -> grs:Grs.t -> seq:string -> history

  val is_empty: history -> bool

  val num_sol: history -> int

  val write_stat: string -> history -> unit

  val save_gr: Domain.t -> string -> history -> unit

  val save_conll: Domain.t -> string -> history -> unit

  (** [save_full_conll base_name rh] saves one conll_file for each normal form defined in [rh].
      Output files are named according to [base_name] and a secondary index after "__".
      The number of conll file produced is returned. *)
  val save_full_conll: Domain.t -> string -> history -> int

  val save_det_gr: Domain.t -> string -> history -> unit

  val save_det_conll: Domain.t -> ?header:string -> string -> history -> unit

  val det_dep_string: Domain.t -> history -> string option

  val conll_dep_string: Domain.t -> ?keep_empty_rh:bool -> history -> string option

  val save_index: dirname:string -> base_names: string list -> unit

  val write_annot: Domain.t -> title:string -> string -> string -> (string * history) list -> unit

  val write_html: Domain.t -> ?no_init: bool -> ?out_gr: bool -> ?filter: string list -> ?main_feat: string -> ?dot: bool -> header: string -> ?graph_file: string -> history -> string ->  unit

  val error_html: Domain.t -> ?no_init:bool -> ?main_feat:string -> ?dot: bool -> header: string -> string -> ?init:Graph.t -> string -> unit

  val make_index: title: string -> grs_file: string -> html: bool -> grs: Grs.t -> seq: string -> input_dir: string -> output_dir: string -> base_names: string list -> unit

  val html_sentences: title:string -> string -> (bool * string * int * string) list -> unit
end

