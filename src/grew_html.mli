(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2013 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: http://grew.loria.fr                                               *)
(*    License: CeCILL (see LICENSE folder or "http://www.cecill.info")            *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open Grew_types
open Grew_domain
open Grew_rule
open Grew_grs
open Grew_graph

(* ================================================================================ *)
module Html_doc : sig
  (* dep is a flag which is true iff dep file are shown in doc (iff dep2pict is available) *)
  val build: dep:bool -> corpus:bool -> string -> Grs.t -> unit
end (* module Html_doc *)

(* ================================================================================ *)
module Html_sentences : sig
  val build: title:string -> string -> (bool * string * int * string) list -> unit
end (* module Html_sentences *)

(* ================================================================================ *)
module Html_rh: sig

  val build:
    ?domain:Domain.t ->
    ?filter: string list ->
    ?main_feat: string ->
    ?dot: bool ->
    ?init_graph:bool ->
    ?out_gr:bool ->
    ?header:string ->
    ?graph_file:string ->
    string ->
    Rewrite_history.t ->
    unit

  val error:
    ?domain: Domain.t ->
    ?main_feat: string ->
    ?dot: bool ->
    ?init_graph:bool ->
    ?header:string ->
    string ->
    string ->
    G_graph.t option ->
    unit
end (* module Html_rh *)

(* ================================================================================ *)
module Gr_stat: sig
  type t

  val from_rew_history: Rewrite_history.t -> t

  val save: string -> t -> unit

  val load: string -> t
end (* module Gr_stat *)

(* ================================================================================ *)
module Corpus_stat: sig
  type t

  val empty: grs:Grs.t -> seq:string -> t

  val add_gr_stat: string -> Gr_stat.t -> t -> t

  val save_html:
    title: string ->
    grs_file: string ->
    input_dir:string ->
    output_dir:string ->
      t -> unit
end (* module Corpus_stat *)

(* ================================================================================ *)
module Html_annot: sig
  val build: ?domain: Domain.t -> title:string -> string -> string -> (string * Rewrite_history.t) list -> unit
end (* module Html_annot *)
