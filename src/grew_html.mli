
open Grew_ast
open Grew_rule
open Grew_grs

module Html_doc : sig
  val build: dep:bool -> string -> string -> Ast.grs -> unit
end

module Html_sentences : sig
  val build: string -> (string option * int * string) list -> unit
end


module Html_rh: sig

  val build:
    ?main_feat: string ->
    ?dot: bool ->
    ?init_graph:bool ->
    ?out_gr:bool ->
    ?header:string ->
    graph_file:string ->
    string ->
    Rewrite_history.t ->
    unit

  val error:
    ?main_feat: string ->
    ?dot: bool ->
    ?init_graph:bool ->
    ?header:string ->
    string ->
    string ->
    Instance.t option ->
    unit
end

module Gr_stat: sig
  type t

  val from_rew_history: Rewrite_history.t -> t

  val save: string -> t -> unit

  val load: string -> t
end

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
end
