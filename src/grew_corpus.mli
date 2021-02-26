open Conllx

open Grew_base
open Grew_graph


module Corpus : sig
  type t

  val merge: t list -> t

  val size: t -> int
  val get_graph: int -> t -> G_graph.t
  val is_conll: int -> t -> bool
  val get_sent_id: int -> t -> string
  val get_text: int -> t -> string

  val fold_left: ('a -> string -> G_graph.t -> 'a) -> 'a -> t -> 'a
  val iteri: (int -> string -> G_graph.t -> unit) -> t -> unit

  val permut_length: t -> int array

  val from_stdin: ?log_file: string -> ?config:Conllx_config.t -> unit -> t
  val from_file: ?log_file: string -> ?config:Conllx_config.t -> string -> t
  val from_dir: ?log_file: string -> ?config:Conllx_config.t -> string -> t
end

module Corpus_desc : sig

  type t

  val build_corpus: t -> Corpus.t
  val load_corpus_opt: t -> Corpus.t option

  val get_config: t -> Conllx_config.t

  val is_rtl: t -> bool
  val is_audio: t -> bool
  val get_id: t -> string
  val get_directory: t -> string
  val load_json: string -> t list

  val compile: ?force:bool -> ?grew_match: string ->  t -> unit

  val clean: t -> unit
end
