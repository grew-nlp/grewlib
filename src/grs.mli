open Utils
open Graph
open Rule



module Rewrite_history: sig
  type t = {
      instance: Instance.t;
      module_name: string; 
      good_nf: t list;
      bad_nf: Instance.t list;
    }

  val is_empty: t -> bool

IFDEF DEP2PICT THEN
  val error_html: ?main_feat:string -> ?init_graph:bool -> ?header:string -> string -> string -> Instance.t option -> unit

  val save_html: ?main_feat:string -> ?init_graph:bool -> ?header:string -> string -> t -> unit
ENDIF
end


module Grs: sig
  type sequence = string * string list
  type t

  val empty:t
  
  val sequences: t -> sequence list

  val build: Ast.grs -> t

  val rewrite: t -> string -> Instance.t -> Rewrite_history.t

  (* only externeal strucutre is returned, each edge contains a "dummy" big_step *)
  val build_rew_display: t -> string -> Instance.t -> Grew_types.rew_display
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
    html:bool -> (* if [html] put hlinks on files *) 
    output_dir:string -> 
      t -> unit
end
