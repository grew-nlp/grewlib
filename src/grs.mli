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

  val rules: t -> (string * string list) list

  val rules_stat: t -> int StringMap.t

IFDEF DEP2PICT THEN
  val save_html: ?main_feat:string -> ?init_graph:bool -> ?header:string -> string -> int -> t -> (string*string list) list
  
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
