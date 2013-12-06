open Grew_utils
open Grew_graph
open Grew_rule
open Grew_ast

module Rewrite_history: sig
  type t = {
      instance: Instance.t;
      module_name: string; 
      good_nf: t list;
      bad_nf: Instance.t list;
    }

  val is_empty: t -> bool

  val num_sol: t -> int
    
  (** [save_nfs ?main_feat base_name t] does two things:
      - write PNG files of normal forms
      - returns a list of couples (rules, file)
  *)
  val save_nfs:
    ?filter: string list -> 
    ?main_feat: string -> 
    dot: bool ->
    string -> 
    t -> 
    ((string * string list) list * string) list

  (** [save_annot out_dir base_name t] writes a set of svg_file for an annotation folder. *)
  val save_annot: string -> string -> t -> (string * int * (string*string*float) * (string*string*float) * (float option * float option)) list

  (** [save_gr base_name t] saves one gr_file for each normal form defined in [t].
      Output files are named according to [base_name] and the Gorn adress in the rewriting tree. *)
  val save_gr: string -> t -> unit
  val save_conll: string -> t -> unit

  (** [save_det_gr base_name t] supposes that the current GRS is deterministic.
      It writes exactly one output file named [base_name].gr with the unique normal form. *)
  val save_det_gr: string -> t -> unit
  val save_det_conll: ?header:string -> string -> t -> unit

  val det_dep_string: t -> string option

  val conll_dep_string: ?keep_empty_rh:bool -> t -> string option
end

module Modul: sig
  type t = {
    name: string;
    local_labels: (string * string list) array;
    rules: Rule.t list;
    filters: Rule.t list;
    confluent: bool;
    loc: Loc.t;
  }
end



module Grs: sig
  type t

  val empty:t
  
  val get_modules: t -> Modul.t list

  val get_ast: t -> Ast.grs

  val get_filename: t -> string

  val sequence_names: t -> string list

  (** [build filename] returns the GRS defined in the file [filename] *)
  val build: string -> t

  val rewrite: t -> string -> Instance.t -> Rewrite_history.t

  (* only externeal strucutre is returned, each edge contains a "dummy" big_step *)
  val build_rew_display: t -> string -> Instance.t -> Grew_types.rew_display

  val rule_iter: (string -> Rule.t -> unit) -> t -> unit
  val filter_iter: (string -> Rule.t -> unit) -> t -> unit

  val modules_of_sequence: t -> string -> Modul.t list
end
