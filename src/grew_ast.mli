(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2013 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: http://grew.loria.fr                                               *)
(*    License: CeCILL (see LICENSE folder or "http://www.cecill.info")            *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open Grew_base
open Grew_types

module Ast : sig

  val to_uname: feature_name -> feature_name

  (* ---------------------------------------------------------------------- *)
  (* simple_ident: cat or V *)
  type simple_ident = Id.name
  val parse_simple_ident: string -> simple_ident
  val is_simple_ident: string -> bool
  val dump_simple_ident: simple_ident -> string

  (* ---------------------------------------------------------------------- *)
  (* label_ident: D:mod.dis *)
  type label_ident = string
  val parse_label_ident: string -> label_ident
  val dump_label_ident: label_ident -> string

  (* ---------------------------------------------------------------------- *)
  (* pattern_label_ident: D:mod.* *)
  type pattern_label_ident = string
  val parse_pattern_label_ident: string -> pattern_label_ident
  val dump_pattern_label_ident: pattern_label_ident -> string

  (* ---------------------------------------------------------------------- *)
  (* node_ident: W0.5 *)
  type node_ident = string
  val parse_node_ident: string -> node_ident
  val dump_node_ident: node_ident -> string

  (* ---------------------------------------------------------------------- *)
  (* feature_ident: V.cat *)
  type feature_ident = Id.name * feature_name
  val parse_feature_ident: string -> feature_ident
  val dump_feature_ident: feature_ident -> string

  (* ---------------------------------------------------------------------- *)
  (* simple_or_feature_ident: union of simple_ident and feature_ident *)
  (* Note: used for parsing of "X < Y" and "X.feat < Y.feat" without conflicts *)
  type simple_or_feature_ident = Id.name * feature_name option
  val parse_simple_or_feature_ident: string -> simple_or_feature_ident

  (* ---------------------------------------------------------------------- *)
  type feature_kind =
    | Equality of feature_value list
    | Disequality of feature_value list
    | Equal_param of string (* $ident *)
    | Absent

  type u_feature = {
    name: feature_name;
    kind: feature_kind;
  }
  val u_feature_to_string: u_feature -> string

  type feature = u_feature * Loc.t

  val default_fs: ?loc:Loc.t -> string -> feature list

  type u_node = {
      node_id: Id.name;
      position: float option;
      fs: feature list;
    }
  type node = u_node * Loc.t

  val grewpy_compare: node -> node -> int

  type edge_label = string (* p_obj.agt:suj *)

  type edge_label_cst =
    | Pos_list of edge_label list (*  X|Y|Z    *)
    | Neg_list of edge_label list (*  ^X|Y|Z   *)
    | Regexp of string            (*  re"a.*"  *)

  type u_edge = {
      edge_id: Id.name option;
      src: Id.name;
      edge_label_cst: edge_label_cst;
      tar: Id.name;
    }
  type edge = u_edge * Loc.t

  type ineq = Lt | Gt | Le | Ge
  val string_of_ineq: ineq -> string

  type u_const =
    | Cst_out of Id.name * edge_label_cst
    | Cst_in of Id.name * edge_label_cst
    | Features_eq of feature_ident * feature_ident
    | Features_diseq of feature_ident * feature_ident
    | Features_ineq of ineq * feature_ident * feature_ident
    | Feature_ineq_cst of ineq * feature_ident * float
    | Feature_eq_float of feature_ident * float
    | Feature_diff_float of feature_ident * float

    | Feature_eq_regexp of feature_ident * string
    | Feature_eq_cst of feature_ident * string
    | Feature_diff_cst of feature_ident * string

    | Immediate_prec of Id.name * Id.name
    | Large_prec of Id.name * Id.name
  type const = u_const * Loc.t

  type basic = {
      pat_nodes: node list;
      pat_edges: edge list;
      pat_const: const list;
    }

  type pattern = {
      pat_pos: basic;
      pat_negs: basic list;
    }

  (* [check for duplicate edge identifier in pos part and
     remove edge identifier in neg part] *)
  val normalize_pattern : pattern -> pattern

  val complete_pattern : pattern -> pattern

  type concat_item =
    | Qfn_item of feature_ident
    | String_item of string
    | Param_item of string

  type u_command =
    | Del_edge_expl of (Id.name * Id.name * edge_label)
    | Del_edge_name of string
    | Add_edge of (Id.name * Id.name * edge_label)
    | Add_edge_expl of (Id.name * Id.name * string)

    (* 4 args: source, target, labels, flag true iff negative cst *)
    | Shift_in of (Id.name * Id.name * edge_label_cst)
    | Shift_out of (Id.name * Id.name * edge_label_cst)
    | Shift_edge of (Id.name * Id.name * edge_label_cst)

    | New_node of Id.name
    | New_before of (Id.name * Id.name)
    | New_after of (Id.name * Id.name)

    | Del_node of Id.name

    | Del_feat of feature_ident
    | Update_feat of feature_ident * concat_item list
  val string_of_u_command:  u_command -> string
  type command = u_command * Loc.t

  type rule = {
      rule_id:Id.name;
      pattern: pattern;
      commands: command list;
      param: (string list * string list) option; (* (files, vars) *)
      lex_par: string list option; (* lexical parameters in the file *)
      rule_doc:string list;
      rule_loc: Loc.t;
      rule_dir: string option; (* the real folder where the file is defined *)
    }

  type modul = {
      module_id:Id.name;
      rules: rule list;
      deterministic: bool;
      module_doc:string list;
      mod_loc:Loc.t;
      mod_dir: string; (* the directory where the module is defined (for lp file localisation) *)
    }

  type module_or_include =
    | Modul of modul
    | Includ of (string * Loc.t)

  type feature_spec =
    | Closed of feature_name * feature_atom list (* cat:V,N *)
    | Open of feature_name (* phon, lemma, ... *)
    | Num of feature_name (* position *)

  val build_closed: feature_name -> feature_atom list -> feature_spec

  type domain = {
      feature_domain: feature_spec list;
      label_domain: (string * string list) list;
    }

  type domain_wi = Dom of domain | Dom_file of string

  type strat_def = (* /!\ The list must not be empty in the Seq or Plus constructor *)
    | Ref of string            (* reference to a module name or to another strategy *)
    | Seq of strat_def list    (* a sequence of strategies to apply one after the other *)
    | Star of strat_def        (* a strategy to apply iteratively *)
    | Pick of strat_def        (* pick one normal form a the given strategy; return 0 if nf *)
    | Sequence of string list  (* compatibility mode with old code *)

  val strat_def_to_string: strat_def -> string

  val strat_def_flatten: strat_def -> strat_def

  (* a strategy is given by its descrition in the grs file and the 4 fields: *)
  type strategy = {
    strat_name:string;       (* a unique name of the stratgy *)
    strat_def:strat_def;     (* the definition itself *)
    strat_doc:string list;   (* lines of docs (if any in the GRS file) *)
    strat_loc:Loc.t;         (* the location of the [name] of the strategy *)
  }

  type grs_wi = {
      domain_wi: domain_wi option;
      modules_wi: module_or_include list;
      strategies_wi: strategy list;
    }

  (* a GRS: graph rewriting system *)
  type grs = {
      domain: domain option;
      modules: modul list;
      strategies: strategy list;
    }
  val empty_grs: grs

  type gr = {
    meta: string list;
    nodes: node list;
    edges: edge list;
  }
  val complete_graph: gr -> gr

  (* phrase structure tree *)
  type pst =
  | Leaf of (Loc.t * string) (* phon *)
  | T of (Loc.t * string * pst list)
  val word_list: pst -> string list
end (* module Ast *)


(* ================================================================================================ *)
module New_ast : sig
  type strat =
  | Ref of Ast.node_ident       (* reference to a rule name or to another strategy *)
  | Pick of strat               (* pick one normal form a the given strategy; return 0 if nf *)
  | Alt of strat list           (* a set of strategies to apply in parallel *)
  | Seq of strat list           (* a sequence of strategies to apply one after the other *)
  | Iter of strat               (* a strategy to apply iteratively *)
  | Onf of strat                (* deterministic computation of One Normal Form *)
  | If of strat * strat * strat (* choose a stragegy with a test *)
  | Try of strat                (* ≜ If (S, S, Empty): pick one normal form a the given strategy; return input if nf *)

  type decl =
  | Conll_fields of string list
  | Features of Ast.feature_spec list
  | Labels of (string * string list) list
  | Package of (Loc.t * Ast.simple_ident * decl list)
  | Rule of Ast.rule
  | Strategy of (Loc.t * Ast.simple_ident * strat)
  | Import of string
  | Include of string

  type grs = decl list

  val strat_to_json: strat -> Yojson.Basic.json

  val strat_list: grs -> string list

  val convert: Ast.grs -> grs
end (* module New_ast *)
