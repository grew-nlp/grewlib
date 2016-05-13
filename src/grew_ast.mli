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
  type feature = u_feature * Loc.t

  type u_node = {
      node_id: Id.name;
      position: float option;
      fs: feature list;
    }
  type node = u_node * Loc.t

  type edge_label = string (* p_obj.agt:suj *)

  (* (list of edge_label separated by '|', bool true iff it is a negative constraint) *)
  type edge_label_cst = edge_label list * bool

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
    | Feature_eq of feature_ident * feature_ident
    | Feature_diseq of feature_ident * feature_ident
    | Feature_ineq of ineq * feature_ident * feature_ident
    | Feature_ineq_cst of ineq * feature_ident * float
    | Feature_re of feature_ident * string
    | Prec of Id.name * Id.name
    | Lprec of Id.name * Id.name

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

  val complete_pattern : pattern -> pattern

  type concat_item =
    | Qfn_item of feature_ident
    | String_item of string
    | Param_item of string

  type u_command =
    | Del_edge_expl of (Id.name * Id.name * edge_label)
    | Del_edge_name of string
    | Add_edge of (Id.name * Id.name * edge_label)

    (* 4 args: source, target, labels, flag true iff negative cst *)
    | Shift_in of (Id.name * Id.name * edge_label_cst)
    | Shift_out of (Id.name * Id.name * edge_label_cst)
    | Shift_edge of (Id.name * Id.name * edge_label_cst)

    | Merge_node of (Id.name * Id.name)
    | New_neighbour of (Id.name * Id.name * edge_label)

    | New_node of Id.name
    | New_before of (Id.name * Id.name)
    | New_after of (Id.name * Id.name)

    | Del_node of Id.name

    | Del_feat of feature_ident
    | Update_feat of feature_ident * concat_item list
  val string_of_u_command:  u_command -> string
  type command = u_command * Loc.t



  val replace_new_neighbour: command list -> command list

  type rule = {
      rule_id:Id.name;
      pattern: pattern;
      commands: command list;
      param: (string list * string list) option; (* (files, vars) *)
      lex_par: string list option; (* lexical parameters in the file *)
      rule_doc:string list;
      rule_loc: Loc.t;
    }

  type modul = {
      module_id:Id.name;
      local_labels: (string * string list) list;
      rules: rule list;
      confluent: bool;
      module_doc:string list;
      mod_loc:Loc.t;
      mod_dir: string; (* the directory where the module is defined (for lp file localisation) *)
    }

  type old_sequence = {
    seq_name:string;
    seq_mod:string list;
    seq_doc:string list;
    seq_loc:Loc.t;
  }

  type new_sequence =
    | Ref of string
    | List of new_sequence list
    | Plus of new_sequence list
    | Star of new_sequence
    | Diamond of new_sequence

  val new_sequence_to_string : new_sequence -> string
  val flatten : new_sequence -> new_sequence

  type sequence =
  | Old of old_sequence
  | New of ((string * Loc.t) * new_sequence)

  type module_or_include =
    | Modul of modul
    | Includ of (string * Loc.t)

  type domain = {
      feature_domain: Feature_domain.feature_spec list;
      label_domain: (string * string list) list;
    }

  type domain_wi = Dom of domain | Dom_file of string

  type grs_wi = {
      domain_wi: domain_wi;
      modules_wi: module_or_include list;
      sequences_wi: sequence list;
    }

  (* a GRS: graph rewriting system *)
  type grs = {
      domain: domain;
      modules: modul list;
      sequences: sequence list;
    }

  type gr = {
    meta: string list;
    nodes: node list;
    edges: edge list;
  }

  val empty_grs: grs
end (* module Ast *)
