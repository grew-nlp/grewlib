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
  (* simple_ident: V.cat *)
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

  (* -------------------------------------------------------------------------------- *)
  (* command_node_ident: V, V#alpha, V.cat, V#alpha.cat, p_obj.loc *)
  type command_node_ident =
    | No_sharp of string
    | Sharp of string * string
  val parse_command_node_ident: string -> command_node_ident
  val dump_command_node_ident: command_node_ident -> string

  val base_command_node_ident: command_node_ident -> string

  (* ---------------------------------------------------------------------- *)
  (* command_feature_ident: V.cat, V#alpha.cat *)
  type command_feature_ident = command_node_ident * feature_name
  val parse_command_feature_ident: string -> command_feature_ident
  val dump_command_feature_ident: command_feature_ident -> string

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

  type u_edge = {
      edge_id: Id.name option;
      src: Id.name;
      edge_labels: edge_label list;
      tar: Id.name;
      negative: bool;
    }
  type edge = u_edge * Loc.t

  type ineq = Lt | Gt | Le | Ge
  val string_of_ineq: ineq -> string

  type u_const =
    | Start of Id.name * edge_label list (* (source, labels) *)
    | Cst_out of Id.name
    | End of Id.name * edge_label list (* (target, labels) *)
    | Cst_in of Id.name
    | Feature_eq of feature_ident * feature_ident
    | Feature_diseq of feature_ident * feature_ident
    | Feature_ineq of ineq * feature_ident * feature_ident
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

  type concat_item =
    | Qfn_item of feature_ident
    | String_item of string
    | Param_item of string

  type u_command =
    | Del_edge_expl of (command_node_ident * command_node_ident * edge_label)
    | Del_edge_name of string
    | Add_edge of (command_node_ident * command_node_ident * edge_label)

    (* 4 args: source, target, labels, flag true iff negative cst *)
    | Shift_in of (command_node_ident * command_node_ident * edge_label list * bool)
    | Shift_out of (command_node_ident * command_node_ident * edge_label list * bool)
    | Shift_edge of (command_node_ident * command_node_ident * edge_label list * bool)

    | Merge_node of (command_node_ident * command_node_ident)
    | New_neighbour of (Id.name * command_node_ident * edge_label)
    | Del_node of command_node_ident
    | Activate of command_node_ident

    | Del_feat of command_feature_ident
    | Update_feat of command_feature_ident * concat_item list
  type command = u_command * Loc.t

  type rule = {
      rule_id:Id.name;
      pos_basic: basic;
      neg_basics: basic list;
      commands: command list;
      param: (string list * string list) option; (* (files, vars) *)
      lex_par: string list option; (* lexical parameters in the file *)
      rule_doc:string list;
      rule_loc: Loc.t;
    }

  type modul = {
      module_id:Id.name;
      local_labels: (string * string list) list;
      suffixes: string list;
      rules: rule list;
      confluent: bool;
      module_doc:string list;
      mod_loc:Loc.t;
      mod_dir: string; (* the directory where the module is defined (for lp file localisation) *)
    }

  type sequence = {
      seq_name:string;
      seq_mod:string list;
      seq_doc:string list;
      seq_loc:Loc.t;
    }

  type module_or_include =
    | Modul of modul
    | Includ of (string * Loc.t)

  type grs_with_include = {
      domain_wi: Domain.t;
      labels_wi: (string * string list) list;    (* the list of global edge labels *)
      modules_wi: module_or_include list;
      sequences_wi: sequence list;
    }

  (* a GRS: graph rewriting system *)
  type grs = {
      domain: Domain.t;
      labels: (string * string list) list;
      modules: modul list;
      sequences: sequence list;
    }

  type gr = {
    meta: (string * string) list;
    nodes: node list;
    edges: edge list;
  }

  val empty_grs: grs
end (* module Ast *)
