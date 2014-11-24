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
  (* -------------------------------------------------------------------------------- *)
  (* complex_id: V, V#alpha, V.cat, V#alpha.cat, p_obj.loc *)
  type complex_id =
    | No_sharp of string
    | Sharp of string * string
  val complex_id_to_string: complex_id -> string

  (* simple_id: V *)
  type simple_id = Id.name
  val simple_id_of_ci: complex_id -> string
  val is_simple: complex_id -> bool

  (* label_id: V *)
  type label_id = Id.name
  val label_id_of_ci: complex_id -> string

  (* act_id: V, V#alpha *)
  type act_id = Id.name * string option
  val act_id_of_ci: complex_id -> act_id
  val act_id_to_string: act_id -> string

  (* simple_qfn: V.cat *)
  type simple_qfn = Id.name * feature_name
  val simple_qfn_of_ci: complex_id -> simple_qfn
  val simple_qfn_to_string: simple_qfn -> string

  (* act_id: V.cat, V#alpha.cat *)
  type act_qfn = act_id * feature_name
  val act_qfn_of_ci: complex_id -> act_qfn

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
    | Feature_eq of simple_qfn * simple_qfn
    | Feature_diseq of simple_qfn * simple_qfn
    | Feature_ineq of ineq * simple_qfn * simple_qfn
  type const = u_const * Loc.t

  type pattern = {
      pat_nodes: node list;
      pat_edges: edge list;
      pat_const: const list;
    }

  type concat_item =
    | Qfn_item of complex_id
    | String_item of string
    | Param_item of string

  type u_command =
    | Del_edge_expl of (act_id * act_id * edge_label)
    | Del_edge_name of string
    | Add_edge of (act_id * act_id * edge_label)
    | Shift_in of (act_id * act_id)
    | Shift_out of (act_id * act_id)
    | Shift_edge of (act_id * act_id)
    | Merge_node of (act_id * act_id)
    | New_neighbour of (Id.name * act_id * edge_label)
    | Del_node of act_id
    | Activate of act_id

    | Del_feat of act_qfn
    | Update_feat of act_qfn * concat_item list
  type command = u_command * Loc.t

  type rule = {
      rule_id:Id.name;
      pos_pattern: pattern;
      neg_patterns: pattern list;
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
