(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2013 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: http://grew.loria.fr                                               *)
(*    License: CeCILL (see LICENSE folder or "http://www.cecill.info")            *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open Printf
open Log
open Grew_base
open Grew_types

(* ================================================================================ *)
module Ast = struct

  (* general function for checking that an identifier is of the right kind *)
  let check_special name allowed s =
    let sp = Str.full_split (Str.regexp "#\\|\\.\\|:\\|\\*") s in
    try
      match List.find
      (function
        | Str.Delim d when not (List.mem d allowed) -> true
        | _ -> false
      ) sp
      with
      | Str.Delim wrong_char ->
       Error.build "The identifier '%s' is not a valid %s, the character '%s' is illegal" s name wrong_char
      | Str.Text _ -> Error.bug "[Grew_ast.check_special]"
    with
    | Not_found -> ()

  (* ---------------------------------------------------------------------- *)
  (* simple_ident: cat *)
  type simple_ident = Id.name
  let parse_simple_ident s = check_special "simple ident" [] s; s
  let is_simple_ident s = try ignore (parse_simple_ident s); true with _ -> false
  let dump_simple_ident name = name

  (* ---------------------------------------------------------------------- *)
  (* label_ident: D:mod.dis *)
  type label_ident = string
  let parse_label_ident s = check_special "label ident" [":"; "."] s; s
  let dump_label_ident name = name

  (* ---------------------------------------------------------------------- *)
  (* pattern_label_ident: D:mod.* *)
  type pattern_label_ident = string
  let parse_pattern_label_ident s = check_special "label ident" [":"; "."; "*"] s; s
  let dump_pattern_label_ident name = name

  (* ---------------------------------------------------------------------- *)
  (* feature_ident: V.cat *)
  type feature_ident = Id.name * feature_name
  let parse_feature_ident s =
    check_special "feature ident" ["."] s;
    match Str.full_split (Str.regexp "\\.") s with
    | [Str.Text base; Str.Delim "."; Str.Text fn] -> (base, fn)
    | _ -> Error.build "The identifier '%s' must be a feature identifier (with exactly one '.' symbol, like \"V.cat\" for instance)" s
  let dump_feature_ident (name, feat_name) = sprintf "%s.%s" name feat_name


  (* ---------------------------------------------------------------------- *)
  (* command_node_id: V, V#alpha *)
  type command_node_ident =
    | No_sharp of Id.name
    | Sharp of Id.name * string

  let parse_command_node_ident s =
    check_special "feature ident" ["#"] s;
    match Str.full_split (Str.regexp "#") s with
    | [Str.Text base; Str.Delim "#"; Str.Text ext] -> Sharp (base, ext)
    | [Str.Text base] -> No_sharp base
    | _ -> Error.build "The identifier '%s' must be a command node identifier (with at most one '#' symbol)" s

  let dump_command_node_ident = function
    | No_sharp x -> x
    | Sharp (x,y) -> x ^ "#" ^ y


  let base_command_node_ident = function
    | No_sharp x -> x
    | Sharp (x,y) -> x

  (* ---------------------------------------------------------------------- *)
  (* command_feature_ident: V.cat, V#alpha.cat *)
  type command_feature_ident = command_node_ident * feature_name

  let parse_command_feature_ident s =
    check_special "feature ident" ["."; "#"] s;
    match Str.full_split (Str.regexp "#\\|\\.") s with
    | [Str.Text base; Str.Delim "#"; Str.Text ext; Str.Delim "."; Str.Text feature_name] -> (Sharp (base, ext), feature_name)
    | [Str.Text base; Str.Delim "."; Str.Text feature_name] -> (No_sharp base, feature_name)
    | _ -> Error.build "The identifier '%s' must be a command feature identifier (with exactly one '.' symbol and at most one '#' symbol in the left part)" s

  let dump_command_feature_ident = function
    | (No_sharp base, feature_name) -> sprintf "%s.%s" base feature_name
    | (Sharp (base,ext), feature_name) -> sprintf "%s#%s.%s" base ext feature_name

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

  type edge_label = string

  type u_edge = {
    edge_id: Id.name option;
    src: Id.name;
    edge_labels: edge_label list;
    tar: Id.name;
    negative: bool;
  }
  type edge = u_edge * Loc.t

  type ineq = Lt | Gt | Le | Ge

  let string_of_ineq = function
    | Lt -> "<"
    | Gt -> ">"
    | Le -> "≤"
    | Ge -> "≥"

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

  type graph = {
    nodes: (Id.name * node) list;
    edge: edge list;
  }

  type concat_item =
    | Qfn_item of feature_ident
    | String_item of string
    | Param_item of string

  type u_command =
    | Del_edge_expl of (command_node_ident * command_node_ident * edge_label)
    | Del_edge_name of string
    | Add_edge of (command_node_ident * command_node_ident * edge_label)
    | Shift_in of (command_node_ident * command_node_ident)
    | Shift_out of (command_node_ident * command_node_ident)
    | Shift_edge of (command_node_ident * command_node_ident)
    | Merge_node of (command_node_ident * command_node_ident)
    | New_neighbour of (Id.name * command_node_ident * edge_label)
    | Del_node of command_node_ident
    | Activate of command_node_ident

    | Del_feat of command_feature_ident
    | Update_feat of command_feature_ident * concat_item list
  type command = u_command * Loc.t

  (* the [rule] type is used for 3 kids of module items:
     - rule     { param=None; ... }
     - lex_rule
     - filter   { param=None; commands=[]; ... }
  *)
  type rule = {
    rule_id:Id.name;
    pos_basic: basic;
    neg_basics: basic list;
    commands: command list;
    param: (string list * string list) option;
    lex_par: string list option;
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

  (** a GRS: graph rewriting system *)
  type module_or_include =
    | Modul of modul
    | Includ of (string * Loc.t)

  type grs_with_include = {
    domain_wi: Domain.t;
    labels_wi: (string * string list) list;    (* the list of global edge labels *)
    modules_wi: module_or_include list;
    sequences_wi: sequence list;
  }

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

  let empty_grs = { domain = []; labels = []; modules = []; sequences= [] }

end (* module Ast *)
