(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2018 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: http://grew.fr                                                     *)
(*    License: CeCILL (see LICENSE folder or "http://www.cecill.info")            *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open Grew_base

type feature_name = string (* cat, num, ... *)
type feature_atom = string (* V, N, inf, ... *)
type feature_value = string (* V, 4, "free text", ... *)

type value =
  | String of string
  | Float of float

val string_of_value : value -> string

val conll_string_of_value : value -> string

type disjunction = value list



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
  type pointed = string * string
  type simple_or_pointed =
    | Simple of Id.name
    | Pointed of pointed

  val parse_simple_or_pointed: string -> simple_or_pointed

  (* ---------------------------------------------------------------------- *)
  type feature_kind =
    | Equality of feature_value list
    | Disequality of feature_value list
    | Equal_lex of string * string
    | Disequal_lex of string * string
    | Absent
    | Else of (feature_value * feature_name * feature_value)

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

  type atom_edge_label_cst =
    | Atom_eq of string * string list      (* 1=subj|obj  *)
    | Atom_diseq of string * string list   (* 1<>subj|obj *)
    | Atom_absent of string                (* !2          *)

  type edge_label = string

  type edge_label_cst =
    | Pos_list of edge_label list           (*  X|Y|Z    *)
    | Neg_list of edge_label list           (*  ^X|Y|Z   *)
    | Regexp of string                      (*  re"a.*"  *)
    | Atom_list of atom_edge_label_cst list (* 1=subj, 2 *)

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
    (* ambiguous case, context needed to make difference "N.cat = M.cat" VS "N.cat = lex.cat" *)
    | Feature_eq_lex_or_fs of feature_ident * (string * string)
    | Feature_diff_lex_or_fs of feature_ident * (string * string)
    (* *)
    | Feature_eq_regexp of feature_ident * string
    | Feature_eq_cst of feature_ident * string
    | Feature_eq_lex of feature_ident * (string * string)
    | Feature_diff_cst of feature_ident * string
    | Feature_diff_lex of feature_ident * (string * string)
    (* *)
    | Immediate_prec of Id.name * Id.name
    | Large_prec of Id.name * Id.name
    (* *)
    | Id_prec of Id.name * Id.name
    (* *)
    | Label_equal of Id.name * Id.name
    | Label_disequal of Id.name * Id.name

  type const = u_const * Loc.t

  type basic = {
    pat_nodes: node list;
    pat_edges: edge list;
    pat_const: const list;
  }

  val empty_basic: basic

  val concat_basic: basic -> basic -> basic

  type u_glob =
    | Glob_cst of string
    | Glob_eq_list of string * string list
    | Glob_diff_list of string * string list
    | Glob_absent of string
    | Glob_regexp of string * string

  type glob = u_glob * Loc.t

  type pattern = {
    pat_glob: glob list;
    pat_pos: basic;
    pat_negs: basic list;
  }

  (* [check for duplicate edge identifier in pos part and
     remove edge identifier in neg part] *)
  val normalize_pattern : pattern -> pattern

  val complete_pattern : pattern -> pattern

  type concat_item =
    | Qfn_or_lex_item of (string * string)
    | String_item of string

  type u_command =
    | Del_edge_expl of (Id.name * Id.name * edge_label)
    | Del_edge_name of string
    | Add_edge of (Id.name * Id.name * edge_label)
    | Add_edge_expl of (Id.name * Id.name * string)
    | Add_edge_items of (Id.name * Id.name * (string * string) list)

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

    | Append_feats of (Id.name * Id.name * string * string)
    | Unorder of Id.name

  val string_of_u_command:  u_command -> string
  type command = u_command * Loc.t

  type lexicon =
    | File of string
    | Final of (int * string) list

  type lexicon_info = (string * lexicon) list

  type rule = {
    rule_id:Id.name;
    pattern: pattern;
    commands: command list;
    lexicon_info: lexicon_info;
    rule_doc:string list;
    rule_loc: Loc.t;
    rule_dir: string option; (* the real folder where the file is defined *)
    rule_path: string;
  }

  type label_spec = string * string list

  type feature_spec =
    | Closed of feature_name * feature_atom list (* cat:V,N *)
    | Open of feature_name (* phon, lemma, ... *)
    | Num of feature_name (* position *)

  val build_closed: feature_name -> feature_atom list -> feature_spec

  type domain = {
    feature_domain: feature_spec list;
    label_domain: label_spec list;
  }

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

  type strat =
    | Ref of node_ident       (* reference to a rule name or to another strategy *)
    | Pick of strat               (* pick one normal form a the given strategy; return 0 if nf *)
    | Alt of strat list           (* a set of strategies to apply in parallel *)
    | Seq of strat list           (* a sequence of strategies to apply one after the other *)
    | Iter of strat               (* a strategy to apply iteratively *)
    | Onf of strat                (* deterministic computation of One Normal Form *)
    | If of strat * strat * strat (* choose a stragegy with a test *)
    | Try of strat                (* ≜ If (S, S, Empty): pick one normal form a the given strategy; return input if nf *)

  type decl =
    | Conll_fields of string list
    | Features of feature_spec list
    | Labels of (string * string list) list
    | Package of (Loc.t * simple_ident * decl list)
    | Rule of rule
    | Strategy of (Loc.t * simple_ident * strat)
    | Import of string
    | Include of string

  type grs = decl list

  val strat_to_json: strat -> Yojson.Basic.t

  val strat_list: grs -> string list

end (* module Ast *)
