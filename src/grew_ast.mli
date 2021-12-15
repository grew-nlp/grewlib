(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2021 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: https://grew.fr                                                     *)
(*    License: CeCILL (see LICENSE folder or "http://cecill.info/")            *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open Grew_base

type feature_name = string (* upos, Gender, … *)
type string_feature_value = string (* V, 4, "free text", … *)

type feature_value =
  | String of string
  | Float of float

val get_range_feature_value: Range.t -> feature_value -> feature_value

val string_of_value : feature_value -> string

val conll_string_of_value : feature_value -> string

val json_of_value : feature_value -> Yojson.Basic.t


val numeric_feature_values: string list
val typed_vos : feature_name -> string -> feature_value

val concat_feature_values: ?loc:Loc.t -> feature_value list -> feature_value

val parse_meta: string -> string * string
val string_of_meta: string * string -> string

type cmp = Eq | Neq
val string_of_cmp: cmp -> string
val cmp_fct: cmp -> ('a -> 'a -> bool)

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
    | Feat_kind_list of cmp * string_feature_value list
    | Feat_kind_lex of cmp * string * string
    | Feat_kind_re of cmp * string
    | Absent
    | Else of (string_feature_value * feature_name * string_feature_value)

  type u_feature = {
    name: feature_name;
    kind: feature_kind;
  }
  val u_feature_to_string: u_feature -> string

  type feature = u_feature * Loc.t

  val default_fs: ?loc:Loc.t -> string -> feature list

  type u_node = {
    node_id: Id.name;
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
    | Pred

  type u_edge = {
    edge_id: Id.name option;
    src: Id.name;
    edge_label_cst: edge_label_cst;
    tar: Id.name;
  }
  type edge = u_edge * Loc.t

  type ineq = Lt | Gt | Le | Ge
  val check_ineq: float -> ineq -> float -> bool
  val string_of_ineq: ineq -> string

  type u_const =
    | Cst_out of Id.name * edge_label_cst
    | Cst_in of Id.name * edge_label_cst
    | Feature_cmp of cmp * feature_ident * feature_ident
    | Feature_ineq of ineq * feature_ident * feature_ident
    | Feature_ineq_cst of ineq * feature_ident * float
    | Feature_cmp_regexp of cmp * feature_ident * string
    | Feature_cmp_value of cmp * feature_ident * feature_value
    | Large_prec of Id.name * Id.name
    | Edge_disjoint of Id.name * Id.name
    | Edge_crossing of Id.name * Id.name

  type const = u_const * Loc.t

  type basic = {
    pat_nodes: node list;
    pat_edges: edge list;
    pat_const: const list;
  }

  val empty_basic: basic

  val concat_basic: basic -> basic -> basic

  val complete_basic: basic -> basic

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

  (* [complete with implicit nodes and check for duplicate edge identifier in pos part] *)
  val complete_and_check_pattern : pattern -> pattern

  type concat_item =
    | Qfn_or_lex_item of (pointed * Range.t)
    | String_item of (string * Range.t)

  type side = Prepend | Append

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

    | Concat_feats of (side * Id.name * Id.name * string * string)
    | Unorder of Id.name

    | Insert_before of (Id.name * Id.name)
    | Insert_after of (Id.name * Id.name)

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
    | Closed of feature_name * string list (* cat:V,N *)
    | Open of feature_name (* phon, lemma, ... *)
    | Num of feature_name (* position *)

  val build_closed: feature_name -> string list -> feature_spec

  type gr = {
    meta: (string * string) list;
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

  (* return the list of strategies declared at the top level *)
  val strat_list: grs -> string list

  (* return the list of strategies declared at the top level *)
  val strat_lists: grs -> (string list * string list)  (* (full_list, not_used_as_substrat) *)

  (* return the list of packages declared at the top level *)
  val package_list: grs -> string list

  (* return the list of rules declared at the top level *)
  val rule_list: grs -> string list

end (* module Ast *)
