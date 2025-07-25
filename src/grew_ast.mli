(**********************************************************************************)
(*    grewlib • a Graph Rewriting library dedicated to NLP applications           *)
(*                                                                                *)
(*    Copyright 2011-2025 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: https://grew.fr                                                    *)
(*    License: CeCILL (see LICENSE folder or "http://cecill.info/")               *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open Grew_utils

(* ================================================================================ *)
module Regexp : sig
  type t

  val all: t
  val re: string -> t
  val pcre: string -> t
  val pcri: string -> t
  val to_string: t -> string

  val re_match: t -> string -> bool
end

(* ================================================================================ *)
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
  type feature_ident = Id.name * string
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

  type key_ident = Id.name * string list
  val parse_key_ident: string -> key_ident

  (* ---------------------------------------------------------------------- *)
  type feature_kind =
    | Feat_kind_list of Cmp.t * string list
    | Feat_kind_lex of Cmp.t * string * string
    | Feat_kind_re of Cmp.t * Regexp.t
    | Absent
    | Else of (string * string * string)

  type u_feature = {
    name: string;
    kind: feature_kind;
  }
  val u_feature_to_string: u_feature -> string

  type feature = u_feature * Loc.t

  val default_fs: ?loc:Loc.t -> string -> feature list

  type fs = feature list

  type u_node = {
    node_id: Id.name;
    fs_disj: fs list;
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
    | Regexp of Regexp.t                    (*  re"a.*"  *)
    | Atom_list of atom_edge_label_cst list (* 1=subj, 2 *)
    | Pred

  type u_edge = {
    edge_id: Id.name option;
    src: Id.name;
    edge_label_cst: edge_label_cst;
    tar: Id.name;
  }
  type edge = u_edge * Loc.t

  type ineq = Eq | Neq| Lt | Gt | Le | Ge
  val check_ineq: 'a -> ineq -> 'a -> bool
  val string_of_ineq: ineq -> string

  type u_const =
    | Cst_out of Id.name * edge_label_cst
    | Cst_in of Id.name * edge_label_cst
    | Feature_cmp of Cmp.t * feature_ident * feature_ident
    | Feature_ineq of ineq * feature_ident * feature_ident
    | Feature_ineq_cst of ineq * feature_ident * float
    | Feature_cmp_regexp of Cmp.t * feature_ident * Regexp.t
    | Feature_cmp_value of Cmp.t * feature_ident * Feature_value.t
    | Feature_else of feature_ident * string * Feature_value.t  (* N.ExtPos/upos = NOUN ==> Else ((N,ExtPos), upos, NOUN)  *)
    | Feature_absent of feature_ident
    | Large_prec of Id.name * Id.name
    | Large_dom of Id.name * Id.name
    | Edge_disjoint of Id.name * Id.name
    | Edge_crossing of Id.name * Id.name
    | Delta of Id.name * Id.name * ineq * int
    | Length of Id.name * Id.name * ineq * int


  type const = u_const * Loc.t

  type basic = {
    req_nodes: node list;
    req_edges: edge list;
    req_const: const list;
  }

  val empty_basic: basic

  val concat_basic: basic -> basic -> basic

  val complete_basic: basic -> basic

  type u_glob =
    | Glob_cst of string
    | Glob_eq_list of string * string list
    | Glob_diff_list of string * string list
    | Glob_absent of string
    | Glob_regexp of string * Regexp.t

  type glob = u_glob * Loc.t

  val glob_to_string: glob -> string

  type request = {
    req_glob: glob list;
    req_pos: basic;
    req_exts: (basic * bool) list;
  }

  (* [complete with implicit nodes and check for duplicate edge identifier in pos part] *)
  val complete_and_check_request : request -> request

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

    | Concat_feats of (side * Id.name * Id.name * Regexp.t * string)
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
    request: request;
    commands: command list;
    lexicon_info: lexicon_info;
    rule_doc:string list;
    rule_loc: Loc.t;
    rule_dir: string option; (* the real directory where the file is defined *)
    rule_path: string;
  }

  type label_spec = string * string list

  type feature_spec =
    | Closed of string * string list (* cat:V,N *)
    | Open of string (* phon, lemma, ... *)
    | Num of string (* position *)

  val build_closed: string -> string list -> feature_spec

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

  val strat_to_string: strat -> string

  (* return the list of strategies declared at the top level *)
  val strat_list: grs -> string list

  (* return the list of strategies declared at the top level *)
  val strat_lists: grs -> (string list * string list)  (* (full_list, not_used_as_substrat) *)

  (* return the list of packages declared at the top level *)
  val package_list: grs -> string list

  (* return the list of rules declared at the top level *)
  val rule_list: grs -> string list

  type key =
    (* global.text *)
    | Meta of string
    (* S#V#O *)
    | Rel_order of string list
    (* X <-> Y *)
    | Sym_rel of (string * string)
    (* X -> Y *)
    | Rel of (string * string)
    (* X.Number *)
    | Feat of (string * string list)
    (* X.MeanFO [gap=10, min=20, max=100] *)
    | Continuous of ((string * string) * float * float option * float option)
    (* delta (X,Y) *)
    | Delta of (string * string)
    (* length (X,Y) *)
    | Length of (string * string)

end (* module Ast *)

(* ================================================================================ *)
module Lexicon : sig
  type t

  (** [load file] build a lexicon from a file.
      The file should contain same data than the ones in the build function
      in separate lines, each line used tabulation as separator *)
  val load: ?loc:Loc.t -> string -> t

  (** [reduce headers lexicon] build a smaller lexicon restricted to a subset of columns (defined in [headers]) *)
  (* val reduce: string list -> t -> t *)

  (** [union lex1 lex2] returns the union of two lexicons
      It supposed that the two lexicons define the same columns *)
  val union: t -> t -> t

  (** [filter_opt head value] returns the sublexicon with only items where the [head] column is match (Eq or Neq) to [value] if any, else returns None *)
  val filter_opt: Cmp.t -> string -> string -> t -> t option

  (** [read head lexicon] return the list of [value] of all items having in the [head] column equals to [value] *)
  val read_all: string -> t -> string list

  (** [get_opt head lexicon] return [value] if one items have the [value] in the [head] field *)
  val get_opt: string -> t -> string option

  (** [read_multi head lexicon] returns "v_1/…/v_k" where v_i are the values of the [head] column *)
  val read_multi: string -> t -> string

  val of_ast: ?loc:Loc.t -> string option -> Ast.lexicon -> t
end (* module Lexicon *)

(* ================================================================================ *)
module Lexicons : sig
  type t = (string * Lexicon.t) list

  val check: ?loc:Loc.t -> string -> string -> t -> unit
end (* module Lexicons *)

