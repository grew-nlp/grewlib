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
  (* allowed is a char list which is a sub set of ['#'; '.'; ':'; '*'] *)
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
  (* node_ident: W0.5 *)
  type node_ident = string
  let parse_node_ident s = check_special "node ident" ["."] s; s
  let dump_node_ident name = name

  (* ---------------------------------------------------------------------- *)
  (* feature_ident: V.cat *)
  type feature_ident = Id.name * feature_name
  let dump_feature_ident (name, feat_name) = sprintf "%s.%s" name feat_name

  let parse_feature_ident s =
    check_special "feature ident" ["."] s;
    match Str.full_split (Str.regexp "\\.") s with
    | [Str.Text base; Str.Delim "."; Str.Text fn] -> (base, fn)
    | _ -> Error.build "The identifier '%s' must be a feature identifier (with exactly one '.' symbol, like \"V.cat\" for instance)" s

  (* ---------------------------------------------------------------------- *)
  (* simple_or_feature_ident: union of simple_ident and feature_ident *)
  (* Note: used for parsing of "X < Y" and "X.feat < Y.feat" without conflicts *)
  type simple_or_feature_ident = Id.name * feature_name option

  let parse_simple_or_feature_ident s =
    check_special "feature ident" ["."] s;
    match Str.full_split (Str.regexp "\\.") s with
    | [Str.Text base; ] -> (base, None)
    | [Str.Text base; Str.Delim "."; Str.Text fn] -> (base, Some fn)
    | _ -> Error.build "The identifier '%s' must be a feature identifier (with at most one '.' symbol, like \"V\" or \"V.cat\" for instance)" s


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

  let grewpy_compare (n1,_) (n2,_) = Id.grewpy_compare n1.node_id n2.node_id

  type edge_label = string

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

  let string_of_ineq = function
    | Lt -> "<"
    | Gt -> ">"
    | Le -> "≤"
    | Ge -> "≥"

  type u_const =
    | Cst_out of Id.name * edge_label_cst
    | Cst_in of Id.name * edge_label_cst
    | Feature_eq of feature_ident * feature_ident
    | Feature_diseq of feature_ident * feature_ident
    | Feature_ineq of ineq * feature_ident * feature_ident
    | Feature_ineq_cst of ineq * feature_ident * float
    | Feature_float of feature_ident * float
    | Feature_diff_float of feature_ident * float

    | Feature_re of feature_ident * string
    | Feature_cst of feature_ident * string
    | Feature_diff_cst of feature_ident * string

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

  let add_implicit_node loc aux name pat_nodes =
    if (List.exists (fun ({node_id},_) -> node_id=name) pat_nodes)
    || (List.exists (fun ({node_id},_) -> node_id=name) aux)
    then pat_nodes
    else ({node_id=name; position=None; fs=[]}, loc) :: pat_nodes

  let complete_basic aux {pat_nodes; pat_edges; pat_const} =
    let pat_nodes_2 = List.fold_left
    (fun acc ({src; tar}, loc) ->
      acc
      |> (add_implicit_node loc aux src)
      |> (add_implicit_node loc aux tar)
    ) pat_nodes pat_edges in

    let pat_nodes_3 = List.fold_left
    (fun acc (u_const, loc) -> match u_const with
      | Feature_eq ((name1,_), (name2,_))
      | Feature_diseq ((name1,_), (name2,_))
      | Feature_ineq (_, (name1,_), (name2,_))
      | Prec (name1, name2)
      | Lprec (name1, name2) ->
        acc
        |> (add_implicit_node loc aux name1)
        |> (add_implicit_node loc aux name2)
      | Feature_ineq_cst (_, (name,_), _)
      | Feature_cst ((name,_), _)
      | Feature_diff_cst ((name,_), _)
      | Feature_float ((name,_), _)
      | Feature_diff_float ((name,_), _)
      | Feature_re ((name,_), _) ->
        acc
        |> (add_implicit_node loc aux name)
      | _ -> acc
    ) pat_nodes_2 pat_const in

    {pat_nodes=pat_nodes_3; pat_edges; pat_const}

  let complete_pattern pattern =
    let new_pat_pos = complete_basic [] pattern.pat_pos in
    let aux = new_pat_pos.pat_nodes in
    let new_pat_negs = List.map (complete_basic aux) pattern.pat_negs in
    { pat_pos = new_pat_pos; pat_negs = new_pat_negs;}

  type graph = {
    nodes: (Id.name * node) list;
    edge: edge list;
  }

  type concat_item =
    | Qfn_item of feature_ident
    | String_item of string
    | Param_item of string

  let string_of_concat_item = function
    | Qfn_item id -> sprintf "%s" (dump_feature_ident id)
    | String_item s -> sprintf "\"%s\"" s
    | Param_item var -> sprintf "%s" var

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
  type command = u_command * Loc.t

  let string_of_u_command u_command = match u_command with
    | Del_edge_expl (n1,n2,label) ->
      sprintf "del_edge %s -[%s]-> %s" n1 label n2
    | Del_edge_name name -> sprintf "del_edge %s" name
    | Add_edge (n1,n2,label) ->
      sprintf "add_edge %s -[%s]-> %s" n1 label n2

    | Shift_in (n1,n2,Neg_list []) ->
      sprintf "shift_in %s ==> %s" n1 n2
    | Shift_in (n1,n2,Pos_list labels) ->
      sprintf "shift_in %s =[%s]=> %s" n1 n2 (List_.to_string (fun x->x) "|" labels)
    | Shift_in (n1,n2,Neg_list labels) ->
      sprintf "shift_in %s =[^%s]=> %s" n1 n2 (List_.to_string (fun x->x) "|" labels)
    | Shift_in (n1,n2,Regexp re) ->
      sprintf "shift_in %s =[re\"%s\"]=> %s" n1 re n2

    | Shift_out (n1,n2,Neg_list []) ->
      sprintf "shift_out %s ==> %s" n1 n2
    | Shift_out (n1,n2,Pos_list labels) ->
      sprintf "shift_out %s =[%s]=> %s" n1 n2 (List_.to_string (fun x->x) "|" labels)
    | Shift_out (n1,n2,Neg_list labels) ->
      sprintf "shift_out %s =[^%s]=> %s" n1 n2 (List_.to_string (fun x->x) "|" labels)
    | Shift_out (n1,n2,Regexp re) ->
      sprintf "shift_out %s =[re\"%s\"]=> %s" n1 re n2


    | Shift_edge (n1,n2,Neg_list []) ->
      sprintf "shift %s ==> %s" n1 n2
    | Shift_edge (n1,n2,Pos_list labels) ->
      sprintf "shift %s =[%s]=> %s" n1 n2 (List_.to_string (fun x->x) "|" labels)
    | Shift_edge (n1,n2,Neg_list labels) ->
      sprintf "shift %s =[^%s]=> %s" n1 n2 (List_.to_string (fun x->x) "|" labels)
    | Shift_edge (n1,n2,Regexp re) ->
      sprintf "shift %s =[re\"%s\"]=> %s" n1 re n2

    | Merge_node (n1,n2) -> sprintf "merge %s ==> %s" n1 n2
    | New_neighbour (n1,n2,label) -> sprintf "add_node %s: <-[%s]- %s" n1 label n2
    | New_node (n) -> sprintf "add_node %s" n
    | New_before (n1,n2) -> sprintf "add_node %s :< %s" n1 n2
    | New_after (n1,n2) -> sprintf "add_node %s :> %s" n1 n2
    | Del_node act_id -> sprintf "del_node %s" act_id
    | Update_feat ((act_id, feat_name),item_list) ->
      sprintf "%s.%s = %s" act_id feat_name (List_.to_string string_of_concat_item " + " item_list)
    | Del_feat (act_id, feat_name) ->
      sprintf "del_feat %s.%s" act_id feat_name

  let rec replace_new_neighbour = function
  | [] -> []
  | (New_neighbour (new_name, old_name, edge),loc) :: tail ->
    (New_after (new_name, old_name),loc) :: (Add_edge (old_name, new_name, edge),loc) :: (replace_new_neighbour tail)
  | head :: tail -> head :: (replace_new_neighbour tail)

  (* the [rule] type is used for 3 kinds of module items:
     - rule     { param=None; ... }
     - lex_rule
     - filter   { param=None; commands=[]; ... }
  *)
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

  let rec new_sequence_to_string = function
  | Ref m -> m
  | List l -> "[" ^ (String.concat "; " (List.map new_sequence_to_string l)) ^ "]"
  | Plus l -> "[" ^ (String.concat "+" (List.map new_sequence_to_string l)) ^ "]"
  | Star s -> "[" ^ (new_sequence_to_string s) ^"]"  ^ "*"
  | Diamond s -> "◇" ^ "[" ^(new_sequence_to_string s)^"]"

  let rec flatten = function
  | Ref m -> Ref m
  | Star s -> Star (flatten s)
  | Diamond s -> Diamond (flatten s)
  | List l ->
    let fl = List.map flatten l in
    let rec loop = function
    | [] -> []
    | (List l) :: tail -> l @ (loop tail)
    | x :: tail -> x :: (loop tail)
    in List (loop fl)
  | Plus l ->
    let fl = List.map flatten l in
    let rec loop = function
    | [] -> []
    | (Plus l) :: tail -> l @ (loop tail)
    | x :: tail -> x :: (loop tail)
    in Plus (loop fl)

  type sequence =
  | Old of old_sequence
  | New of ((string * Loc.t) * new_sequence)

  (** a GRS: graph rewriting system *)
  type module_or_include =
    | Modul of modul
    | Includ of (string * Loc.t)

  type domain = {
      feature_domain: Feature_domain.feature_spec list;
      label_domain: (string * string list) list;
    }

  let empty_domain = { feature_domain=[]; label_domain=[] }

  type domain_wi = Dom of domain | Dom_file of string

  type grs_wi = {
    domain_wi: domain_wi option;
    modules_wi: module_or_include list;
    sequences_wi: sequence list;
  }

  type grs = {
    domain: domain option;
    modules: modul list;
    sequences: sequence list;
  }

  type gr = {
    meta: string list;
    nodes: node list;
    edges: edge list;
  }

  let empty_grs = { domain = None; modules = []; sequences= [] }

end (* module Ast *)
