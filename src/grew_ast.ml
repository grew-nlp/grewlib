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

  let feature_kind_to_string = function
    | Equality fv_list -> sprintf " = %s" (String.concat "|" fv_list)
    | Disequality [] -> ""
    | Disequality fv_list -> sprintf " <> %s" (String.concat "|" fv_list)
    | Equal_param param -> sprintf  " = $%s" param
    | Absent -> " <> *"

  type u_feature = {
    name: feature_name;
    kind: feature_kind;
  }
  let u_feature_to_string uf =
    sprintf "%s%s" uf.name (feature_kind_to_string uf.kind)

  type feature = u_feature * Loc.t


  let default_fs ?loc lab =
    match loc with
    | None -> [({name="label"; kind=Equality [lab]}, Loc.empty)]
    | Some l -> [({name="label"; kind=Equality [lab]}, l)]

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

  let check_duplicate_edge_identifier basic =
    let ids = List_.opt_map
      (function ({edge_id= Some e},loc) -> Some (e, loc) | _ -> None)
       basic.pat_edges in
    let rec loop = function
    | [] -> ()
    | (x,loc)::t when List.exists (fun (y,_) -> x=y) t ->
        Error.build ~loc "The identifier '%s' is used twice" x
    | _::t -> loop t in
    loop ids

  let normalize_pattern pattern =
    check_duplicate_edge_identifier pattern.pat_pos;
    { pattern with pat_negs =
        List.map
          (fun pat_neg ->
            { pat_neg with pat_edges =
              List.map
                (fun (u_edge,loc) ->
                  match u_edge.edge_id with
                  | None -> (u_edge,loc)
                  | Some id ->
                    Log.fwarning "[%s] identifier \"%s\" is useless in without part" (Loc.to_string loc) id;
                    ({u_edge with edge_id=None},loc)
                ) pat_neg.pat_edges
            }
          ) pattern.pat_negs
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
      | Features_eq ((name1,_), (name2,_))
      | Features_diseq ((name1,_), (name2,_))
      | Features_ineq (_, (name1,_), (name2,_))
      | Immediate_prec (name1, name2)
      | Large_prec (name1, name2) ->
        acc
        |> (add_implicit_node loc aux name1)
        |> (add_implicit_node loc aux name2)
      | Feature_ineq_cst (_, (name,_), _)
      | Feature_eq_cst ((name,_), _)
      | Feature_diff_cst ((name,_), _)
      | Feature_eq_float ((name,_), _)
      | Feature_diff_float ((name,_), _)
      | Feature_eq_regexp ((name,_), _)
      | Cst_in (name,_)
      | Cst_out (name, _) ->
        acc
        |> (add_implicit_node loc aux name)
    ) pat_nodes_2 pat_const in

    {pat_nodes=pat_nodes_3; pat_edges; pat_const}

  let complete_pattern pattern =
    let new_pat_pos = complete_basic [] pattern.pat_pos in
    let aux = new_pat_pos.pat_nodes in
    let new_pat_negs = List.map (complete_basic aux) pattern.pat_negs in
    { pat_pos = new_pat_pos; pat_negs = new_pat_negs;}

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
  type command = u_command * Loc.t

  let string_of_u_command u_command = match u_command with
    | Del_edge_expl (n1,n2,label) ->
      sprintf "del_edge %s -[%s]-> %s" n1 label n2
    | Del_edge_name name -> sprintf "del_edge %s" name
    | Add_edge (n1,n2,label) ->
      sprintf "add_edge %s -[%s]-> %s" n1 label n2
    | Add_edge_expl (n1,n2,name) ->
        sprintf "add_edge %s: %s -> %s" name n1 n2

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

    | New_node (n) -> sprintf "add_node %s" n
    | New_before (n1,n2) -> sprintf "add_node %s :< %s" n1 n2
    | New_after (n1,n2) -> sprintf "add_node %s :> %s" n1 n2
    | Del_node act_id -> sprintf "del_node %s" act_id
    | Update_feat ((act_id, feat_name),item_list) ->
      sprintf "%s.%s = %s" act_id feat_name (List_.to_string string_of_concat_item " + " item_list)
    | Del_feat (act_id, feat_name) ->
      sprintf "del_feat %s.%s" act_id feat_name

  (* the [rule] type is used for 3 kinds of module items:
     - rule     { param=None; ... }
     - lex_rule
  *)
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

  type strat_def = (* /!\ The list must not be empty in the Seq constructor *)
    | Ref of string            (* reference to a module name or to another strategy *)
    | Seq of strat_def list    (* a sequence of strategies to apply one after the other *)
    | Star of strat_def        (* a strategy to apply iteratively *)
    | Pick of strat_def        (* pick one normal form a the given strategy; return 0 if nf *)
    | Sequence of string list  (* compatibility mode with old code *)

  let rec strat_def_to_string = function
  | Ref m -> m
  | Seq l -> "(" ^ (String.concat "; " (List.map strat_def_to_string l)) ^ ")"
  | Star s -> "(" ^ (strat_def_to_string s) ^")" ^ "*"
  | Pick s -> "pick" ^ "(" ^(strat_def_to_string s)^")"
  | Sequence names -> "{" ^ (String.concat ";" names) ^ "}"

  (* invariant: Seq list and Plus list are not empty in the input and so not empty in the output *)
  let rec strat_def_flatten = function
  | Sequence l -> Sequence l
  | Ref m -> Ref m
  | Star s -> Star (strat_def_flatten s)
  | Pick s -> Pick (strat_def_flatten s)
  | Seq l ->
    let fl = List.map strat_def_flatten l in
    let rec loop = function
    | [] -> []
    | (Seq l) :: tail -> l @ (loop tail)
    | x :: tail -> x :: (loop tail)
    in Seq (loop fl)

  type strategy = {
    strat_name: string;
    strat_def: strat_def;
    strat_doc: string list;
    strat_loc: Loc.t;
  }

  (** a GRS: graph rewriting system *)
  type module_or_include =
    | Modul of modul
    | Includ of (string * Loc.t)

  type feature_spec =
    | Closed of feature_name * feature_atom list (* cat:V,N *)
    | Open of feature_name (* phon, lemma, ... *)
    | Num of feature_name (* position *)

  let build_closed feature_name feature_values =
    let sorted_list = List.sort Pervasives.compare feature_values in
    let without_duplicate =
      let rec loop = function
        | [] -> []
        | x::y::tail when x=y ->
          Log.fwarning "In the declaration of the feature name \"%s\", the value \"%s\" appears more than once" feature_name x;
          loop (y::tail)
        | x::tail -> x:: (loop tail)
      in loop sorted_list in
    Closed (feature_name, without_duplicate)

  type domain = {
    feature_domain: feature_spec list;
    label_domain: (string * string list) list;
  }

  type domain_wi = Dom of domain | Dom_file of string

  type grs_wi = {
    domain_wi: domain_wi option;
    modules_wi: module_or_include list;
    strategies_wi: strategy list;
  }

  type grs = {
    domain: domain option;
    modules: modul list;
    strategies: strategy list;
  }

  type gr = {
    meta: string list;
    nodes: node list;
    edges: edge list;
  }

  let complete id nodes =
    let rec loop n = match n with
    | [] -> [{node_id=id; position=None; fs=default_fs id},Loc.empty]
    | ({ node_id = head_id },_)::_ when head_id = id -> n
    | head::tail -> head :: (loop tail)
  in loop nodes

  let complete_graph gr =
    let new_nodes =
      List.fold_left
        (fun acc (edge,_) ->
          acc
          |> (complete edge.src)
          |> (complete edge.tar)
        ) gr.nodes gr.edges in
    { gr with nodes = new_nodes }

  let empty_grs = { domain = None; modules = []; strategies= [] }

  (* phrase structure tree *)
  type pst =
  | Leaf of (Loc.t * string) (* phon *)
  | T of (Loc.t * string * pst list)

  let rec word_list = function
    | Leaf (_, p) -> [p]
    | T (_,_,l) -> List.flatten (List.map word_list l)
end (* module Ast *)



(* ================================================================================================ *)
module New_ast = struct
  type strat =
  | Ref of Ast.node_ident       (* reference to a rule name or to another strategy *)
  | Pick of strat               (* pick one normal form a the given strategy; return 0 if nf *)
  | Alt of strat list           (* a set of strategies to apply in parallel *)
  | Seq of strat list           (* a sequence of strategies to apply one after the other *)
  | Iter of strat               (* a strategy to apply iteratively *)
  | If of strat * strat * strat (* choose a stragegy with a test *)
  | Try of strat                (* ≜ If (S, S, Empty): pick one normal form a the given strategy; return input if nf *)

  type decl =
  | Features of Ast.feature_spec list
  | Labels of (string * string list) list
  | Package of (Loc.t * Ast.simple_ident * decl list)
  | Rule of Ast.rule
  | Strategy of (Loc.t * Ast.simple_ident * strat)
  | Import of string
  | Include of string

  type grs = decl list

  let strat_list grs =
    let rec loop pref = function
    [] -> []
    | Strategy (_,name,_) :: tail -> name :: (loop pref tail)
    | Package (_,pack_name,decl_list) :: tail -> (loop (pref^"."^pack_name) decl_list) @  (loop pref tail)
    | _ :: tail -> loop pref tail
    in loop "" grs

  (* conversion from old grs to new ones *)
  let modul2package modul =
    let decl_list = List.map (fun rule -> Rule rule) modul.Ast.rules in
    Package (modul.Ast.mod_loc, modul.Ast.module_id, decl_list)

  let convert_strat det_modules old_strat =
    let new_strat_list =
      match old_strat.Ast.strat_def with
      | Ast.Sequence module_list ->
        Seq (List.map
          (fun name ->
            if List.mem name det_modules
            then Pick (Iter (Ref name))
            else Iter (Ref name)
          ) module_list
        )
      | _ -> failwith "No translation of old strat ≠ Sequence" in
    let strat_name = old_strat.Ast.strat_name in
    let loc = old_strat.Ast.strat_loc in
    Strategy (loc, strat_name, new_strat_list)

  let convert old_grs =
    let new_domain =
      match old_grs.Ast.domain with
      | None -> []
      | Some { Ast.feature_domain; label_domain } -> [Features feature_domain; Labels label_domain] in
    let packages = List.map modul2package old_grs.Ast.modules in
    let det_modules = List.fold_left (fun acc modul -> if modul.Ast.deterministic then modul.Ast.module_id::acc else acc) [] old_grs.Ast.modules in
    let new_strat_list = List.map (convert_strat det_modules) old_grs.Ast.strategies in
    new_domain @ packages @ new_strat_list
  end (* module New_ast *)






