(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2021 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: https://grew.fr                                                     *)
(*    License: CeCILL (see LICENSE folder or "http://cecill.info/")            *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open Printf
open Log
open Grew_base

type feature_name = string (* upos, Gender, … *)
type string_feature_value = string (* V, 4, "free text", … *)

type feature_value =
  | String of string
  | Float of float

let string_of_value = function
  | String s -> Str.global_replace (Str.regexp "\"") "\\\""
                  (Str.global_replace (Str.regexp "\\\\") "\\\\\\\\" s)
  | Float f -> sprintf "%g" f

let json_of_value = function
  | String s -> `String s
  | Float f ->  `Float f

let conll_string_of_value = function
  | String s -> s
  | Float f -> sprintf "%g" f

let numeric_feature_values = [
  "level";  (* use for edges in UDtoSUD grs *)
  "freq"; "freq_old"; "freq_new"; (* use for nodes in POStoSSQ grs *)
  "_start"; "_stop";  (* nodes in Orfeo timestamps *)
  "AlignBegin"; "AlignEnd";  (* nodes in SUD_Naija *)
  "length"; "delta";
]

(* Typing float/string for feature value is hardcoded, should evolve with a new config implementation *)
let typed_vos feat_name string_value =
  if List.mem feat_name numeric_feature_values
  then
    begin
      match float_of_string_opt string_value with
      | Some f -> Float f
      | None when string_value = "unknown" -> Float (-1.) (* to deal with AlignBegin=unknown|AlignEnd=unknown in SUD_Naija *)
      | None -> Error.run "The featue \"%s\" must be numeric, it cannot be associated with value: \"%s\"" feat_name string_value
    end
  else String string_value

let concat_feature_values ?loc = function
  | [one] -> one
  | l ->
    let rec loop = function
      | [] -> ""
      | String s :: tail -> s ^ (loop tail)
      | Float _ :: _ -> Error.run ?loc "Cannot concat with numeric value" in
    String (loop l)

let parse_meta s =
  match Str.bounded_split (Str.regexp "# *\\| *= *") s 2 with
  | [key;value] -> (key,value)
  | _ -> ("",s)

let string_of_meta = function
  | ("", s) -> s
  | (k,v) -> sprintf "# %s = %s" k v



(* ================================================================================ *)
module Ast = struct

  (* general function for checking that an identifier is of the right kind *)
  (* allowed is a char list which is a sub set of ['#'; '.'; ':'; '*'] *)
  let check_special name allowed s =
    let rec loop = function
      | [] -> ()
      | Str.Delim wrong_char :: _ when not (List.mem wrong_char allowed) ->
        Error.build "The identifier '%s' is not a valid %s, the character '%s' is illegal" s name wrong_char
      | _::t -> loop t in
    loop (Str.full_split (Str.regexp "#\\|\\.\\|:\\|\\*") s)

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
    | [Str.Text base; Str.Delim "."; Str.Text fn] -> (base, to_uname fn)
    | _ -> Error.build "The identifier '%s' must be a feature identifier (with exactly one '.' symbol, like \"V.cat\" for instance)" s

  (* ---------------------------------------------------------------------- *)
  (* simple_or_pointed: union of simple_ident, feature_ident and lex field *)
  (* Note: used for parsing of "X < Y" and "X.feat < Y.feat" without conflicts *)
  type pointed = string * string
  let dump_pointed (s1,s2) = sprintf "%s.%s" s1 s2
  type simple_or_pointed =
    | Simple of Id.name
    | Pointed of pointed

  let parse_simple_or_pointed s =
    check_special "feature ident" ["."] s;
    match Str.split (Str.regexp "\\.") s with
    | [base] -> Simple base
    | [s1; s2] -> Pointed (s1, to_uname s2)
    | _ -> Error.build "The identifier '%s' must be a feature identifier or a lexical reference (with at most one '.' symbol, like \"V\", \"V.cat\" or \"lex.cat\" for instance)" s


  (* ---------------------------------------------------------------------- *)
  type feature_kind =
    | Equality of string_feature_value list
    | Disequality of string_feature_value list
    | Equal_lex of string * string
    | Disequal_lex of string * string
    | Absent
    | Else of (string_feature_value * feature_name * string_feature_value)

  let feature_kind_to_string = function
    | Equality fv_list -> sprintf " = %s" (String.concat "|" fv_list)
    | Disequality [] -> ""
    | Disequality fv_list -> sprintf " <> %s" (String.concat "|" fv_list)
    | Equal_lex (lex,fn) -> sprintf " = %s.%s" lex fn
    | Disequal_lex (lex,fn) -> sprintf " <> %s.%s" lex fn
    | Absent -> " <> *"
    | Else (fv1, fn2, fv2) -> sprintf " = %s/%s = %s" fv1 fn2 fv2

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
    fs: feature list;
  }
  type node = u_node * Loc.t

  type atom_edge_label_cst =
    | Atom_eq of string * string list      (* 1=subj|obj  *)
    | Atom_diseq of string * string list   (* 1<>subj|obj *)
    | Atom_absent of string                (* !2          *)

  let string_of_atom_edge_label_cst = function
    | Atom_eq (lfeat, values) -> sprintf "%s=%s" lfeat (String.concat "|" values)
    | Atom_diseq (lfeat, values) -> sprintf "%s<>%s" lfeat (String.concat "|" values)
    | Atom_absent name -> sprintf "!%s" name

  type edge_label = string

  type edge_label_cst =
    | Pos_list of edge_label list           (*  A -[X|Y|Z]-> B      *)
    | Neg_list of edge_label list           (*  A -[^X|Y|Z]-> B     *)
    | Regexp of string                      (*  A -[re"a.*"]-> B    *)
    | Atom_list of atom_edge_label_cst list (*  A -[1=subj, 2]-> B  *)
    | Pred                                  (*  A < B               *)

  let string_of_edge_label_cst = function
    | Neg_list [] -> ""
    | Pos_list labels -> sprintf "[%s]" (List_.to_string (fun x->x) "|" labels)
    | Neg_list labels -> sprintf "[^%s]" (List_.to_string (fun x->x) "|" labels)
    | Regexp re -> sprintf "[re\"%s\"]" re
    | Atom_list l -> String.concat "," (List.map string_of_atom_edge_label_cst l)
    | Pred -> "PRED"

  type u_edge = {
    edge_id: Id.name option;
    src: Id.name;
    edge_label_cst: edge_label_cst;
    tar: Id.name;
  }
  type edge = u_edge * Loc.t

  type ineq = Lt | Gt | Le | Ge

  let check_ineq v1 ineq v2 =
    match ineq with
    | Lt -> v1 < v2
    | Gt -> v1 > v2
    | Le -> v1 <= v2
    | Ge -> v1 >= v2

  let string_of_ineq = function
    | Lt -> "<"
    | Gt -> ">"
    | Le -> "≤"
    | Ge -> "≥"

  type u_const =
    | Cst_out of Id.name * edge_label_cst
    | Cst_in of Id.name * edge_label_cst
    | Feature_equal of feature_ident * feature_ident
    | Feature_diff of feature_ident * feature_ident
    | Feature_ineq of ineq * feature_ident * feature_ident
    | Feature_ineq_cst of ineq * feature_ident * float
    | Feature_equal_regexp of feature_ident * string
    | Feature_equal_value of feature_ident * feature_value
    | Feature_diff_value of feature_ident * feature_value
    | Large_prec of Id.name * Id.name
    | Edge_disjoint of Id.name * Id.name
    | Edge_crossing of Id.name * Id.name

  type const = u_const * Loc.t

  type basic = {
    pat_nodes: node list;
    pat_edges: edge list;
    pat_const: const list;
  }

  let empty_basic = { pat_nodes=[]; pat_edges=[]; pat_const=[]; }

  let concat_basic b1 b2 =
    { pat_nodes=b1.pat_nodes @ b2.pat_nodes; pat_edges=b1.pat_edges @ b2.pat_edges; pat_const=b1.pat_const @ b2.pat_const; }

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

  let check_dup_edge_in_pattern pattern =
    let ids = List_.opt_map
        (function ({edge_id= Some e},loc) -> Some (e, loc) | _ -> None)
        pattern.pat_pos.pat_edges in
    let rec loop = function
      | [] -> ()
      | (x,loc)::t when List.exists (fun (y,_) -> x=y) t ->
        Error.build ~loc "The identifier '%s' is used twice" x
      | _::t -> loop t in
    loop ids

  let add_implicit_node loc aux name pat_nodes =
    if (List.exists (fun ({node_id},_) -> node_id=name) pat_nodes)
    || (List.exists (fun ({node_id},_) -> node_id=name) aux)
    then pat_nodes
    else ({node_id=name; fs=[]}, loc) :: pat_nodes

  let complete_basic_aux aux basic =
    let new_pat_nodes = List.fold_left
        (fun acc ({src; edge_label_cst; tar}, loc) ->
           if edge_label_cst = Pred
           then acc
           else
             acc
             |> (add_implicit_node loc aux src)
             |> (add_implicit_node loc aux tar)
        ) basic.pat_nodes basic.pat_edges in
    {basic with pat_nodes=new_pat_nodes}

  let complete_basic = complete_basic_aux []

  let complete_and_check_pattern pattern =
    check_dup_edge_in_pattern pattern;
    let new_pat_pos = complete_basic_aux [] pattern.pat_pos in
    let aux = new_pat_pos.pat_nodes in
    let new_pat_negs = List.map (complete_basic_aux aux) pattern.pat_negs in
    { pattern with pat_pos = new_pat_pos; pat_negs = new_pat_negs;}

  type concat_item =
    | Qfn_or_lex_item of pointed
    | String_item of string

  let string_of_concat_item = function
    | Qfn_or_lex_item pointed -> sprintf "%s.%s" (fst pointed) (snd pointed)
    | String_item s -> sprintf "\"%s\"" s

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
    (* Append_feats (src, tar, regexp, separator)*)
    | Append_feats of (Id.name * Id.name * string * string)
    | Unorder of Id.name

  type command = u_command * Loc.t

  let string_of_u_command u_command = match u_command with
    | Del_edge_expl (n1,n2,label) ->
      sprintf "del_edge %s -[%s]-> %s" n1 label n2
    | Del_edge_name name -> sprintf "del_edge %s" name
    | Add_edge (n1,n2,label) ->
      sprintf "add_edge %s -[%s]-> %s" n1 label n2
    | Add_edge_expl (n1,n2,name) ->
      sprintf "add_edge %s: %s -> %s" name n1 n2
    | Add_edge_items (n1,n2,items) ->
      sprintf "add_edge %s -[%s]-> %s" n1 (String.concat "," (List.map (fun (x,y) -> x^"="^y) items)) n2

    | Shift_in (n1,n2,edge_label_cst) ->
      sprintf "shift_in %s =%s=> %s" n1 (string_of_edge_label_cst edge_label_cst) n2
    | Shift_out (n1,n2,edge_label_cst) ->
      sprintf "shift_out %s =%s=> %s" n1 (string_of_edge_label_cst edge_label_cst) n2
    | Shift_edge (n1,n2,edge_label_cst) ->
      sprintf "shift %s =%s=> %s" n1 (string_of_edge_label_cst edge_label_cst) n2

    | New_node (n) -> sprintf "add_node %s" n
    | New_before (n1,n2) -> sprintf "add_node %s :< %s" n1 n2
    | New_after (n1,n2) -> sprintf "add_node %s :> %s" n1 n2
    | Del_node act_id -> sprintf "del_node %s" act_id
    | Update_feat ((act_id, feat_name),item_list) ->
      sprintf "%s.%s = %s" act_id feat_name (List_.to_string string_of_concat_item " + " item_list)
    | Del_feat (act_id, feat_name) ->
      sprintf "del_feat %s.%s" act_id feat_name
    | Append_feats (src, tar, regexp, "") ->
      sprintf "append_feats %s =%s=> %s" src regexp tar
    | Append_feats (src, tar, regexp, separator) ->
      sprintf "append_feats \"%s\" %s =%s=> %s" separator src regexp tar
    | Unorder n -> sprintf "unorder %s" n


  type lexicon =
    | File of string
    | Final of (int * string) list

  type lexicon_info = (string * lexicon) list

  type rule = {
    rule_id: Id.name;
    pattern: pattern;
    commands: command list;
    lexicon_info: lexicon_info;
    rule_doc: string list;
    rule_loc: Loc.t;
    rule_dir: string option; (* the real folder where the file is defined *)
    rule_path: string;
  }

  (* [label_spec] is the type for a label declaration: the name and a list of display options *)
  type label_spec = string * string list

  type feature_spec =
    | Closed of feature_name * string list (* cat:V,N *)
    | Open of feature_name (* phon, lemma, ... *)
    | Num of feature_name (* position *)

  let build_closed feature_name string_feature_values =
    let sorted_list = List.sort Stdlib.compare string_feature_values in
    let without_duplicate =
      let rec loop = function
        | [] -> []
        | x::y::tail when x=y ->
          Error.warning "In the declaration of the feature name \"%s\", the value \"%s\" appears more than once" feature_name x;
          loop (y::tail)
        | x::tail -> x:: (loop tail)
      in loop sorted_list in
    Closed (feature_name, without_duplicate)

  type gr = {
    meta: (string * string) list;
    nodes: node list;
    edges: edge list;
  }

  let complete id nodes =
    let rec loop n = match n with
      | [] -> [{node_id=id; fs=default_fs id},Loc.empty]
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

  (* phrase structure tree *)
  type pst =
    | Leaf of (Loc.t * string) (* phon *)
    | T of (Loc.t * string * pst list)

  let rec word_list = function
    | Leaf (_, p) -> [p]
    | T (_,_,l) -> List.flatten (List.map word_list l)

  type strat =
    | Ref of node_ident           (* reference to a rule name or to another strategy *)
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

  let rec strat_to_json = function
    | Ref name -> `Assoc [("Ref", `String name)]
    | Pick s -> `Assoc [("Pick", strat_to_json s)]
    | Onf s -> `Assoc [("Onf", strat_to_json s)]
    | Alt l -> `Assoc [("Alt", `List (List.map strat_to_json l))]
    | Seq l -> `Assoc [("Seq", `List (List.map strat_to_json l))]
    | Iter s -> `Assoc [("Iter", strat_to_json s)]
    | If (s, s1, s2) -> `Assoc [("If", strat_to_json s); ("Then", strat_to_json s1); ("Else", strat_to_json s2)]
    | Try s -> `Assoc [("Try", strat_to_json s)]

  let strat_list grs =
    List.fold_left
      (fun acc -> function
        | Strategy (_,name,_) -> name :: acc
        | _ -> acc
      ) [] grs

  let package_list grs =
    List.fold_left
      (fun acc -> function
        | Package (_,name,_) -> name :: acc
        | _ -> acc
      ) [] grs

  let rule_list grs =
    List.fold_left
      (fun acc -> function
        | Rule {rule_id} -> rule_id :: acc
        | _ -> acc
      ) [] grs

end (* module Ast *)
