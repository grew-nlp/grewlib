(**********************************************************************************)
(*    grewlib • a Graph Rewriting library dedicated to NLP applications           *)
(*                                                                                *)
(*    Copyright 2011-2025 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: https://grew.fr                                                    *)
(*    License: CeCILL (see LICENSE folder or "http://cecill.info/")               *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

open Printf
open Grew_utils
open Grew_types

(* ================================================================================ *)
module Regexp = struct
  type t =
    | All
    | Re of string * Str.regexp
    | Pcre of string * Re.re
    | Pcri of string * Re.re

  let all = All
  let re s = Re (s, Str.regexp s)
  let pcre s = Pcre (s, Re__Pcre.regexp s)
  let pcri s = Pcri (s, Re__Pcre.regexp ~flags:[`CASELESS] s)
  let to_string = function
    | All -> sprintf "re\".*\""
    | Re (s,_) -> sprintf "re\"%s\"" s
    | Pcre (s,_) -> sprintf "/%s/" s
    | Pcri (s,_) -> sprintf "/%s/i" s

  let re_match re string =
    match re with
    | All -> true
    | Re (_,re) -> String_.re_match re string
    | Pcre (_,rex) | Pcri (_,rex) ->
        try let groups = Re__Pcre.extract ~rex string in groups.(0) = string
        with Not_found -> false
end

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
  type feature_ident = Id.name * string
  let dump_feature_ident (name, feat_name) = sprintf "%s.%s" name feat_name

  let parse_feature_ident s =
    check_special "feature ident" ["."] s;
    match Str.full_split (Str.regexp "\\.") s with
    | [Str.Text base; Str.Delim "."; Str.Text fn] -> (base, fn)
    | _ -> Error.build "The identifier '%s' must be a feature identifier (with exactly one '.' symbol, like \"V.cat\" for instance)" s


  type key_ident = Id.name * string list

  let parse_key_ident s =
    check_special "feature ident" ["."] s;
    match Str.full_split (Str.regexp "\\.") s with
    | [Str.Text feature_name; Str.Delim "."; Str.Text values] -> (feature_name, Str.split (Str.regexp "/") values)
    | _ -> Error.build "The identifier '%s' must be a key identifier (with exactly one '.' symbol, like \"X.Number\" for instance)" s

  (* ---------------------------------------------------------------------- *)
  (* simple_or_pointed: union of simple_ident, feature_ident and lex field *)
  (* Note: used for parsing of "X < Y" and "X.feat < Y.feat" without conflicts *)
  type pointed = string * string
  let _dump_pointed (s1,s2) = sprintf "%s.%s" s1 s2

  type simple_or_pointed =
    | Simple of Id.name
    | Pointed of pointed

  let parse_simple_or_pointed s =
    check_special "feature ident" ["."] s;
    match Str.split (Str.regexp "\\.") s with
    | [base] -> Simple base
    | [s1; s2] -> Pointed (s1, s2)
    | _ -> Error.build "The identifier '%s' must be a feature identifier or a lexical reference (with at most one '.' symbol, like \"V\", \"V.cat\" or \"lex.cat\" for instance)" s


  (* ---------------------------------------------------------------------- *)
  type feature_kind =
    | Feat_kind_list of Cmp.t * string list
    | Feat_kind_lex of Cmp.t * string * string
    | Feat_kind_re of Cmp.t * Regexp.t
    | Absent
    | Else of (string * string * string)

  let feature_kind_to_string = function
    | Feat_kind_list (Neq, []) -> ""
    | Feat_kind_list (cmp, fv_list) -> sprintf " %s %s" (Cmp.to_string cmp) (String.concat "|" fv_list)
    | Feat_kind_lex (cmp,lex,fn) -> sprintf " %s %s.%s" (Cmp.to_string cmp) lex fn
    | Feat_kind_re (cmp,re) -> sprintf " %s re\"%s\"" (Cmp.to_string cmp) (Regexp.to_string re)
    | Absent -> " <> *"
    | Else (fv1, fn2, fv2) -> sprintf " = %s/%s = %s" fv1 fn2 fv2

  type u_feature = {
    name: string;
    kind: feature_kind;
  }
  let u_feature_to_string uf =
    sprintf "%s%s" uf.name (feature_kind_to_string uf.kind)

  type feature = u_feature * Loc.t


  let default_fs ?loc lab =
    match loc with
    | None -> [({name="label"; kind=Feat_kind_list (Eq, [lab])}, Loc.empty)]
    | Some l -> [({name="label"; kind=Feat_kind_list (Eq, [lab])}, l)]

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

  let string_of_atom_edge_label_cst = function
    | Atom_eq (lfeat, values) -> sprintf "%s=%s" lfeat (String.concat "|" values)
    | Atom_diseq (lfeat, values) -> sprintf "%s<>%s" lfeat (String.concat "|" values)
    | Atom_absent name -> sprintf "!%s" name

  type edge_label = string

  type edge_label_cst =
    | Pos_list of edge_label list           (*  A -[X|Y|Z]-> B      *)
    | Neg_list of edge_label list           (*  A -[^X|Y|Z]-> B     *)
    | Regexp of Regexp.t                    (*  A -[re"a.*"]-> B    *)
    | Atom_list of atom_edge_label_cst list (*  A -[1=subj, 2]-> B  *)
    | Pred                                  (*  A < B               *)

  let string_of_edge_label_cst = function
    | Neg_list [] -> ""
    | Pos_list labels -> sprintf "[%s]" (String.concat "|" labels)
    | Neg_list labels -> sprintf "[^%s]" (String.concat "|" labels)
    | Regexp re -> sprintf "[%s]" (Regexp.to_string re)
    | Atom_list l -> String.concat "," (List.map string_of_atom_edge_label_cst l)
    | Pred -> "PRED"

  type u_edge = {
    edge_id: Id.name option;
    src: Id.name;
    edge_label_cst: edge_label_cst;
    tar: Id.name;
  }
  type edge = u_edge * Loc.t

  type ineq = Eq | Neq| Lt | Gt | Le | Ge

  let check_ineq v1 ineq v2 =
    match ineq with
    | Eq -> v1 = v2
    | Neq -> v1 <> v2
    | Lt -> v1 < v2
    | Gt -> v1 > v2
    | Le -> v1 <= v2
    | Ge -> v1 >= v2

  let string_of_ineq = function
    | Lt -> "<"
    | Gt -> ">"
    | Le -> "≤"
    | Ge -> "≥"
    | Eq -> "="
    | Neq -> "≠"

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

  let empty_basic = { req_nodes=[]; req_edges=[]; req_const=[]; }

  let concat_basic b1 b2 =
    { req_nodes=b1.req_nodes @ b2.req_nodes; req_edges=b1.req_edges @ b2.req_edges; req_const=b1.req_const @ b2.req_const; }

  type u_glob =
    | Glob_cst of string
    | Glob_eq_list of string * string list
    | Glob_diff_list of string * string list
    | Glob_absent of string
    | Glob_regexp of string * Regexp.t

  type glob = u_glob * Loc.t

  let glob_to_string (u_glob,_) =
    match u_glob with
    | Glob_cst s -> s
    | Glob_eq_list (s,l) -> sprintf "%s = %s" s (String.concat "|" l)
    | Glob_diff_list (s,l) -> sprintf "%s <> %s" s (String.concat "|" l)
    | Glob_absent s -> sprintf "!%s" s
    | Glob_regexp (f,re) -> sprintf "%s = %s" f (Regexp.to_string re)

  type request = {
    req_glob: glob list;
    req_pos: basic;
    req_exts: (basic * bool) list; (* with iff true and without iff false *)
  }

  let check_dup_edge_in_request request =
    let ids = CCList.filter_map
        (function ({edge_id= Some e; _},loc) -> Some (e, loc) | _ -> None)
        request.req_pos.req_edges in
    let rec loop = function
      | [] -> ()
      | (x,loc)::t when List.exists (fun (y,_) -> x=y) t ->
        Error.build ~loc "The identifier '%s' is used twice" x
      | _::t -> loop t in
    loop ids

  let add_implicit_node loc aux name req_nodes =
    if (List.exists (fun ({node_id; _},_) -> node_id=name) req_nodes)
    || (List.exists (fun ({node_id; _},_) -> node_id=name) aux)
    then req_nodes
    else ({node_id=name; fs_disj=[[]]}, loc) :: req_nodes

  let complete_basic_aux aux basic =
    let new_req_nodes = List.fold_left
        (fun acc ({src; edge_label_cst; tar; _}, loc) ->
           if edge_label_cst = Pred
           then acc
           else
             acc
             |> (add_implicit_node loc aux src)
             |> (add_implicit_node loc aux tar)
        ) basic.req_nodes basic.req_edges in
    {basic with req_nodes=new_req_nodes}

  let complete_basic = complete_basic_aux []

  let complete_and_check_request request =
    check_dup_edge_in_request request;
    let new_req_pos = complete_basic_aux [] request.req_pos in
    let aux = new_req_pos.req_nodes in
    let new_req_exts = List.map (fun (ext, flag) -> (complete_basic_aux aux ext, flag)) request.req_exts in
    { request with req_pos = new_req_pos; req_exts = new_req_exts;}

  type concat_item =
    | Qfn_or_lex_item of (pointed * Range.t)
    | String_item of (string * Range.t)

  let string_of_concat_item = function
    | Qfn_or_lex_item (pointed,_) -> sprintf "%s.%s" (fst pointed) (snd pointed)
    | String_item (s,_) -> sprintf "\"%s\"" s

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
    (* Concat_feats (side, src, tar, regexp, separator)*)
    | Concat_feats of (side * Id.name * Id.name * Regexp.t * string)
    | Unorder of Id.name

    | Insert_before of (Id.name * Id.name)
    | Insert_after of (Id.name * Id.name)

  type command = u_command * Loc.t

  let string_of_u_command = function
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
      sprintf "%s.%s = %s" act_id feat_name (String.concat " + " (List.map string_of_concat_item item_list))
    | Del_feat (act_id, feat_name) ->
      sprintf "del_feat %s.%s" act_id feat_name
    | Concat_feats (Append, src, tar, regexp, "") ->
      sprintf "append_feats %s =%s=> %s" src (Regexp.to_string regexp) tar
    | Concat_feats (Append, src, tar, regexp, separator) ->
      sprintf "append_feats \"%s\" %s =%s=> %s" separator src (Regexp.to_string regexp) tar
    | Concat_feats (Prepend, src, tar, regexp, "") ->
      sprintf "prepend_feats %s =%s=> %s" src (Regexp.to_string regexp) tar
    | Concat_feats (Prepend, src, tar, regexp, separator) ->
      sprintf "prepend_feats \"%s\" %s =%s=> %s" separator src (Regexp.to_string regexp) tar
    | Unorder n -> sprintf "unorder %s" n
    | Insert_before (n1,n2) -> sprintf "insert %s :< %s" n1 n2
    | Insert_after (n1,n2) -> sprintf "insert %s :> %s" n1 n2

  type lexicon =
    | File of string
    | Final of (int * string) list

  type lexicon_info = (string * lexicon) list

  type rule = {
    rule_id: Id.name;
    request: request;
    commands: command list;
    lexicon_info: lexicon_info;
    rule_doc: string list;
    rule_loc: Loc.t;
    rule_dir: string option; (* the real directory where the file is defined *)
    rule_path: string;
  }

  (* [label_spec] is the type for a label declaration: the name and a list of display options *)
  type label_spec = string * string list

  type feature_spec =
    | Closed of string * string list (* cat:V,N *)
    | Open of string (* phon, lemma, ... *)
    | Num of string (* position *)

  let build_closed feature_name string_feature_values =
    let sorted_list = List.sort Stdlib.compare string_feature_values in
    let without_duplicate =
      let rec loop = function
        | [] -> []
        | x::y::tail when x=y ->
          Warning.magenta "In the declaration of the feature name \"%s\", the value \"%s\" appears more than once" feature_name x;
          loop (y::tail)
        | x::tail -> x:: (loop tail) in
      loop sorted_list in
    Closed (feature_name, without_duplicate)

  type gr = {
    meta: (string * string) list;
    nodes: node list;
    edges: edge list;
  }

  let complete id nodes =
    let rec loop n =
      match n with
      | [] -> [{node_id=id; fs_disj=[default_fs id]},Loc.empty]
      | ({ node_id = head_id ; _},_)::_ when head_id = id -> n
      | head::tail -> head :: (loop tail) in
    loop nodes

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

  let rec strat_to_string = function
    | Ref name -> name
    | Pick s -> sprintf "Pick (%s)" (strat_to_string s)
    | Onf s -> sprintf "Onf (%s)" (strat_to_string s)
    | Alt l -> sprintf "Alt (%s)" (String.concat "," (List.map strat_to_string l))
    | Seq l -> sprintf "Seq (%s)" (String.concat "," (List.map strat_to_string l))
    | Iter s -> sprintf "Iter (%s)" (strat_to_string s)
    | If (s, s1, s2) -> sprintf "If (%s,%s,%s)" (strat_to_string s) (strat_to_string s1) (strat_to_string s2)
    | Try s -> sprintf "Try (%s)" (strat_to_string s)

  let strat_list grs =
    List.fold_left
      (fun acc -> function
        | Strategy (_,name,_) -> name :: acc
        | _ -> acc
      ) [] grs

  let fold_sub_strat fct init =
    let rec loop acc = function
      | Ref name -> fct name acc
      | Pick s | Iter s | Onf s | Try s -> loop acc s
      | Alt l | Seq l -> List.fold_left (fun acc2 s -> loop acc2 s) acc l
      | If (s1, s2, s3) -> List.fold_left (fun acc2 s -> loop acc2 s) acc [s1;s2;s3] in
    loop init

  let strat_lists grs =
    let (strat_list, sub_strat_set) =
      List.fold_left
      (fun (acc_strat_list, acc_sub_strat_set) -> function
        | Strategy (_,name,strat) ->
          let new_acc_sub_strat_set = fold_sub_strat String_set.add acc_sub_strat_set strat in
          (name :: acc_strat_list, new_acc_sub_strat_set)
        | _ -> (acc_strat_list, acc_sub_strat_set)
      ) ([],String_set.empty) grs in
      (strat_list, List.filter (fun x -> not (String_set.mem x sub_strat_set)) strat_list)


  let package_list grs =
    List.fold_left
      (fun acc -> function
        | Package (_,name,_) -> name :: acc
        | _ -> acc
      ) [] grs

  let rule_list grs =
    List.fold_left
      (fun acc -> function
        | Rule {rule_id; _} -> rule_id :: acc
        | _ -> acc
      ) [] grs

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
module Lexicon = struct
  module Line_set = Set.Make (struct type t=string list let compare = Stdlib.compare end)

  type t = {
    header: string list;  (* ordered list of column headers *)
    lines: Line_set.t;
    loc: Loc.t option;
  }

  let rec transpose = function
    | [] -> []
    | [] :: xss -> transpose xss
    | (x::xs) :: xss -> (x :: List.map List.hd xss) :: transpose (xs :: List.map List.tl xss)

  exception Equal of string
  let strict_compare x y =
    match Stdlib.compare x y with
    | 0 -> raise (Equal x)
    | x -> x

  (** [build loc items] build a lexicon from a list.
      The first list is interpreted as the column headers.
      All other lines are lexicon items.
      It is supposed that all sublist have the same length *)
  let of_item_list ?loc items =
    let real_items = List.filter (fun (_,x) -> x <> "" && x.[0] <> '%') items in
    match real_items with
    | [] | [_] -> Error.build ?loc "[Lexicon.of_ast] a lexicon must not be empty"
    | (linenum_h, h)::t ->
      let fields = Str.split (Str.regexp "\t") h in
      let l = List.length fields in
      let rec loop = function
        | [] -> []
        | (linenum, line)::tail ->
          let norm_line =
            if String.length line > 1 && line.[0] = '\\' && line.[1] = '%'
            then String_.rm_first_char line
            else line in
          let items = Str.split (Str.regexp "\t") norm_line in
          if List.length items <> l then
            begin
              let loc = CCOption.map (Loc.set_line linenum) loc in
              Error.build ?loc "[Lexicon.of_ast] line with %d items (%d expected!!)" (List.length items) l
            end;
          items :: (loop tail) in
      let items_list = fields ::(loop t) in
      let tr = transpose items_list in
      try
        let sorted_tr = List.sort (fun l1 l2 -> strict_compare (List.hd l1) (List.hd l2)) tr in
        match transpose sorted_tr with
        | [] -> Error.bug ?loc "[Lexicon.of_ast] inconsistent data"
        | header :: lines_list -> { header; lines = List.fold_right Line_set.add lines_list Line_set.empty; loc }
      with Equal v ->
        let loc = CCOption.map (Loc.set_line linenum_h) loc in
        Error.build ?loc "[Lexicon.of_ast] the field name \"%s\" is used twice" v

  let load ?loc file =
    try
      Global.update_grs_timestamp file;
      let lines =
        CCIO.(with_in file read_lines_l)
        |> CCList.mapi (fun i l -> (i+1,l))
        (* Blanks lines (empty or only with spaces and tabs) and lines starting with '%' are ignored. *)
        |> List.filter (fun (_,line) -> not ((Str.string_match (Str.regexp "^[ \t]*$") line 0) || (line.[0] = '%'))) in
      of_item_list ~loc:(Loc.file file) lines
    with Sys_error _ -> Error.build ?loc "[Lexicon.load] unable to load file %s" file

  let union lex1 lex2 =
    if lex1.header <> lex2.header then Error.build "[Lexicon.union] different header";
    { lex1 with lines = Line_set.union lex1.lines lex2.lines }
  (* NOTE: the loc field of a union may be not accurate *)

  let filter_opt cmp head value lex =
    match List_.index_opt head lex.header with
    | None -> Error.build ?loc:lex.loc "[Lexicon.filter_opt] cannot find the fiels \"%s\" in lexicon" head
    | Some index ->
      let new_set = Line_set.filter (fun line -> Cmp.fct cmp (List.nth line index) value) lex.lines in
      if Line_set.is_empty new_set
      then None
      else Some { lex with lines = new_set }

  let projection head lex =
    match List_.index_opt head lex.header with
    | None -> Error.build ?loc:lex.loc "[Lexicon.projection] cannot find %s in lexicon" head
    | Some index -> Line_set.fold (fun line acc -> String_set.add (List.nth line index) acc) lex.lines String_set.empty

  let read_all head lex =
    match String_set.elements (projection head lex) with
    | [] -> Error.bug "[Lexicon.read] a lexicon must not be empty"
    | l -> l

  let get_opt head lex = String_set.choose_opt (projection head lex)

  let read_multi head lex =
    match String_set.elements (projection head lex) with
    | [] -> Error.bug "[Lexicon.read] a lexicon must not be empty"
    | l -> String.concat "/" l

  let of_ast ?loc dir_opt = function
    | Ast.File filename ->
      let dir = match dir_opt with Some s -> s | None -> Sys.getcwd () in
      if Filename.is_relative filename
      then load ?loc (Filename.concat dir filename)
      else load ?loc filename
    | Ast.Final (line_list) -> of_item_list ?loc line_list
end (* module Lexicon *)

(* ================================================================================ *)
module Lexicons = struct
  type t = (string * Lexicon.t) list

  let check ?loc lexicon_name field_name t =
    match List.assoc_opt lexicon_name t with
    | None ->
      Error.build ?loc "Undefined lexicon name \"%s\"" lexicon_name
    | Some lexicon when not (List.mem field_name lexicon.Lexicon.header) ->
      Error.build ?loc "Undefined field name \"%s\" in lexicon %s" field_name lexicon_name
    | _ -> ()
end (* module Lexicons *)

