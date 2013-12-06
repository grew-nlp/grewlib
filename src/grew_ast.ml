open Printf
open Log

open Grew_utils

module Ast = struct
  let dot_split s = Str.split (Str.regexp "\\.") s
  let get_single s = match dot_split s with
    | [one] -> one
    | _ -> Error.build "The identifier '%s' contains the '.' symbol" s

  type feature_name = string (* cat, num, ... *)
  type feature_atom = string (* V, N, inf, ... *)
  type feature_value = string (* V, 4, "free text", ... *)

  (* -------------------------------------------------------------------------------- *)
  (* complex_id: V, V#alpha, V.cat, V#alpha.cat, p_obj.loc *)
  type complex_id =
    | No_sharp of string
    | Sharp of string * string

  let complex_id_to_string = function
    | No_sharp x -> x
    | Sharp (x,y) -> x ^ "#" ^ y

  (* -------------------------------------------------------------------------------- *)
  (* simple_id: V *)
  type simple_id = Id.name

  let simple_id_of_ci ci = match ci with
    | No_sharp s -> get_single s
    | Sharp _ -> Error.build "The identifier '%s' must be basic (without '#' symbol)" (complex_id_to_string ci)
  let is_simple = function
    | No_sharp s when List.length (dot_split s) = 1 -> true
    | _ -> false

  (* -------------------------------------------------------------------------------- *)
  (* label_id: p_obj.loc x.y.z *)
  type label_id = string

  let label_id_of_ci ci = match ci with
    | No_sharp s -> s
    | Sharp _ -> Error.build "The identifier '%s' must be a label (without '#' symbol)" (complex_id_to_string ci)

  (* -------------------------------------------------------------------------------- *)
  (* act_id: V, V#alpha *)
  type act_id = Id.name * string option

  let act_id_of_ci = function
    | No_sharp s -> (get_single s, None)
    | Sharp (s1,s2) -> (get_single s1, Some (get_single s2))

  let act_id_to_string = function
    | (base, None) -> base
    | (base, Some ln) -> sprintf "%s#%s" base ln

  (* -------------------------------------------------------------------------------- *)
  (* simple_qfn: V.cat *)
  type simple_qfn = Id.name * feature_name
  let simple_qfn_of_ci ci = match ci with
    | No_sharp s ->
      (match dot_split s with
        | [base;fn] -> (base, fn)
        | _ -> Error.build "The identifier '%s' must be a qualified feature name (with one '.' symbol)" s
      )
    | Sharp _ -> Error.build "The identifier '%s' must be a qualified feature name (without '#' symbol)" (complex_id_to_string ci)
  let simple_qfn_to_string (name, feat_name) = sprintf "%s.%s" name feat_name


  (* -------------------------------------------------------------------------------- *)
  (* act_qfn: V.cat, V#alpha.cat *)
  type act_qfn = act_id * feature_name

  let act_qfn_of_ci = function
    | No_sharp s ->
      (match dot_split s with
        | [base;fn] -> ((base, None), fn)
        | _ -> Error.build "The identifier '%s' must be a qualified feature name (with one '.' symbol)" s
      )
    | Sharp (base, s) ->
      (match dot_split s with
        | [ext;fn] -> ((get_single base, Some ext), fn)
        | _ -> Error.build "The identifier '%s' must be a qualified feature name (with one '.' symbol)" s
      )

  type feature_spec = 
    | Closed of feature_name * feature_atom list (* cat:V,N *)
    | Open of feature_name (* phon, lemma, ... *)
    | Int of feature_name (* position *)

  type domain = feature_spec list

  let is_defined feature_name domain =
    List.exists (function
      | Closed (fn,_) when fn = feature_name -> true
      | Open fn when fn = feature_name -> true
      | Int fn when fn = feature_name -> true
      | _ -> false
    ) domain

  let rec normalize_domain = function
    | [] -> [Int "position"]
    | (Int "position") :: tail -> Log.warning "[Domain] declaration of the feature name \"position\" in useless"; normalize_domain tail
    | (Open "position") :: _
    | (Closed ("position",_)) :: _ ->
      Error.build "[Domain] The feature named \"position\" is reserved and must be types 'integer', you cannot not redefine it"
    | (Int fn) :: tail |  (Open fn) :: tail |  Closed (fn,_) :: tail when is_defined fn tail ->
      Error.build "[Domain] The feature named \"%s\" is defined several times" fn
    | x :: tail -> x :: (normalize_domain tail)

  type feature_kind = 
    | Equality of feature_value list
    | Disequality of feature_value list
    | Param of string (* $ident *)
    | Absent

  type u_feature = {
    name: feature_name;
    kind: feature_kind;
  }
  type feature = u_feature * Loc.t

  type u_node = {
      node_id: Id.name;
      position: int option;
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
    | Feature_eq of simple_qfn * simple_qfn
    | Feature_diseq of simple_qfn * simple_qfn
    | Feature_ineq of ineq * simple_qfn * simple_qfn
  type const = u_const * Loc.t

  type pattern = {
      pat_nodes: node list;
      pat_edges: edge list;
      pat_const: const list;
    }

  type graph = {
      nodes: (Id.name * node) list;
      edge: edge list;
    }

  type concat_item =
    | Qfn_item of complex_id (* Warning: either a simple string (without .) of a real qualified feature_name *)
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

  (* the [rule] type is used for 3 kids of module items:
     - rule     { param=None; ... }
     - lex_rule
     - filter   { param=None; commands=[]; ... }
   *)
  type rule = {
      rule_id:Id.name;
      pos_pattern: pattern;
      neg_patterns: pattern list;
      commands: command list;
      param: (string list * string list) option;
      lp: string list option;
      rule_doc:string list;
      rule_loc: Loc.t;
    }

  type modul = {
      module_id:Id.name;
      local_labels: (string * string list) list;
      new_node_names: string list;
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
        
(** 
    a GRS: graph rewriting system 
 *)
  type module_or_include = 
    | Modul of modul
    | Includ of (string * Loc.t)

  type grs_with_include = {
      domain_wi: domain;
      labels_wi: (string * string list) list;    (* the list of global edge labels *)
      modules_wi: module_or_include list; 
      sequences_wi: sequence list;
    }

  type grs = {
      domain: domain;
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
