open Printf
open Log

open Grew_utils

module Ast = struct
  type feature_spec = 
    | Closed of string * string list (* (the name, the set of atomic values) *)
    | Open of string (* the name *)
    | Int of string (* the name *)
          
  type domain = feature_spec list
        
  type feature_kind = 
    | Equality of string list 
    | Disequality of string list
    | Param of string

  type u_feature = {
      name: string;
      kind: feature_kind;
    }  

  type feature = u_feature * Loc.t
        
  type u_node = {
      node_id: Id.name;
      position: int option;
      fs: feature list;
    }
  type node = u_node * Loc.t
        
  type u_edge = {
      edge_id: Id.name option;
      src: Id.name;
      edge_labels: string list;
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

  type feature_name = string

  type u_const = 
    | Start of Id.name * string list (* (source, labels) *)
    | Cst_out of Id.name
    | End of Id.name * string list (* (target, labels) *)
    | Cst_in of Id.name
    | Feature_eq of (Id.name * feature_name) * (Id.name * feature_name)
    | Feature_diseq of (Id.name * feature_name) * (Id.name * feature_name)
    | Feature_ineq of ineq * (Id.name * feature_name) * (Id.name * feature_name)

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

  (* the base node name and the eventual new_node extension *)
  type c_ident = Id.name * string option

  let c_ident_to_string (string_node, new_opt) =
    match new_opt with
      | None -> string_node
      | Some a -> sprintf "%s#%s" string_node a

  type concat_item =
    | Qfn_item of (c_ident * feature_name)
    | String_item of string
    | Param_item of string

  type u_command = 
    | Del_edge_expl of (c_ident * c_ident * string)
    | Del_edge_name of string
    | Add_edge of (c_ident * c_ident * string)
    | Shift_in of (c_ident*c_ident)
    | Shift_out of (c_ident*c_ident)
    | Shift_edge of (c_ident*c_ident)
    | Merge_node of (c_ident*c_ident)
    | New_neighbour of (c_ident * c_ident * string)
    | Del_node of c_ident
    | Activate of c_ident

    | Del_feat of (c_ident * feature_name)
    | Update_feat of (c_ident * feature_name) * concat_item list

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
end (* module Ast *)        
