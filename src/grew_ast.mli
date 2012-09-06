open Grew_utils
 
module Ast : sig
  type feature_spec = 
    | Closed of string * string list (* (the name, the set of atomic values) *)
    | Open of string (* the name *)
          
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

(* qualified feature name "A.lemma" *)
  type qfn = string * string

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

  type u_const = 
    | Start of Id.name * string list (* (source, labels) *)
    | Cst_out of Id.name
    | End of Id.name * string list (* (target, labels) *)
    | Cst_in of Id.name
    | Feature_eq of qfn * qfn

  type const = u_const * Loc.t

  type pattern = {
      pat_nodes: node list;
      pat_edges: edge list;
      pat_const: const list;
    }

  type concat_item =
    | Qfn_item of (string * string)
    | String_item of string
    | Param_item of string
          
  type u_command = 
    | Del_edge_expl of (Id.name * Id.name * string)
    | Del_edge_name of string
    | Add_edge of (Id.name * Id.name * string)
    | Shift_in of (Id.name*Id.name)
    | Shift_out of (Id.name*Id.name)
    | Shift_edge of (Id.name*Id.name)
    | Merge_node of (Id.name*Id.name)
    | New_neighbour of (Id.name * Id.name * string)
    | Del_node of Id.name

    | Del_feat of qfn
    | Update_feat of qfn * concat_item list

  type command = u_command * Loc.t

  type rule = {
      rule_id:Id.name;
      pos_pattern: pattern;
      neg_patterns: pattern list;
      commands: command list;
      param: (string list * string list) option; (* (files, vars) *)
      lp: string list option; (* lexical parameters in the file *)
      rule_doc:string list;
      rule_loc: Loc.t;
    }
        
  type modul = {
      module_id:Id.name;
      local_labels: (string * string option) list;
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
      labels_wi: (string * string option) list;    (* the list of global edge labels *)
      modules_wi: module_or_include list; 
      sequences_wi: sequence list;
    }

  type grs = {
      domain: domain;
      labels: (string * string option) list;
      modules: modul list;
      sequences: sequence list;
    }

  type gr = {
      nodes: node list;
      edges: edge list;
    }
end (* module Ast *)
