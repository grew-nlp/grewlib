open Printf

open Log
open Utils

 type feature_spec = 
  | Closed of string * string list (* (the name, the set of atomic values) *)
  | Open of string (* the name *)
 
type domain = feature_spec list
 
type feature_kind = Equality | Disequality


type u_feature = {
    kind: feature_kind;
    name: string;
    values: string list;
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
   | No_out of Id.name
   | End of Id.name * string list (* (target, labels) *)
   | No_in of Id.name
   | Feature_eq of qfn * qfn

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
  | Qfn_item of (string * string)
  | String_item of string

type u_command = 
  | Del_edge_expl of (Id.name * Id.name * string)
  | Del_edge_name of string
  | Add_edge of (Id.name * Id.name * string)
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
    rule_doc:string;
    rule_loc: Loc.t;
  }
 
type modul = {
    module_id:Id.name;
    local_labels: (string * string option) list;
    bad_labels: string list;
    rules: rule list;
    confluent: bool;
    module_doc:string;
    mod_loc:Loc.t;
  }
 
type sequence = {
    seq_name:string;
    seq_mod:string list;
    seq_doc:string;
    seq_loc:Loc.t;
  }
      
(** 
    a GRS: graph rewriting system 
*)
type module_or_include = 
  | Modul of modul
  | Includ of string

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
 
module AST_HTML = struct
  
  let feat_values_tab_to_html = List_.to_string (fun x->x) " | " 

  let string_of_concat_item = function
    | Qfn_item (n,f) -> sprintf "%s.%s" n f 
    | String_item s -> sprintf "\"%s\"" s
 	
  let string_of_qfn (node, feat_name) = sprintf "%s.%s" node feat_name

  let buff_html_command ?(li_html=false) buff (u_command,_) =
    bprintf buff "      ";
    if li_html then bprintf buff "<li>";
    (match u_command with
    | Del_edge_expl (n1,n2,label) -> bprintf buff "del_edge %s -[%s]-> %s" n1 label n2
    | Del_edge_name name -> bprintf buff "del_edge %s" name
    | Add_edge (n1,n2,label) -> bprintf buff "add_edge %s -[%s]-> %s" n1 label n2
    | Shift_edge (n1,n2) -> bprintf buff "shift %s ==> %s" n1 n2
    | Merge_node (n1,n2) -> bprintf buff "merge %s ==> %s" n1 n2
    | New_neighbour (n1,n2,label) -> bprintf buff "add_node %s: <-[%s]- %s \n" n1 label n2
    | Del_node n -> bprintf buff "del_node %s" n
    | Update_feat (qfn,item_list) -> bprintf buff "%s = %s" (string_of_qfn qfn) (List_.to_string string_of_concat_item " + " item_list)
    | Del_feat qfn -> bprintf buff "del_feat %s" (string_of_qfn qfn));
    if li_html then bprintf buff "</li>\n" else bprintf buff ";\n"

  let to_html_commands_pretty = function
    | [] -> ""
    | commands ->
        let buff = Buffer.create 32 in
        bprintf buff "<ul>\n";
        List.iter (buff_html_command ~li_html:true buff) commands;
        bprintf buff "</ul>\n";
        Buffer.contents buff
    		
  let buff_html_feature buff (u_feature,_) =
    bprintf buff "%s %s %s" 
      u_feature.name 
      (match u_feature.kind with Equality -> "=" | Disequality -> "<>")
      (List_.to_string (fun x->x) ", " u_feature.values)

  let buff_html_node buff (u_node,_) =
    bprintf buff "      %s [" u_node.node_id;
    List.iter (buff_html_feature buff) u_node.fs;
    bprintf buff "];\n"

  let buff_html_edge buff (u_edge,_) =
    bprintf buff "      ";
    bprintf buff "%s" (match u_edge.edge_id with Some n -> n^": " | None -> "");
    bprintf buff "%s -[%s%s]-> %s;\n" 
      u_edge.src 
      (if u_edge.negative then "^" else "")
      (List_.to_string (fun x->x) "|" u_edge.edge_labels)
      u_edge.tar
	
  let buff_html_const buff (u_const,_) =
    bprintf buff "      ";
    (match u_const with
    | Start (id,labels) -> bprintf buff "%s -[%s]-> *" id (List_.to_string (fun x->x) "|" labels)
    | No_out id -> bprintf buff "%s -> *" id
    | End (id,labels) -> bprintf buff "* -[%s]-> %s" (List_.to_string (fun x->x) "|" labels) id
    | No_in id -> bprintf buff "* -> %s" id
    | Feature_eq (qfn_l, qfn_r) -> bprintf buff "%s = %s" (string_of_qfn qfn_l) (string_of_qfn qfn_r));
    bprintf buff "\n"

   
  let buff_html_pos_pattern buff pos_pattern =
    bprintf buff "   <font color=\"purple\">match</font> <b>{</b>\n";
    List.iter (buff_html_node buff) pos_pattern.pat_nodes;
    List.iter (buff_html_edge buff) pos_pattern.pat_edges;
    List.iter (buff_html_const buff) pos_pattern.pat_const;
    bprintf buff "    <b>}</b>\n"
       
  let buff_html_neg_pattern buff neg_pattern =
    bprintf buff "    <font color=\"purple\">without</font> <b>{</b>\n";
    List.iter (buff_html_node buff) neg_pattern.pat_nodes;
    List.iter (buff_html_edge buff) neg_pattern.pat_edges;
    List.iter (buff_html_const buff) neg_pattern.pat_const;
    bprintf buff "    <b>}</b>\n"

  let to_html_rules rules =
    let buff = Buffer.create 32 in
    List.iter 
      (fun rule ->
        bprintf buff "  <font color=\"purple\">rule</font> %s <b>{</b>\n" rule.rule_id;

        (* the match part *)
        buff_html_pos_pattern buff rule.pos_pattern;

        (* the without parts *)
        List.iter (buff_html_neg_pattern buff) rule.neg_patterns;

        (*  the commands part *)
        bprintf buff "        <font color=\"purple\">commands</font> <b>{</b>\n";
        List.iter (buff_html_command buff) rule.commands;
        bprintf buff "        <b>}</b>\n"; 

        bprintf buff "    <b>}</b>\n"; 
      ) rules;
    Buffer.contents buff
end

