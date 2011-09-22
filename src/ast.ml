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
   | End of Id.name * string list (* (target, labels) *)

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
    
type command = 
  | Del_edge_expl of (Id.name * Id.name * string)
  | Del_edge_name of string
  | Add_edge of (Id.name * Id.name * string)
  | Shift_edge of (Id.name*Id.name)
  | Merge_node of (Id.name*Id.name)
  | New_neighbour of (Id.name * Id.name * string)
  | Del_node of Id.name

  | New_feat of string * string 
  | Copy_feat of string * string
  | Del_feat of string
 
 
type rule = {
    rule_id:Id.name;
    pos_pattern: pattern;
    neg_patterns: pattern list;
    commands: (command * Loc.t) list;
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


	let accleft = "<b>{</b>"
	let accright = "<b>}</b>"

	let rec tab_to_html tab = match tab with
		| [] -> ""
		| h::[] -> h
		| h::t -> h^", "^(tab_to_html t)
	
	let rec tab_to_html_semic tab = match tab with
		| [] -> ""
		| h::[] -> h
		| h::t -> h^"; "^(tab_to_html_semic t)
		
	let rec tab_to_html_pipe tab = match tab with
		| [] -> ""
		| h::[] -> h
		| h::t -> h^"| "^(tab_to_html_pipe t)

	let rec tab_to_html_arrow tab = match tab with
		| [] -> ""
		| h::[] -> h
		| h::t -> h^" ⇨ "^(tab_to_html_arrow t)
	
	let rec feat_values_tab_to_html tab = match tab with
		| [] -> ""
		| h::[] -> h
		| h::t -> h^" | "^(feat_values_tab_to_html t)
	
	

	let rec feat_values_tab_to_dot tab = match tab with
		| [] -> ""
		| h::[] -> h
		| h::t -> h^" \\| "^(feat_values_tab_to_dot t)

	let rec feat_tab_to_html_newline tab = match tab with
		| [] -> ""
		| (equal,h1,h2,_,_)::[] -> h1^(if equal then " = " else " <> ")^(feat_values_tab_to_dot h2)
		| (equal,h1,h2,_,_)::t -> h1^(if equal then " = " else " <> ")^(feat_values_tab_to_dot h2)^"\\n"^(feat_tab_to_html_newline t)




	let to_html_domain domain = 
		"features {\n"^
		(let rec compute tab = match tab with
			| [] -> ""
			| h::[] -> begin match h with Open a -> "\t"^a^" : *\n" | Closed (name,values)  -> "\t"^name^" : "^(tab_to_html values)^"\n"; end;
			| h::t -> begin match h with Open a -> "\t"^a^" : * ;\n"^compute t | Closed (name,values)  -> "\t"^name^" : "^(tab_to_html values)^" ;\n"^compute t; end; 
		in compute domain)^
		"}\n"


	let to_html_labels ?(tab="") labels =
		if (List.length labels <> 0) then
			tab^"labels { "^(tab_to_html (List.map fst labels))^" }\n"
		else ""

	let to_html_bad_labels labels = 
		if (List.length labels <> 0) then
			"\tbad_labels { "^(tab_to_html labels)^" }\n"
		else ""

	let to_html_commands commands = 
		if (List.length commands > 0) then (
			let tmp = 
			(let rec compute tab = match tab with
				| [] -> ""
				| (Del_edge_expl (n1,n2,label))::t ->
					"            del_edge "^n1^" -["^label^"]-> "^n2^" ;\n"^
					compute t
				| (Del_edge_name name)::t ->
					"            del_edge "^name^" ;\n"^
					compute t
				| (Add_edge (n1,n2,label))::t ->
					"            add_edge "^n1^" -["^label^"]-> "^n2^" ;\n"^
					compute t
				| (Shift_edge (n1,n2))::t ->
					"            shift "^n1^" ==> "^n2^" ;\n"^
					compute t
				| (Merge_node (n1,n2))::t ->
					"            merge "^n1^" ==> "^n2^" ;\n"^
					compute t
				| (New_neighbour (n1,n2,label))::t -> 
					"            add_node "^n1^": <-["^label^"]- "^n2^" ;\n"^
					compute t
				| (Del_node n)::t ->
					"            del_node "^n^" ;\n"^
					compute t
				| (New_feat (feat,value))::t ->
					"            "^feat^" = "^value^" ;\n"^
					compute t
				| (Copy_feat (f1,f2))::t ->
					"            "^f1^"="^f2^" ;\n"^
					compute t
				| (Del_feat (feat))::t ->
					"            del_feat "^feat^" ;\n"^
					compute t
	  		in compute commands)
			 in
			String.fill tmp (String.rindex tmp ';') 1 ' ';
			tmp
		) else (
			""
		)
		
	let to_html_commands_pretty commands = 
		if (List.length commands > 0) then (
			let tmp = "<ul>"^
			(let rec compute tab = match tab with
				| [] -> "</ul>"
				| (Del_edge_expl (n1,n2,label))::t ->
					"<li>del_edge "^n1^" -["^label^"]-> "^n2^" ;</li>"^
					compute t
				| (Del_edge_name name)::t ->
					"<li>del_edge "^name^" ;\n"^
					compute t
				| (Add_edge (n1,n2,label))::t ->
					"<li>add_edge "^n1^" -["^label^"]-> "^n2^" ;</li>"^
					compute t
				| (Shift_edge (n1,n2))::t ->
					"<li>shift "^n1^" ==> "^n2^" ;</li>"^
					compute t
				| (Merge_node (n1,n2))::t ->
					"<li>merge "^n1^" ==> "^n2^" ;</li>"^
					compute t
				| (New_neighbour (n1,n2,label))::t -> 
					"<li>add_node "^n1^": <-["^label^"]- "^n2^" ;</li>"^
					compute t
				| (Del_node n)::t ->
					"<li>del_node "^n^" ;\n"^
					compute t
				| (New_feat (feat,value))::t ->
					"<li>"^feat^" = "^value^" ;</li>"^
					compute t
				| (Copy_feat (f1,f2))::t ->
					"<li>"^f1^" = "^f2^" ;</li>"^
					compute t
				| (Del_feat (feat))::t ->
					"<li>del_feat "^feat^" ;</li>"^
					compute t
	  		in compute commands)
			 in
			String.fill tmp (String.rindex tmp ';') 1 ' ';
			tmp
		) else (
			""
		)
		
	let rec feat_tab_to_html tab = match tab with
		| [] -> ""
		| (u_f,loc)::[] -> 
			u_f.name^(match u_f.kind with Equality -> " = "^(tab_to_html u_f.values) | _ -> " = * ")
		| (u_f,loc)::t -> 
			u_f.name^(match u_f.kind with Equality -> " = "^(tab_to_html u_f.values) | _ -> " = *")^"; "^(feat_tab_to_html t)

	let rec to_html_nodes nodes = match nodes with
		| [] -> ""
		| (u_n,loc)::t -> 
			"            "^u_n.node_id^" [ "^(feat_tab_to_html u_n.fs)^" ] ;\n"^(to_html_nodes t)

  let rec to_html_edges edges = 
    match edges with
    | [] -> ""
    | (u_e,loc)::t ->
	"            " ^
	(match u_e.edge_id with Some n -> n^": " | None -> "") ^
	u_e.src ^ 
	(if u_e.negative then " <-[" else " -[") ^
	(tab_to_html_pipe u_e.edge_labels) ^
	(if u_e.negative then "]- " else "]-> ") ^ 
	u_e.tar ^
	";\n" ^ 
	(to_html_edges t)
	
	let pat_const_to_string pc = 
		match pc with
			| Start (id,labels) -> "            "^id^" -["^(tab_to_html_pipe labels)^"]-> *\n"
			| End (id,labels) -> "            * -["^(tab_to_html_pipe labels)^"]-> "^id^"\n"
	
	let rec to_html_const pat_const = 
		match pat_const with
			| [] -> ""
			| (h,_)::t -> (pat_const_to_string h)^(to_html_const t)
	

	let rec to_html_neg_pattern pos_pattern = 
		match pos_pattern with
			| [] -> ""
			| h::t -> 
				"        <font color=\"purple\">without</font> "^accleft^"\n"^
				(to_html_nodes h.pat_nodes)^
				(to_html_edges h.pat_edges)^
				(to_html_const h.pat_const)^
				"        "^accright^"\n"^
				(to_html_neg_pattern t)

	let to_html_pos_pattern pos_pattern = 
		(to_html_nodes pos_pattern.pat_nodes)^
		(to_html_edges pos_pattern.pat_edges)^(if pos_pattern.pat_edges <> [] then "" else "\n")

	let rec to_html_rules rules = 
		 match rules with 
		 	| [] -> "" 
		 	| h::t -> 
		 		"    <font color=\"purple\">rule</font> "^h.rule_id^" "^accleft^"\n"^ 
		 		"        <font color=\"purple\">match</font> "^accleft^"\n"^
		 		(to_html_pos_pattern h.pos_pattern)^ 
		 		"        "^accright^"\n"^
		 		(to_html_neg_pattern h.neg_patterns)^ 
		 		"        <font color=\"purple\">commands</font> "^accleft^"\n"^
		 		(to_html_commands (List.map fst h.commands))^
		 		"        "^accright^"\n"^ 
		 		"    "^accright^"\n"^ 
		 		to_html_rules t 


end

