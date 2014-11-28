(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2013 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: http://grew.loria.fr                                               *)
(*    License: CeCILL (see LICENSE folder or "http://www.cecill.info")            *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

%{
open Grew_base
open Grew_types
open Grew_ast

(* Some intermediate sum types used in sub-functions when building the ast *)
type pat_item =
  | Pat_node of Ast.node
  | Pat_edge of Ast.edge
  | Pat_const of Ast.const

type graph_item =
  | Graph_meta of (string * string)
  | Graph_node of Ast.node
  | Graph_edge of Ast.edge

let get_loc () = (!Parser_global.current_file,!Parser_global.current_line+1)
let localize t = (t,get_loc ())
%}

%token LACC                        /* { */
%token RACC                        /* } */
%token LBRACKET                    /* [ */
%token RBRACKET                    /* ] */
%token LPAREN                      /* ( */
%token RPAREN                      /* ) */
%token DDOT                        /* : */
%token COMA                        /* , */
%token SEMIC                       /* ; */
%token STAR                        /* * */
%token SHARP                       /* # */
%token PLUS                        /* + */
%token EQUAL                       /* = */
%token DISEQUAL                    /* <> */
%token BANG                        /* ! */

%token LT                          /* < */
%token GT                          /* > */
%token LE                          /* <= or ≤ */
%token GE                          /* >= or ≥ */

%token PIPE                        /* | */
%token GOTO_NODE                   /* -> */
%token LTR_EDGE_LEFT               /* -[ */
%token LTR_EDGE_LEFT_NEG           /* -[^ */
%token LTR_EDGE_RIGHT              /* ]-> */
%token RTL_EDGE_LEFT               /* <-[ */
%token RTL_EDGE_RIGHT              /* ]- */
%token LONGARROW                   /* ==> */

%token INCLUDE                     /* include */
%token FEATURES                    /* features */
%token FEATURE                     /* feature */
%token FILE                        /* file */
%token LABELS                      /* labels */
%token SUFFIXES                    /* suffixes */
%token ACTIVATE                    /* activate */
%token MATCH                       /* match */
%token WITHOUT                     /* without */
%token COMMANDS                    /* commands */
%token MODULE                      /* module */
%token CONFLUENT                   /* confluent */
%token RULE                        /* rule */
%token LEX_RULE                    /* lex_rule */
%token FILTER                      /* filter */
%token SEQUENCES                   /* sequences */
%token GRAPH                       /* graph */

%token DEL_EDGE                    /* del_edge */
%token ADD_EDGE                    /* add_edge */
%token MERGE                       /* merge */
%token SHIFT_IN                    /* shift_in */
%token SHIFT_OUT                   /* shift_out */
%token SHIFT                       /* shift */
%token DEL_NODE                    /* del_node */
%token ADD_NODE                    /* add_node */
%token DEL_FEAT                    /* del_feat */

%token <string> DOLLAR_ID          /* $id */
%token <string> AROBAS_ID          /* @id */
%token <string> COLOR              /* @#89abCD */

%token <Grew_ast.Ast.complex_id>   COMPLEX_ID

%token <string>           STRING
%token <float>            FLOAT
%token <string list>      COMMENT
%token <string list>      LP

%token EOF                         /* end of file */

%start <Grew_ast.Ast.grs_with_include> grs_with_include
%start <Grew_ast.Ast.grs> grs
%start <Grew_ast.Ast.gr> gr
%start <Grew_ast.Ast.module_or_include list> included
%start <Grew_ast.Ast.isolated_pattern> isolated_pattern
%%


%public separated_list_final_opt(separator,X):
|                                                               { [] }
|   x=X                                                         { [x] }
|   x=X; separator; xs=separated_list_final_opt(separator,X)    { x :: xs }

%public separated_nonempty_list_final_opt(separator,X):
|   x=X                                                                  { [x] }
|   x=X; separator                                                       { [x] }
|   x=X; separator; xs=separated_nonempty_list_final_opt(separator,X)    { x :: xs }

/*=============================================================================================*/
/*  BASIC DEFINITIONS                                                                          */
/*=============================================================================================*/
string_or_int:
        | v=COMPLEX_ID    { Ast.simple_id_of_ci v }
        | v=STRING        { v }
        | v=FLOAT         { Printf.sprintf "%g" v }

label_ident:
        | x=separated_nonempty_list(DDOT,COMPLEX_ID)   { String.concat ":" (List.map Ast.label_id_of_ci x) }

simple_id_with_loc:
        | id=COMPLEX_ID { (Ast.simple_id_of_ci id,!Parser_global.current_line+1) }

num:
        | FLOAT       { $1 }

/*=============================================================================================*/
/*  GREW GRAPH                                                                                 */
/*=============================================================================================*/
gr:
        | GRAPH LACC items=separated_list_final_opt(SEMIC,gr_item) RACC EOF
            {
             {
              Ast.meta = List_.opt_map (function Graph_meta n -> Some n | _ -> None) items;
              Ast.nodes = List_.opt_map (function Graph_node n -> Some n | _ -> None) items;
              Ast.edges = List_.opt_map (function Graph_edge n -> Some n | _ -> None) items;
            }
           }

gr_item:
        (* sentence = "Jean dort." *)
        | id=COMPLEX_ID EQUAL value=string_or_int
            { Graph_meta (Ast.simple_id_of_ci id, value) }

        (* B (1) [phon="pense", lemma="penser", cat=v, mood=ind ] *)
        (* B [phon="pense", lemma="penser", cat=v, mood=ind ] *)
        | id=COMPLEX_ID position=option(delimited(LPAREN,num,RPAREN)) feats=delimited(LBRACKET,separated_list_final_opt(COMA,node_features),RBRACKET)
            { Graph_node (localize {Ast.node_id = Ast.simple_id_of_ci id; position=position; fs=feats}) }

        (* A -[x|y|z]-> B*)
        | n1=COMPLEX_ID labels=delimited(LTR_EDGE_LEFT,separated_nonempty_list(PIPE,label_ident),LTR_EDGE_RIGHT) n2=COMPLEX_ID
            { Graph_edge (localize {Ast.edge_id = None; src=Ast.simple_id_of_ci n1; edge_labels=labels; tar=Ast.simple_id_of_ci n2; negative=false; }) }

/*=============================================================================================*/
/*  GREW GRAPH SYSTEM REWRITING                                                                */
/*=============================================================================================*/
grs_with_include:
        | f=features_group g=labels m=module_or_include_list s=sequences EOF
            {
             { Ast.domain_wi=f;
               labels_wi=g;
               modules_wi=m;
               sequences_wi=s;
             }
           }

grs:
        | f=features_group g=labels m=modules s=sequences EOF
            {
             { Ast.domain=f;
               labels=g;
               modules=m;
               sequences=s;
             }
           }

module_or_include_list:
        | x=list(module_or_include) { x }

module_or_include:
        | m=grew_module             { Ast.Modul m }
        | INCLUDE sub=subfile SEMIC { Ast.Includ sub }

subfile:
        | f=STRING  { localize f }

/*=============================================================================================*/
/* FEATURES DOMAIN DEFINITION                                                                  */
/*=============================================================================================*/
features_group:
        | FEATURES x=features { x }

%inline features:
        | LACC x=separated_nonempty_list_final_opt(SEMIC,feature) RACC { x }

%inline feature:
        (* phon:* *)
        (* pos=# *)
        (* m: ind,inf,part,subj,imp *)
        | feature_name=feature_name DDOT feature_values=features_values
            {
              match feature_values with
                | ["*"] -> Domain.Open feature_name
                | ["#"] -> Domain.Num feature_name
                | _ -> Domain.Closed (feature_name, List.sort Pervasives.compare feature_values)
            }

feature_name:
        | ci=COMPLEX_ID { Ast.simple_id_of_ci ci }

feature_value:
        | v=string_or_int  { v }

features_values:
        | STAR                                          { ["*"] }
        | SHARP                                         { ["#"] }
        | x=separated_nonempty_list(COMA,feature_value) { x }


/*=============================================================================================*/
/* GLOBAL LABELS DEFINITION                                                                    */
/*=============================================================================================*/
%inline labels:
        (* labels { OBJ, SUBJ, DE_OBJ, ANT } *)
        | LABELS x=delimited(LACC,separated_nonempty_list_final_opt(COMA,label),RACC) { x }

%inline label:
        | x=label_ident display_list=list(display)  { (x, display_list) }

display:
        | dis=AROBAS_ID   { dis }
        | col=COLOR       { col }

/*=============================================================================================*/
/* MODULE DEFINITION                                                                           */
/*=============================================================================================*/
included:
        | x=list(module_or_include) EOF { x }

modules:
        | x=list(grew_module) { x }

grew_module:
        | doc=option(COMMENT) MODULE conf=boption(CONFLUENT) id_loc=simple_id_with_loc LACC l=option(labels) suff=option(suffixes) r=rules RACC
           {
            { Ast.module_id = fst id_loc;
              local_labels = (match l with None -> [] | Some x -> x);
              suffixes = (match suff with None -> [] | Some x -> x);
              rules = r;
              confluent = conf;
              module_doc = (match doc with Some d -> d | None -> []);
              mod_loc = (!Parser_global.current_file, snd id_loc);
              mod_dir = "";
            }
          }

suffixes:
        (* "suffixes {a, b, c}" *)
        | SUFFIXES x=delimited(LACC,separated_nonempty_list_final_opt(COMA,COMPLEX_ID),RACC)
            { List.map Ast.simple_id_of_ci x }


/*=============================================================================================*/
/* RULES DEFINITION                                                                            */
/*=============================================================================================*/
rules:
        | r = list(rule) { r }

rule:
        | doc=option(COMMENT) RULE id_loc=simple_id_with_loc LACC p=pos_item n=list(neg_item) cmds=commands RACC
            {
              { Ast.rule_id = fst id_loc;
                pos_basic = p;
                neg_basics = n;
                commands = cmds;
                param = None;
                lp = None;
                rule_doc = begin match doc with Some d -> d | None -> [] end;
                rule_loc = (!Parser_global.current_file,snd id_loc);
              }
            }
        | doc=option(COMMENT) LEX_RULE id_loc=simple_id_with_loc param=option(param) LACC p=pos_item n=list(neg_item) cmds=commands RACC lp=option(lp)
            {
              { Ast.rule_id = fst id_loc;
                pos_basic = p;
                neg_basics = n;
                commands = cmds;
                param = param;
                lp = lp;
                rule_doc = begin match doc with Some d -> d | None -> [] end;
                rule_loc = (!Parser_global.current_file,snd id_loc);
              }
            }
        | doc=option(COMMENT) FILTER id_loc=simple_id_with_loc LACC p=pos_item n=list(neg_item) RACC
            {
              { Ast.rule_id = fst id_loc;
                pos_basic = p;
                neg_basics = n;
                commands = [];
                param = None;
                lp = None;
                rule_doc = begin match doc with Some d -> d | None -> [] end;
                rule_loc = (!Parser_global.current_file,snd id_loc);
              }
            }

lp:
        | lp = LP  { lp }

param:
        | LPAREN FEATURE vars=separated_nonempty_list(COMA,var) RPAREN                                       { ([],vars) }
        | LPAREN FEATURE vars=separated_nonempty_list(COMA,var) SEMIC files=separated_list(COMA,file) RPAREN { (files,vars) }

file:
        | FILE f=STRING     { f }
var:
        | i=DOLLAR_ID       { i }
        | i=AROBAS_ID       { i }

pos_item:
        | MATCH i=pn_item   { i }

neg_item:
        | WITHOUT i=pn_item { i }

pn_item:
        | l=delimited(LACC,separated_list_final_opt(SEMIC,pat_item),RACC)
            {
             {
              Ast.pat_nodes = List_.opt_map (function Pat_node n -> Some n | _ -> None) l;
              Ast.pat_edges = List_.opt_map (function Pat_edge n -> Some n | _ -> None) l;
              Ast.pat_const = List_.opt_map (function Pat_const n -> Some n | _ -> None) l;
            }
           }

/*=============================================================================================*/
/* MATCH DEFINITION                                                                            */
/*=============================================================================================*/
pat_item:
        | n=pat_node  { Pat_node n }
        | e=pat_edge  { Pat_edge e }
        | c=pat_const { Pat_const c }

pat_node:
        (* R [cat=V, lemma=$lemma ] *)
        | id=COMPLEX_ID feats=delimited(LBRACKET,separated_list_final_opt(COMA,node_features),RBRACKET)
            { localize ({Ast.node_id = Ast.simple_id_of_ci id; position=None; fs= feats}) }

node_features:
        (* cat=* *)
        | name=COMPLEX_ID EQUAL STAR
            { localize {Ast.kind = Ast.Disequality []; name=Ast.simple_id_of_ci name; } }

        (* cat=n|v|adj *)
        | name=COMPLEX_ID EQUAL values=separated_nonempty_list(PIPE,feature_value)
            { localize {Ast.kind = Ast.Equality values; name=Ast.simple_id_of_ci name; } }

        (* cat<>n|v|adj *)
        | name=COMPLEX_ID DISEQUAL values=separated_nonempty_list(PIPE,feature_value)
            { localize {Ast.kind = Ast.Disequality values; name=Ast.simple_id_of_ci name; } }

        (* lemma=$lem *)
        | name=COMPLEX_ID EQUAL p=DOLLAR_ID
            { localize {Ast.kind = Ast.Param p; name=Ast.simple_id_of_ci name; } }

        (* !lemma *)
        | BANG name=COMPLEX_ID
            { localize {Ast.kind = Ast.Absent; name=Ast.simple_id_of_ci name; } }

pat_edge:
        (* "e: A -> B" OR "e: A -[*]-> B" *)
        | id=COMPLEX_ID DDOT n1=COMPLEX_ID GOTO_NODE n2=COMPLEX_ID
        | id=COMPLEX_ID DDOT n1=COMPLEX_ID LTR_EDGE_LEFT STAR LTR_EDGE_RIGHT n2=COMPLEX_ID
               { localize ({Ast.edge_id = Some (Ast.simple_id_of_ci id); src=Ast.simple_id_of_ci n1; edge_labels=[]; tar=Ast.simple_id_of_ci n2; negative=true}) }

        (* "A -> B" *)
        | n1=COMPLEX_ID GOTO_NODE n2=COMPLEX_ID
               { localize ({Ast.edge_id = None; src=Ast.simple_id_of_ci n1; edge_labels=[]; tar=Ast.simple_id_of_ci n2; negative=true}) }

        (* "e: A -[^X|Y]-> B" *)
        | id=COMPLEX_ID DDOT n1=COMPLEX_ID labels=delimited(LTR_EDGE_LEFT_NEG,separated_nonempty_list(PIPE,label_ident),LTR_EDGE_RIGHT) n2=COMPLEX_ID
            { localize ({Ast.edge_id = Some (Ast.simple_id_of_ci id); src=Ast.simple_id_of_ci n1; edge_labels=labels; tar=Ast.simple_id_of_ci n2; negative=true}) }

        (* "A -[^X|Y]-> B"*)
        | n1=COMPLEX_ID labels=delimited(LTR_EDGE_LEFT_NEG,separated_nonempty_list(PIPE,label_ident),LTR_EDGE_RIGHT) n2=COMPLEX_ID
            { localize ({Ast.edge_id = None; src=Ast.simple_id_of_ci n1; edge_labels=labels; tar=Ast.simple_id_of_ci n2; negative=true}) }

        (* "e: A -[X|Y]-> B" *)
        | id=COMPLEX_ID DDOT n1=COMPLEX_ID labels=delimited(LTR_EDGE_LEFT,separated_nonempty_list(PIPE,label_ident),LTR_EDGE_RIGHT) n2=COMPLEX_ID
            { localize ({Ast.edge_id = Some (Ast.simple_id_of_ci id); src=Ast.simple_id_of_ci n1; edge_labels=labels; tar=Ast.simple_id_of_ci n2; negative=false}) }

        (* "A -[X|Y]-> B" *)
        | n1=COMPLEX_ID labels=delimited(LTR_EDGE_LEFT,separated_nonempty_list(PIPE,label_ident),LTR_EDGE_RIGHT) n2=COMPLEX_ID
            { localize ({Ast.edge_id = None; src=Ast.simple_id_of_ci n1; edge_labels=labels; tar=Ast.simple_id_of_ci n2; negative=false}) }

pat_const:
        (* "A -[X|Y]-> *" *)
        | n=COMPLEX_ID labels=delimited(LTR_EDGE_LEFT,separated_nonempty_list(PIPE,label_ident),LTR_EDGE_RIGHT) STAR
            { localize (Ast.Start (Ast.simple_id_of_ci n,labels)) }

        (* "A -> *" *)
        | n=COMPLEX_ID GOTO_NODE STAR
            { localize (Ast.Cst_out (Ast.simple_id_of_ci n)) }

        (* "* -[X|Y]-> A" *)
        | STAR labels=delimited(LTR_EDGE_LEFT,separated_nonempty_list(PIPE,label_ident),LTR_EDGE_RIGHT) n=COMPLEX_ID
            { localize (Ast.End (Ast.simple_id_of_ci n,labels)) }

        (* "* -> A" *)
        | STAR GOTO_NODE n=COMPLEX_ID
            { localize (Ast.Cst_in (Ast.simple_id_of_ci n)) }

        (* X.cat = Y.cat *)
        | qfn1=COMPLEX_ID EQUAL qfn2=COMPLEX_ID
            { localize (Ast.Feature_eq (Ast.simple_qfn_of_ci qfn1, Ast.simple_qfn_of_ci qfn2)) }

        (* X.position < Y.position *)
        | qfn1=COMPLEX_ID LT qfn2=COMPLEX_ID
            { localize (Ast.Feature_ineq (Ast.Lt, Ast.simple_qfn_of_ci qfn1, Ast.simple_qfn_of_ci qfn2)) }

        (* X.position > Y.position *)
        | qfn1=COMPLEX_ID GT qfn2=COMPLEX_ID
            { localize (Ast.Feature_ineq (Ast.Gt, Ast.simple_qfn_of_ci qfn1, Ast.simple_qfn_of_ci qfn2)) }

        (* X.position <= Y.position *)
        | qfn1=COMPLEX_ID LE qfn2=COMPLEX_ID
            { localize (Ast.Feature_ineq (Ast.Le, Ast.simple_qfn_of_ci qfn1, Ast.simple_qfn_of_ci qfn2)) }

        (* X.position >= Y.position *)
        | qfn1=COMPLEX_ID GE qfn2=COMPLEX_ID
            { localize (Ast.Feature_ineq (Ast.Ge, Ast.simple_qfn_of_ci qfn1, Ast.simple_qfn_of_ci qfn2)) }


/*=============================================================================================*/
/* COMMANDS DEFINITION                                                                         */
/*=============================================================================================*/
commands:
        | COMMANDS x=delimited(LACC,separated_nonempty_list_final_opt(SEMIC,command),RACC) { x }

command:
        (* del_edge e *)
        | DEL_EDGE n=COMPLEX_ID
            { localize (Ast.Del_edge_name (Ast.simple_id_of_ci n)) }

        (* del_edge m -[x]-> n *)
        | DEL_EDGE src=COMPLEX_ID label=delimited(LTR_EDGE_LEFT,label_ident,LTR_EDGE_RIGHT) tar=COMPLEX_ID
            { localize (Ast.Del_edge_expl (Ast.act_id_of_ci src, Ast.act_id_of_ci tar, label)) }

        (* add_edge m -[x]-> n *)
        | ADD_EDGE src=COMPLEX_ID label=delimited(LTR_EDGE_LEFT,label_ident,LTR_EDGE_RIGHT) tar=COMPLEX_ID
            { localize (Ast.Add_edge (Ast.act_id_of_ci src, Ast.act_id_of_ci tar, label)) }

        (* shift_in m ==> n *)
        | SHIFT_IN src=COMPLEX_ID LONGARROW tar=COMPLEX_ID
            { localize (Ast.Shift_in (Ast.act_id_of_ci src, Ast.act_id_of_ci tar)) }

        (* shift_out m ==> n *)
        | SHIFT_OUT src=COMPLEX_ID LONGARROW tar=COMPLEX_ID
            { localize (Ast.Shift_out (Ast.act_id_of_ci src, Ast.act_id_of_ci tar)) }

        (* shift m ==> n *)
        | SHIFT src=COMPLEX_ID LONGARROW tar=COMPLEX_ID
            { localize (Ast.Shift_edge (Ast.act_id_of_ci src, Ast.act_id_of_ci tar)) }

        (* merge m ==> n *)
        | MERGE src=COMPLEX_ID LONGARROW tar=COMPLEX_ID
            { localize (Ast.Merge_node (Ast.act_id_of_ci src, Ast.act_id_of_ci tar)) }

        (* del_node n *)
        | DEL_NODE ci=COMPLEX_ID
            { localize (Ast.Del_node (Ast.act_id_of_ci ci)) }

        (* add_node n: <-[x]- m *)
        | ADD_NODE new_ci=COMPLEX_ID DDOT label=delimited(RTL_EDGE_LEFT,label_ident,RTL_EDGE_RIGHT) anc_ci=COMPLEX_ID
            { localize (Ast.New_neighbour (Ast.simple_id_of_ci new_ci, Ast.act_id_of_ci anc_ci,label)) }

        (* activate n#a *)
        | ACTIVATE ci=COMPLEX_ID
            { localize (Ast.Activate (Ast.act_id_of_ci ci)) }

        (* del_feat m.cat *)
        | DEL_FEAT qfn=COMPLEX_ID
            { localize (Ast.Del_feat (Ast.act_qfn_of_ci qfn)) }

        (* m.cat = n.x + "_" + nn.y *)
        | qfn=COMPLEX_ID EQUAL items=separated_nonempty_list (PLUS, concat_item)
            { localize (Ast.Update_feat ((Ast.act_qfn_of_ci qfn), items)) }

concat_item:
        | cid=COMPLEX_ID   { Ast.Qfn_item cid }
        | s=STRING         { Ast.String_item s }
        | f=FLOAT          { Ast.String_item (Printf.sprintf "%g" f) }
        | p=AROBAS_ID      { Ast.Param_item p }
        | p=DOLLAR_ID      { Ast.Param_item p }


/*=============================================================================================*/
/* SEQUENCE DEFINITION                                                                         */
/*=============================================================================================*/
sequences:
        | SEQUENCES seq=delimited(LACC,list(sequence),RACC) { seq }

sequence:
        (* sequence { ant; p7_to_p7p-mc} *)
        | doc=option(COMMENT) id_loc=simple_id_with_loc mod_names=delimited(LACC,separated_list_final_opt(SEMIC,COMPLEX_ID),RACC)
            {
              { Ast.seq_name = fst id_loc;
                seq_mod = List.map (fun x -> Ast.simple_id_of_ci x) mod_names ;
                seq_doc = begin match doc with Some d -> d | None -> [] end;
                seq_loc = (!Parser_global.current_file,snd id_loc);
              }
            }

/*=============================================================================================*/
/* ISOLATED PATTERN (grep mode)                                                                 */
/*=============================================================================================*/
isolated_pattern:
        | p=pos_item n=list(neg_item) { {Ast.isol_pos=p; isol_negs=n} }

%%
