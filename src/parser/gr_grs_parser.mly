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

let get_loc () = Loc.file_line !Parser_global.current_file (!Parser_global.current_line+1)
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
%token <string list>      LEX_PAR

%token EOF                         /* end of file */

%start <Grew_ast.Ast.grs_with_include> grs_with_include
%start <Grew_ast.Ast.grs> grs
%start <Grew_ast.Ast.gr> gr
%start <Grew_ast.Ast.module_or_include list> included
%start <Grew_ast.Ast.pattern> pattern
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
        | x=STRING                                     { x }

simple_id:
        | id=COMPLEX_ID { Ast.simple_id_of_ci id }

simple_id_with_loc:
        | id=COMPLEX_ID { (Ast.simple_id_of_ci id, Loc.file_line !Parser_global.current_file (!Parser_global.current_line+1)) }

complex_id_with_loc:
        | id=COMPLEX_ID { (id, Loc.file_line !Parser_global.current_file (!Parser_global.current_line+1)) }

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
        | id=simple_id EQUAL value=string_or_int
            { Graph_meta (id, value) }

        (* B (1) [phon="pense", lemma="penser", cat=v, mood=ind ] *)
        (* B [phon="pense", lemma="penser", cat=v, mood=ind ] *)
        | id_loc=simple_id_with_loc position=option(delimited(LPAREN,num,RPAREN)) feats=delimited(LBRACKET,separated_list_final_opt(COMA,node_features),RBRACKET)
            { let (id,loc) = id_loc in
              Graph_node ({Ast.node_id = id; position=position; fs=feats}, loc) }

        (* A -[x]-> B*)
        | n1_loc=simple_id_with_loc label=delimited(LTR_EDGE_LEFT,label_ident,LTR_EDGE_RIGHT) n2=simple_id
            { Graph_edge ({Ast.edge_id = None; src=fst n1_loc; edge_labels=[label]; tar=n2; negative=false }, snd n1_loc) }

/*=============================================================================================*/
/*  GREW GRAPH REWRITING SYSTEM                                                                */
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
        | ci=simple_id { ci }

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
              mod_loc = snd id_loc;
              mod_dir = "";
            }
          }

suffixes:
        (* "suffixes {a, b, c}" *)
        | SUFFIXES x=delimited(LACC,separated_nonempty_list_final_opt(COMA,simple_id),RACC)
            { x }


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
                lex_par = None;
                rule_doc = begin match doc with Some d -> d | None -> [] end;
                rule_loc = snd id_loc;
              }
            }
        | doc=option(COMMENT) LEX_RULE id_loc=simple_id_with_loc param=option(param) LACC p=pos_item n=list(neg_item) cmds=commands RACC lex_par=option(lex_par)
            {
              { Ast.rule_id = fst id_loc;
                pos_basic = p;
                neg_basics = n;
                commands = cmds;
                param = param;
                lex_par = lex_par;
                rule_doc = begin match doc with Some d -> d | None -> [] end;
                rule_loc = snd id_loc;
              }
            }
        | doc=option(COMMENT) FILTER id_loc=simple_id_with_loc LACC p=pos_item n=list(neg_item) RACC
            {
              { Ast.rule_id = fst id_loc;
                pos_basic = p;
                neg_basics = n;
                commands = [];
                param = None;
                lex_par = None;
                rule_doc = begin match doc with Some d -> d | None -> [] end;
                rule_loc = snd id_loc;
              }
            }

lex_par:
        | lex_par = LEX_PAR  { lex_par }

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
        | id_loc=simple_id_with_loc feats=delimited(LBRACKET,separated_list_final_opt(COMA,node_features),RBRACKET)
            { ({Ast.node_id = fst id_loc; position=None; fs= feats}, snd id_loc) }

node_features:
        (* cat=* *)
        | name_loc=simple_id_with_loc EQUAL STAR
            { let (name,loc) = name_loc in ({Ast.kind = Ast.Disequality []; name},loc) }

        (* cat=n|v|adj *)
        | name_loc=simple_id_with_loc EQUAL values=separated_nonempty_list(PIPE,feature_value)
            { let (name,loc) = name_loc in ({Ast.kind = Ast.Equality values; name }, loc) }

        (* cat<>n|v|adj *)
        | name_loc=simple_id_with_loc DISEQUAL values=separated_nonempty_list(PIPE,feature_value)
            { let (name,loc) = name_loc in ( {Ast.kind = Ast.Disequality values; name}, loc) }

        (* lemma=$lem *)
        | name_loc=simple_id_with_loc EQUAL p=DOLLAR_ID
            { let (name,loc) = name_loc in ( {Ast.kind = Ast.Equal_param p; name }, loc) }

        (* !lemma *)
        | BANG name_loc=simple_id_with_loc
            { let (name,loc) = name_loc in ({Ast.kind = Ast.Absent; name}, loc) }

pat_edge:
        (* "e: A -> B" OR "e: A -[*]-> B" *)
        | id_loc=simple_id_with_loc DDOT n1=simple_id GOTO_NODE n2=simple_id
        | id_loc=simple_id_with_loc DDOT n1=simple_id LTR_EDGE_LEFT STAR LTR_EDGE_RIGHT n2=simple_id
            { let (id,loc) = id_loc in ({Ast.edge_id = Some id; src=n1; edge_labels=[]; tar=n2; negative=true}, loc) }

        (* "A -> B" OR "e: A -[*]-> B" *)
        | n1_loc=simple_id_with_loc GOTO_NODE n2=simple_id
        | n1_loc=simple_id_with_loc LTR_EDGE_LEFT STAR LTR_EDGE_RIGHT n2=simple_id
            { let (n1,loc) = n1_loc in ({Ast.edge_id = None; src=n1; edge_labels=[]; tar=n2; negative=true}, loc) }

        (* "e: A -[^X|Y]-> B" *)
        | id_loc=simple_id_with_loc DDOT n1=simple_id labels=delimited(LTR_EDGE_LEFT_NEG,separated_nonempty_list(PIPE,label_ident),LTR_EDGE_RIGHT) n2=simple_id
            { let (id,loc) = id_loc in ({Ast.edge_id = Some id; src=n1; edge_labels=labels; tar=n2; negative=true}, loc) }

        (* "A -[^X|Y]-> B"*)
        | n1_loc=simple_id_with_loc labels=delimited(LTR_EDGE_LEFT_NEG,separated_nonempty_list(PIPE,label_ident),LTR_EDGE_RIGHT) n2=simple_id
            { let (n1,loc) = n1_loc in ({Ast.edge_id = None; src=n1; edge_labels=labels; tar=n2; negative=true}, loc) }

        (* "e: A -[X|Y]-> B" *)
        | id_loc=simple_id_with_loc DDOT n1=simple_id labels=delimited(LTR_EDGE_LEFT,separated_nonempty_list(PIPE,label_ident),LTR_EDGE_RIGHT) n2=simple_id
            { let (id,loc) = id_loc in ({Ast.edge_id = Some id; src=n1; edge_labels=labels; tar=n2; negative=false}, loc) }

        (* "A -[X|Y]-> B" *)
        | n1_loc=simple_id_with_loc labels=delimited(LTR_EDGE_LEFT,separated_nonempty_list(PIPE,label_ident),LTR_EDGE_RIGHT) n2=simple_id
            { let (n1,loc) = n1_loc in ({Ast.edge_id = None; src=n1; edge_labels=labels; tar=n2; negative=false}, loc) }

pat_const:
        (* "A -[X|Y]-> *" *)
        | n_loc=simple_id_with_loc labels=delimited(LTR_EDGE_LEFT,separated_nonempty_list(PIPE,label_ident),LTR_EDGE_RIGHT) STAR
            { let (n,loc) = n_loc in (Ast.Start (n,labels), loc) }

        (* "A -> *" *)
        | n_loc=simple_id_with_loc GOTO_NODE STAR
            { let (n,loc) = n_loc in (Ast.Cst_out (n), loc) }

        (* "* -[X|Y]-> A" *)
        | STAR labels=delimited(LTR_EDGE_LEFT,separated_nonempty_list(PIPE,label_ident),LTR_EDGE_RIGHT) n_loc=simple_id_with_loc
            { let (n,loc) = n_loc in (Ast.End (n,labels), loc) }

        (* "* -> A" *)
        | STAR GOTO_NODE n_loc=simple_id_with_loc
            { let (n,loc) = n_loc in (Ast.Cst_in (n), loc) }

        (* X.cat = Y.cat *)
        | qfn1_loc=complex_id_with_loc EQUAL qfn2=COMPLEX_ID
            { let (qfn1,loc)=qfn1_loc in (Ast.Feature_eq (Ast.simple_qfn_of_ci qfn1, Ast.simple_qfn_of_ci qfn2), loc) }

        (* X.position < Y.position *)
        | qfn1_loc=complex_id_with_loc LT qfn2=COMPLEX_ID
            { let (qfn1,loc)=qfn1_loc in (Ast.Feature_ineq (Ast.Lt, Ast.simple_qfn_of_ci qfn1, Ast.simple_qfn_of_ci qfn2), loc) }

        (* X.position > Y.position *)
        | qfn1_loc=complex_id_with_loc GT qfn2=COMPLEX_ID
            { let (qfn1,loc)=qfn1_loc in (Ast.Feature_ineq (Ast.Gt, Ast.simple_qfn_of_ci qfn1, Ast.simple_qfn_of_ci qfn2), loc) }

        (* X.position <= Y.position *)
        | qfn1_loc=complex_id_with_loc LE qfn2=COMPLEX_ID
            { let (qfn1,loc)=qfn1_loc in (Ast.Feature_ineq (Ast.Le, Ast.simple_qfn_of_ci qfn1, Ast.simple_qfn_of_ci qfn2), loc) }

        (* X.position >= Y.position *)
        | qfn1_loc=complex_id_with_loc GE qfn2=COMPLEX_ID
            { let (qfn1,loc)=qfn1_loc in (Ast.Feature_ineq (Ast.Ge, Ast.simple_qfn_of_ci qfn1, Ast.simple_qfn_of_ci qfn2), loc) }


/*=============================================================================================*/
/* COMMANDS DEFINITION                                                                         */
/*=============================================================================================*/
commands:
        | COMMANDS x=delimited(LACC,separated_nonempty_list_final_opt(SEMIC,command),RACC) { x }

command:
        (* del_edge e *)
        | DEL_EDGE n_loc=simple_id_with_loc
            { let (n,loc) = n_loc in (Ast.Del_edge_name n, loc) }

        (* del_edge m -[x]-> n *)
        | DEL_EDGE src_loc=complex_id_with_loc label=delimited(LTR_EDGE_LEFT,label_ident,LTR_EDGE_RIGHT) tar=COMPLEX_ID
            { let (src,loc) = src_loc in (Ast.Del_edge_expl (Ast.act_id_of_ci src, Ast.act_id_of_ci tar, label), loc) }

        (* add_edge m -[x]-> n *)
        | ADD_EDGE src_loc=complex_id_with_loc label=delimited(LTR_EDGE_LEFT,label_ident,LTR_EDGE_RIGHT) tar=COMPLEX_ID
            { let (src,loc) = src_loc in (Ast.Add_edge (Ast.act_id_of_ci src, Ast.act_id_of_ci tar, label), loc) }

        (* shift_in m ==> n *)
        | SHIFT_IN src_loc=complex_id_with_loc LONGARROW tar=COMPLEX_ID
            { let (src,loc) = src_loc in (Ast.Shift_in (Ast.act_id_of_ci src, Ast.act_id_of_ci tar), loc) }

        (* shift_out m ==> n *)
        | SHIFT_OUT src_loc=complex_id_with_loc LONGARROW tar=COMPLEX_ID
            { let (src,loc) = src_loc in (Ast.Shift_out (Ast.act_id_of_ci src, Ast.act_id_of_ci tar), loc) }

        (* shift m ==> n *)
        | SHIFT src_loc=complex_id_with_loc LONGARROW tar=COMPLEX_ID
            { let (src,loc) = src_loc in (Ast.Shift_edge (Ast.act_id_of_ci src, Ast.act_id_of_ci tar), loc) }

        (* merge m ==> n *)
        | MERGE src_loc=complex_id_with_loc LONGARROW tar=COMPLEX_ID
            { let (src,loc) = src_loc in (Ast.Merge_node (Ast.act_id_of_ci src, Ast.act_id_of_ci tar), loc) }

        (* del_node n *)
        | DEL_NODE ci_loc=complex_id_with_loc
            { let (ci,loc) = ci_loc in (Ast.Del_node (Ast.act_id_of_ci ci), loc) }

        (* add_node n: <-[x]- m *)
        | ADD_NODE new_ci_loc=simple_id_with_loc DDOT label=delimited(RTL_EDGE_LEFT,label_ident,RTL_EDGE_RIGHT) anc_ci=COMPLEX_ID
            { let (new_ci,loc) = new_ci_loc in (Ast.New_neighbour (new_ci, Ast.act_id_of_ci anc_ci,label), loc) }

        (* activate n#a *)
        | ACTIVATE ci_loc=complex_id_with_loc
            { let (ci,loc) = ci_loc in (Ast.Activate (Ast.act_id_of_ci ci), loc) }

        (* del_feat m.cat *)
        | DEL_FEAT qfn_loc=complex_id_with_loc
            { let (qfn,loc) = qfn_loc in (Ast.Del_feat (Ast.act_qfn_of_ci qfn), loc) }

        (* m.cat = n.x + "_" + nn.y *)
        | qfn_loc=complex_id_with_loc EQUAL items=separated_nonempty_list (PLUS, concat_item)
            { let (qfn,loc) = qfn_loc in (Ast.Update_feat ((Ast.act_qfn_of_ci qfn), items), loc) }

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
        | doc=option(COMMENT) id_loc=simple_id_with_loc mod_names=delimited(LACC,separated_list_final_opt(SEMIC,simple_id),RACC)
            {
              { Ast.seq_name = fst id_loc;
                seq_mod = mod_names;
                seq_doc = begin match doc with Some d -> d | None -> [] end;
                seq_loc = snd id_loc;
              }
            }

/*=============================================================================================*/
/* ISOLATED PATTERN (grep mode)                                                                 */
/*=============================================================================================*/
pattern:
        | p=pos_item n=list(neg_item) EOF { {Ast.pat_pos=p; pat_negs=n} }
%%
