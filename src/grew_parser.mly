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
  | Graph_meta of string
  | Graph_node of Ast.node
  | Graph_edge of Ast.edge

let get_loc () = Loc.file_line !Global.current_file !Global.current_line
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
%token SHARP                       /* # */
%token PLUS                        /* + */
%token EQUAL                       /* = */
%token REGEXP                      /* == */
%token DISEQUAL                    /* <> */
%token BANG                        /* ! */
%token STAR                        /* * */
%token LT                          /* < */
%token GT                          /* > */
%token LE                          /* <= or ≤ */
%token GE                          /* >= or ≥ */

%token PIPE                        /* | */

%token EDGE                        /* -> */
%token LTR_EDGE_LEFT               /* -[ */
%token LTR_EDGE_LEFT_NEG           /* -[^ */
%token LTR_EDGE_RIGHT              /* ]-> */
%token RTL_EDGE_LEFT               /* <-[ */
%token RTL_EDGE_RIGHT              /* ]- */

%token ARROW                       /* ==> */
%token ARROW_LEFT                  /* =[ */
%token ARROW_LEFT_NEG              /* =[^ */
%token ARROW_RIGHT                 /* ]=> */

%token DOMAIN                      /* domain */
%token INCL                        /* include */
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

%token <string> ID   /* the general notion of id */

/* %token <Grew_ast.Ast.complex_id>   COMPLEX_ID*/

%token <string>           STRING
%token <float>            FLOAT
%token <string list>      COMMENT
%token <string list>      LEX_PAR

%token EOF                         /* end of file */

%start <Grew_ast.Ast.grs_wi> grs_wi
%start <Grew_ast.Ast.gr> gr
%start <Grew_ast.Ast.module_or_include list> included
%start <Grew_ast.Ast.pattern> pattern
%start <Grew_ast.Ast.domain> domain

%left SEMIC
%left PLUS
%nonassoc STAR
%nonassoc DISEQUAL
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

label_ident:
        | x=ID        { Ast.parse_label_ident x }

pattern_label_ident:
        | x=ID        { Ast.parse_pattern_label_ident x }

simple_id:
        | id=ID       { Ast.parse_simple_ident id }

simple_id_with_loc:
        | id=ID       { localize (Ast.parse_simple_ident id) }

command_node_ident :
        | id=ID       { Ast.parse_command_node_ident id }

command_node_ident_with_loc :
        | id=ID       { localize (Ast.parse_command_node_ident id) }

feature_ident :
        | id=ID       { Ast.parse_feature_ident id }

feature_ident_with_loc :
        | id=ID      { localize (Ast.parse_feature_ident id) }

ineq_ident :
        | id=ID       { Ast.parse_ineq_ident id }

ineq_ident_with_loc :
        | id=ID      { localize (Ast.parse_ineq_ident id) }

command_feature_ident_with_loc :
        | id=ID      { localize (Ast.parse_command_feature_ident id) }

feature_value:
        | v=ID        { v }
        | v=STRING    { v }
        | v=FLOAT     { Printf.sprintf "%g" v }

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
        | id=simple_id EQUAL value=feature_value
            { Graph_meta (id ^ " = " ^ value) }

        (* B (1) [phon="pense", lemma="penser", cat=v, mood=ind ] *)
        (* B [phon="pense", lemma="penser", cat=v, mood=ind ] *)
        | id_loc=simple_id_with_loc position=option(delimited(LPAREN, FLOAT ,RPAREN)) feats=delimited(LBRACKET,separated_list_final_opt(COMA,node_features),RBRACKET)
            { let (id,loc) = id_loc in
              Graph_node ({Ast.node_id = id; position=position; fs=feats}, loc) }

        (* A -[x]-> B *)
        | n1_loc=simple_id_with_loc label=delimited(LTR_EDGE_LEFT,label_ident,LTR_EDGE_RIGHT) n2=simple_id
            { Graph_edge ({Ast.edge_id = None; src=fst n1_loc; edge_label_cst=([label],false); tar=n2}, snd n1_loc) }

/*=============================================================================================*/
/*  DOMAIN DEFINITION                                                                          */
/*=============================================================================================*/
domain:
        | f=features_group g=labels
            {
              {  Ast.feature_domain = f;
                 label_domain = g;
              }
            }

/*=============================================================================================*/
/*  GREW GRAPH REWRITING SYSTEM                                                                */
/*=============================================================================================*/
grs_wi:
        | d=domain m=module_or_include_list s=option(sequences) EOF
            {
             { Ast.domain_wi=Ast.Dom d;
               modules_wi=m;
               sequences_wi=match s with Some seq -> seq | None -> [];
             }
           }
        | DOMAIN file=STRING m=module_or_include_list s=option(sequences) EOF
            {
             { Ast.domain_wi=Ast.Dom_file file;
               modules_wi=m;
               sequences_wi=match s with Some seq -> seq | None -> [];
             }
           }

module_or_include_list:
        | x=list(module_or_include) { x }

module_or_include:
        | m=grew_module             { Ast.Modul m }
        | INCL sub=subfile SEMIC { Ast.Includ sub }

subfile:
        | f=STRING  { localize f }

/*=============================================================================================*/
/* FEATURES DOMAIN DEFINITION                                                                  */
/*=============================================================================================*/
features_group:
        | FEATURES x=features { x }

features:
        | LACC x=separated_nonempty_list_final_opt(SEMIC,feature) RACC { x }

feature:
        (* pos=# *)
        (* m: ind,inf,part,subj,imp *)
        | feature_name=feature_name DDOT feature_values=features_values
            {
              match feature_values with
                | ["#"] -> Feature_domain.Num feature_name
                | _ -> Feature_domain.build_closed feature_name feature_values
            }

        (* phon:* *)
        | feature_name=feature_name DDOT STAR
            { Feature_domain.Open feature_name }

feature_name:
        | ci=ID { ci }

features_values:
        | SHARP                                         { ["#"] }
        | x=separated_nonempty_list(COMA,feature_value) { x }

/*=============================================================================================*/
/* GLOBAL LABELS DEFINITION                                                                    */
/*=============================================================================================*/
labels:
        (* labels { OBJ, SUBJ, DE_OBJ, ANT } *)
        | LABELS x=delimited(LACC,separated_nonempty_list_final_opt(COMA,label),RACC) { x }

label:
        | x=label_ident display_list=list(display)  { (x, display_list) }
        | x=STRING                                  { (x,[]) }

display:
        | dis=AROBAS_ID   { dis }
        | col=COLOR       { col }

/*=============================================================================================*/
/* MODULE DEFINITION                                                                           */
/*=============================================================================================*/
included:
        | x=list(module_or_include) EOF { x }

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
                pattern = Ast.complete_pattern { Ast.pat_pos = p; Ast.pat_negs = n };
                commands = cmds;
                param = None;
                lex_par = None;
                rule_doc = begin match doc with Some d -> d | None -> [] end;
                rule_loc = snd id_loc;
              }
            }
        | doc=option(COMMENT) RULE id_loc=simple_id_with_loc param=param LACC p=pos_item n=list(neg_item) cmds=commands RACC lex_par=option(lex_par)
            {
              { Ast.rule_id = fst id_loc;
                pattern = Ast.complete_pattern { Ast.pat_pos = p; Ast.pat_negs = n };
                commands = cmds;
                param = Some param;
                lex_par = lex_par;
                rule_doc = begin match doc with Some d -> d | None -> [] end;
                rule_loc = snd id_loc;
              }
            }
        | doc=option(COMMENT) FILTER id_loc=simple_id_with_loc LACC p=pos_item n=list(neg_item) RACC
            {
              { Ast.rule_id = fst id_loc;
                pattern = Ast.complete_pattern { Ast.pat_pos = p; Ast.pat_negs = n };
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
        | n=pat_node           { Pat_node n }
        | ec=pat_edge_or_const { ec }

pat_node:
        (* "R [cat=V, lemma=$lemma]" *)
        | id_loc=simple_id_with_loc feats=delimited(LBRACKET,separated_list_final_opt(COMA,node_features),RBRACKET)
            { ({Ast.node_id = fst id_loc; position=None; fs= feats}, snd id_loc) }

node_features:
        (*  "cat = n|v|adj"     *)
        (*  "cat = *"           *)
        | name_loc=simple_id_with_loc EQUAL values=separated_nonempty_list(PIPE,feature_value)
            { let (name,loc) = name_loc in
              match values with
              | ["*"] -> ({Ast.kind = Ast.Disequality []; name},loc)
              | _ -> ({Ast.kind = Ast.Equality values; name }, loc) }

        (*  "cat = *"           *)
        | name_loc=simple_id_with_loc EQUAL STAR
            { let (name,loc) = name_loc in ({Ast.kind = Ast.Disequality []; name},loc) }

        (*   "cat<>n|v|adj"     *)
        | name_loc=simple_id_with_loc DISEQUAL values=separated_nonempty_list(PIPE,feature_value)
            { let (name,loc) = name_loc in ( {Ast.kind = Ast.Disequality values; name}, loc) }

        (*   "lemma=$lem"       *)
        | name_loc=simple_id_with_loc EQUAL p=DOLLAR_ID
            { let (name,loc) = name_loc in ( {Ast.kind = Ast.Equal_param p; name }, loc) }

        (*   "!lemma"           *)
        | BANG name_loc=simple_id_with_loc
            { let (name,loc) = name_loc in ({Ast.kind = Ast.Absent; name}, loc) }

pat_edge_or_const:
        (*   "e: A -> B"           *)
        | id_loc=simple_id_with_loc DDOT n1=simple_id EDGE n2=simple_id
            { let (id,loc) = id_loc in Pat_edge ({Ast.edge_id = Some id; src=n1; edge_label_cst=([],true); tar=n2}, loc) }

        (* "e: A -[X|Y]-> B" *)
        | id_loc=simple_id_with_loc DDOT n1=simple_id labels=delimited(LTR_EDGE_LEFT,separated_nonempty_list(PIPE,pattern_label_ident),LTR_EDGE_RIGHT) n2=simple_id
            { let (id,loc) = id_loc in Pat_edge ({Ast.edge_id = Some id; src=n1; edge_label_cst=(labels,false); tar=n2}, loc) }

        (* "e: A -[^X|Y]-> B" *)
        | id_loc=simple_id_with_loc DDOT n1=simple_id labels=delimited(LTR_EDGE_LEFT_NEG,separated_nonempty_list(PIPE,pattern_label_ident),LTR_EDGE_RIGHT) n2=simple_id
            { let (id,loc) = id_loc in Pat_edge ({Ast.edge_id = Some id; src=n1; edge_label_cst=(labels,true); tar=n2}, loc) }

        (*   "A -> B"           *)
        | n1_loc=simple_id_with_loc EDGE n2=simple_id
            { let (n1,loc) = n1_loc in Pat_edge ({Ast.edge_id = None; src=n1; edge_label_cst=([],true); tar=n2}, loc) }

        (*   "A -> *"           *)
        | n1_loc=simple_id_with_loc EDGE STAR
            { let (n1,loc) = n1_loc in Pat_const (Ast.Cst_out (n1,([],true)), loc) }

        (*   "* -> B"           *)
        | STAR EDGE n2_loc=simple_id_with_loc
            { let (n2,loc) = n2_loc in Pat_const (Ast.Cst_in (n2,([],true)), loc) }

        (*   "A -[X|Y]-> B"   *)
        | n1_loc=simple_id_with_loc labels=delimited(LTR_EDGE_LEFT,separated_nonempty_list(PIPE,pattern_label_ident),LTR_EDGE_RIGHT) n2=simple_id
            { let (n1,loc) = n1_loc in Pat_edge ({Ast.edge_id = None; src=n1; edge_label_cst=(labels,false); tar=n2}, loc) }

        (*   "A -[X|Y]-> *"   *)
        | n1_loc=simple_id_with_loc labels=delimited(LTR_EDGE_LEFT,separated_nonempty_list(PIPE,pattern_label_ident),LTR_EDGE_RIGHT) STAR
            { let (n1,loc) = n1_loc in Pat_const (Ast.Cst_out (n1,(labels,false)), loc) }

        (*   "* -[X|Y]-> B"   *)
        | STAR labels=delimited(LTR_EDGE_LEFT,separated_nonempty_list(PIPE,pattern_label_ident),LTR_EDGE_RIGHT) n2_loc=simple_id_with_loc
            { let (n2,loc) = n2_loc in Pat_const (Ast.Cst_in (n2,(labels,false)), loc) }


        (* "A -[^X|Y]-> B" *)
        | n1_loc=simple_id_with_loc labels=delimited(LTR_EDGE_LEFT_NEG,separated_nonempty_list(PIPE,pattern_label_ident),LTR_EDGE_RIGHT) n2=simple_id
            { let (n1,loc) = n1_loc in Pat_edge ({Ast.edge_id = None; src=n1; edge_label_cst=(labels,true); tar=n2}, loc) }

        (* "A -[^X|Y]-> *" *)
        | n1_loc=simple_id_with_loc labels=delimited(LTR_EDGE_LEFT_NEG,separated_nonempty_list(PIPE,pattern_label_ident),LTR_EDGE_RIGHT) STAR
            { let (n1,loc) = n1_loc in Pat_const (Ast.Cst_out (n1,(labels,true)), loc) }

        (* "* -[^X|Y]-> B" *)
        | STAR labels=delimited(LTR_EDGE_LEFT_NEG,separated_nonempty_list(PIPE,pattern_label_ident),LTR_EDGE_RIGHT) n2_loc=simple_id_with_loc
            { let (n2,loc) = n2_loc in Pat_const (Ast.Cst_in (n2,(labels,true)), loc) }

        (* "X.cat = Y.cat" *)
        | feat_id1_loc=feature_ident_with_loc EQUAL feat_id2=feature_ident
            { let (feat_id1,loc)=feat_id1_loc in Pat_const (Ast.Feature_eq (feat_id1, feat_id2), loc) }

        (* "X.cat <> Y.cat" *)
        | feat_id1_loc=feature_ident_with_loc DISEQUAL feat_id2=feature_ident
            { let (feat_id1,loc)=feat_id1_loc in Pat_const (Ast.Feature_diseq (feat_id1, feat_id2), loc) }

        (* "X.cat == "regexp" " *)
        | feat_id_loc=feature_ident_with_loc REGEXP regexp=STRING
            { let (feat_id,loc)=feat_id_loc in Pat_const (Ast.Feature_re (feat_id, regexp), loc) }

        (* "X.position < Y.position" *)
        | feat_id1_loc=ineq_ident_with_loc LT feat_id2=ineq_ident
            { let (feat_id1,loc)=feat_id1_loc in Pat_const (Ast.Feature_ineq (Ast.Lt, feat_id1, feat_id2), loc) }

        (* "X.position > Y.position" *)
        | feat_id1_loc=ineq_ident_with_loc GT feat_id2=ineq_ident
            { let (feat_id1,loc)=feat_id1_loc in Pat_const (Ast.Feature_ineq (Ast.Gt, feat_id1, feat_id2), loc) }

        (* "X.position <= Y.position" *)
        | feat_id1_loc=ineq_ident_with_loc LE feat_id2=ineq_ident
            { let (feat_id1,loc)=feat_id1_loc in Pat_const (Ast.Feature_ineq (Ast.Le, feat_id1, feat_id2), loc) }

        (* "X.position >= Y.position" *)
        | feat_id1_loc=ineq_ident_with_loc GE feat_id2=ineq_ident
            { let (feat_id1,loc)=feat_id1_loc in Pat_const (Ast.Feature_ineq (Ast.Ge, feat_id1, feat_id2), loc) }

        (* "X.feat >= 12.34" *)
        | feat_id1_loc=ineq_ident_with_loc GE num=FLOAT
        | num=FLOAT LE feat_id1_loc=ineq_ident_with_loc
            { let (feat_id1,loc)=feat_id1_loc in Pat_const (Ast.Feature_ineq_cst (Ast.Ge, feat_id1, num), loc)  }

        (* "X.feat > 12.34" *)
        | feat_id1_loc=ineq_ident_with_loc GT num=FLOAT
        | num=FLOAT LT feat_id1_loc=ineq_ident_with_loc
            { let (feat_id1,loc)=feat_id1_loc in Pat_const (Ast.Feature_ineq_cst (Ast.Gt, feat_id1, num), loc)  }

        (* "X.feat <= 12.34" *)
        | feat_id1_loc=ineq_ident_with_loc LE num=FLOAT
        | num=FLOAT GE feat_id1_loc=ineq_ident_with_loc
            { let (feat_id1,loc)=feat_id1_loc in Pat_const (Ast.Feature_ineq_cst (Ast.Le, feat_id1, num), loc)  }

        (* "X.feat < 12.34" *)
        | feat_id1_loc=ineq_ident_with_loc LT num=FLOAT
        | num=FLOAT GT feat_id1_loc=ineq_ident_with_loc
            { let (feat_id1,loc)=feat_id1_loc in Pat_const (Ast.Feature_ineq_cst (Ast.Lt, feat_id1, num), loc)  }
       

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
        | DEL_EDGE src_loc=command_node_ident_with_loc label=delimited(LTR_EDGE_LEFT,label_ident,LTR_EDGE_RIGHT) tar=command_node_ident
            { let (src,loc) = src_loc in (Ast.Del_edge_expl (src, tar, label), loc) }

        (* add_edge m -[x]-> n *)
        | ADD_EDGE src_loc=command_node_ident_with_loc label=delimited(LTR_EDGE_LEFT,label_ident,LTR_EDGE_RIGHT) tar=command_node_ident
            { let (src,loc) = src_loc in (Ast.Add_edge (src, tar, label), loc) }

        (* "shift_in m ==> n" *)
        | SHIFT_IN src_loc=command_node_ident_with_loc ARROW tar=command_node_ident
            { let (src,loc) = src_loc in (Ast.Shift_in (src, tar, ([], true)), loc) }

        (* "shift_in m =[x*|y]=> n" *)
        | SHIFT_IN src_loc=command_node_ident_with_loc 
          labels=delimited(ARROW_LEFT,separated_nonempty_list(PIPE,pattern_label_ident),ARROW_RIGHT)
          tar=command_node_ident
            { let (src,loc) = src_loc in (Ast.Shift_in (src, tar, (labels, false)), loc) }

        (* "shift_in m =[^x*|y]=> n" *)
        | SHIFT_IN src_loc=command_node_ident_with_loc 
          labels=delimited(ARROW_LEFT_NEG,separated_nonempty_list(PIPE,pattern_label_ident),ARROW_RIGHT)
          tar=command_node_ident
            { let (src,loc) = src_loc in (Ast.Shift_in (src, tar, (labels, true)), loc) }


        (* "shift_out m ==> n" *)
        | SHIFT_OUT src_loc=command_node_ident_with_loc ARROW tar=command_node_ident
            { let (src,loc) = src_loc in (Ast.Shift_out (src, tar, ([], true)), loc) }

        (* "shift_out m =[x*|y]=> n" *)
        | SHIFT_OUT src_loc=command_node_ident_with_loc 
          labels=delimited(ARROW_LEFT,separated_nonempty_list(PIPE,pattern_label_ident),ARROW_RIGHT)
          tar=command_node_ident
            { let (src,loc) = src_loc in (Ast.Shift_out (src, tar, (labels, false)), loc) }

        (* "shift_out m =[^x*|y]=> n" *)
        | SHIFT_OUT src_loc=command_node_ident_with_loc 
          labels=delimited(ARROW_LEFT_NEG,separated_nonempty_list(PIPE,pattern_label_ident),ARROW_RIGHT)
          tar=command_node_ident
            { let (src,loc) = src_loc in (Ast.Shift_out (src, tar, (labels, true)), loc) }


        (* "shift m ==> n" *)
        | SHIFT src_loc=command_node_ident_with_loc ARROW tar=command_node_ident
            { let (src,loc) = src_loc in (Ast.Shift_edge (src, tar, ([], true)), loc) }

        (* "shift m =[x*|y]=> n" *)
        | SHIFT src_loc=command_node_ident_with_loc 
          labels=delimited(ARROW_LEFT,separated_nonempty_list(PIPE,pattern_label_ident),ARROW_RIGHT)
          tar=command_node_ident
            { let (src,loc) = src_loc in (Ast.Shift_edge (src, tar, (labels, false)), loc) }

        (* "shift m =[^x*|y]=> n" *)
        | SHIFT src_loc=command_node_ident_with_loc 
          labels=delimited(ARROW_LEFT_NEG,separated_nonempty_list(PIPE,pattern_label_ident),ARROW_RIGHT)
          tar=command_node_ident
            { let (src,loc) = src_loc in (Ast.Shift_edge (src, tar, (labels, true)), loc) }

        (* merge m ==> n *)
        | MERGE src_loc=command_node_ident_with_loc ARROW tar=command_node_ident
            { let (src,loc) = src_loc in (Ast.Merge_node (src, tar), loc) }

        (* del_node n *)
        | DEL_NODE ci_loc=command_node_ident_with_loc
            { let (ci,loc) = ci_loc in (Ast.Del_node (ci), loc) }

        (* add_node n: <-[x]- m *)
        | ADD_NODE new_ci_loc=simple_id_with_loc DDOT label=delimited(RTL_EDGE_LEFT,label_ident,RTL_EDGE_RIGHT) anc_ci=command_node_ident
            { let (new_ci,loc) = new_ci_loc in (Ast.New_neighbour (new_ci, anc_ci,label), loc) }

        (* activate n#a *)
        | ACTIVATE ci_loc= command_node_ident_with_loc
            { let (ci,loc) = ci_loc in (Ast.Activate ci, loc) }

        (* del_feat m.cat *)
        | DEL_FEAT com_fead_id_loc= command_feature_ident_with_loc
            { let (com_fead_id,loc) = com_fead_id_loc in (Ast.Del_feat com_fead_id, loc) }

        (* m.cat = n.x + "_" + nn.y *)
        | com_fead_id_loc= command_feature_ident_with_loc EQUAL items=separated_nonempty_list (PLUS, concat_item)
            { let (com_fead_id,loc) = com_fead_id_loc in (Ast.Update_feat (com_fead_id, items), loc) }

concat_item:
        | gi=ID            { if Ast.is_simple_ident gi then Ast.String_item gi else Ast.Qfn_item (Ast.parse_feature_ident gi) }
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
/*        | doc=option(COMMENT) id_loc=simple_id_with_loc mod_names=delimited(LACC,separated_list_final_opt(SEMIC,simple_id),RACC)
            {
              { Ast.seq_name = fst id_loc;
                seq_mod = mod_names;*/

        | doc = option(COMMENT) id_loc=simple_id_with_loc mod_names=delimited(LACC,separated_list_final_opt(SEMIC,simple_id),RACC)
            {
              Ast.Old { Ast.seq_name = fst id_loc;
                seq_mod = mod_names ;
                seq_doc = begin match doc with Some d -> d | None -> [] end;
                seq_loc = snd id_loc;
              }
            }
        | doc = option(COMMENT) id_loc=simple_id_with_loc EQUAL s=op_seq { Ast.New (id_loc, s) }

op_seq:
        | m=simple_id               { Ast.Ref m }
        | LPAREN s=op_seq RPAREN    { s }
        | s=op_seq STAR             { Ast.Star (s) }
        | s1=op_seq PLUS s2=op_seq  { Ast.Plus [s1; s2] }
        | s1=op_seq SEMIC s2=op_seq { Ast.List [s1; s2] }
        | DISEQUAL s=op_seq         { Ast.Diamond s }


/*=============================================================================================*/
/* ISOLATED PATTERN (grep mode)                                                                 */
/*=============================================================================================*/
pattern:
        | p=pos_item n=list(neg_item) EOF { Ast.complete_pattern {Ast.pat_pos=p; pat_negs=n} }
%%
