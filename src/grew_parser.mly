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

type ineq_item =
  | Ineq_sofi of Ast.simple_or_pointed
  | Ineq_float of float

let get_loc () = Global.get_loc ()
let localize t = (t,get_loc ())
%}

%token DUMMY

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
%token DISEQUAL                    /* <> */
%token BANG                        /* ! */
%token STAR                        /* * */
%token LT                          /* < */
%token GT                          /* > */
%token LE                          /* <= or ≤ */
%token GE                          /* >= or ≥ */
%token LPREC                       /* << */
%token LSUCC                       /* >> */

%token BEFORE                      /* :< */
%token AFTER                       /* :> */

%token PIPE                        /* | */

%token EDGE                        /* -> */
%token LTR_EDGE_LEFT               /* -[ */
%token LTR_EDGE_LEFT_NEG           /* -[^ */
%token LTR_EDGE_RIGHT              /* ]-> */

%token ARROW                       /* ==> */
%token ARROW_LEFT                  /* =[ */
%token ARROW_LEFT_NEG              /* =[^ */
%token ARROW_RIGHT                 /* ]=> */

%token DOMAIN                      /* domain */
%token INCL                        /* include */
%token IMPORT                      /* import */
%token FEATURES                    /* features */
%token FEATURE                     /* feature */
%token FILE                        /* file */
%token LABELS                      /* labels */
%token PATTERN                     /* pattern */
%token WITHOUT                     /* without */
%token COMMANDS                    /* commands */
%token MODULE                      /* module */
%token STRAT                       /* strat */
%token PACKAGE                     /* package */
%token DETERMINISTIC               /* deterministic (or deprecated confluent) */
%token RULE                        /* rule */
%token SEQUENCES                   /* sequences */
%token GRAPH                       /* graph */

%token DEL_EDGE                    /* del_edge */
%token ADD_EDGE                    /* add_edge */
%token SHIFT_IN                    /* shift_in */
%token SHIFT_OUT                   /* shift_out */
%token SHIFT                       /* shift */
%token DEL_NODE                    /* del_node */
%token ADD_NODE                    /* add_node */
%token DEL_FEAT                    /* del_feat */

%token PICK                        /* Pick */
%token ALT                         /* Alt */
%token SEQ                         /* Seq */
%token ITER                        /* Iter */
%token IF                          /* If */
%token ONF                         /* Onf */
%token EMPTY                       /* Empty */
%token TRY                         /* Try */

%token <string> DOLLAR_ID          /* $id */
%token <string> AROBAS_ID          /* @id */
%token <string> COLOR              /* @#89abCD */

%token <string> ID   /* the general notion of id */

/* %token <Grew_ast.Ast.complex_id>   COMPLEX_ID*/

%token <string>                STRING
%token <string>                REGEXP
%token <float>                 FLOAT
%token <string list>           COMMENT
%token <string * string list>  LEX_PAR

%token EOF                         /* end of file */

%start <Grew_ast.Ast.grs_wi> grs_wi
%start <Grew_ast.Ast.gr> gr
%start <Grew_ast.Ast.module_or_include list> included
%start <Grew_ast.Ast.pattern> pattern
%start <Grew_ast.Ast.domain> domain

%start <Grew_ast.New_ast.grs> new_grs
%start <Grew_ast.New_ast.strat> strat_alone

/* parsing of the string representation of the constituent representation of Sequoia */
/* EX: "( (SENT (NP (NC Amélioration) (PP (P de) (NP (DET la) (NC sécurité))))))"    */
%start <Grew_ast.Ast.pst> phrase_structure_tree

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

node_id:
        | id=ID       { Ast.parse_node_ident id }

node_id_with_loc:
        | id=ID       { localize (Ast.parse_node_ident id) }

feature_ident :
        | id=ID       { Ast.parse_feature_ident id }

feature_ident_with_loc :
        | id=ID      { localize (Ast.parse_feature_ident id) }

feature_value:
        | v=ID        { Ast.parse_simple_ident v }
        | v=STRING    { v }
        | v=FLOAT     { Printf.sprintf "%g" v }

pattern_feature_value:
        | v=ID        { Ast.parse_simple_or_pointed v }
        | v=STRING    { Ast.Simple v }
        | v=FLOAT     { Ast.Simple (Printf.sprintf "%g" v) }

ineq_value:
        | v=ID    { Ineq_sofi (Ast.parse_simple_or_pointed v) }
        | v=FLOAT { Ineq_float v }

ineq_value_with_loc:
        | v=ID    { localize (Ineq_sofi (Ast.parse_simple_or_pointed v)) }
        | v=FLOAT { localize (Ineq_float v) }

/*=============================================================================================*/
/*  GREW GRAPH                                                                                 */
/*=============================================================================================*/
gr:
        | GRAPH LACC items=separated_list_final_opt(SEMIC,gr_item) RACC EOF
            {
              Ast.complete_graph
              {
                Ast.meta = List_.opt_map (function Graph_meta n -> Some n | _ -> None) items;
                Ast.nodes = List_.opt_map (function Graph_node n -> Some n | _ -> None) items;
                Ast.edges = List_.opt_map (function Graph_edge n -> Some n | _ -> None) items;
              }
            }

gr_item:
        /*  sentence = "Jean dort."   */
        | id=simple_id EQUAL value=feature_value
            { Graph_meta (id ^ " = " ^ value) }

        /*  B (1) [phon="pense", lemma="penser", cat=v, mood=ind ]   */
        /*  B [phon="pense", lemma="penser", cat=v, mood=ind ]   */
        | id_loc=node_id_with_loc position=option(delimited(LPAREN, FLOAT ,RPAREN)) feats=delimited(LBRACKET,separated_list_final_opt(COMA,node_features),RBRACKET)
            { let (id,loc) = id_loc in
              Graph_node ({Ast.node_id = id; position=position; fs=feats}, loc) }
        /*   A   */
        | id_loc=node_id_with_loc
            { let (id,loc) = id_loc in
              Graph_node ({Ast.node_id = id; position=None; fs=Ast.default_fs ~loc id}, loc) }

        /*   A -[x]-> B   */
        | n1_loc=node_id_with_loc label=delimited(LTR_EDGE_LEFT,label_ident,LTR_EDGE_RIGHT) n2=node_id
            { Graph_edge ({Ast.edge_id = None; src=fst n1_loc; edge_label_cst=Ast.Pos_list [label]; tar=n2}, snd n1_loc) }

/*=============================================================================================*/
/*  DOMAIN DEFINITION                                                                          */
/*=============================================================================================*/
domain:
        | c=option(DUMMY) f=feature_group g=labels EOF
            {
              {  Ast.feature_domain = f;
                 label_domain = g;
              }
            }

/*=============================================================================================*/
/*  GREW GRAPH REWRITING SYSTEM                                                                */
/*=============================================================================================*/
grs_wi:
        | d=option(domain) m=module_or_include_list s=option(sequences) EOF
            {
             { Ast.domain_wi=(match d with Some dom -> Some (Ast.Dom dom) | None -> None);
               modules_wi=m;
               strategies_wi=match s with Some seq -> seq | None -> [];
             }
           }
        | DOMAIN file=STRING m=module_or_include_list s=option(sequences) EOF
            {
             { Ast.domain_wi= Some (Ast.Dom_file file);
               modules_wi=m;
               strategies_wi=match s with Some seq -> seq | None -> [];
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
feature_group:
        | FEATURES x=features { x }

features:
        | LACC x=separated_nonempty_list_final_opt(SEMIC,feature) RACC { x }

feature:
        /*   pos=#   */
        /*   m: ind,inf,part,subj,imp   */
        | feature_name=feature_name DDOT feature_values=feature_values
            {
              match feature_values with
                | ["#"] -> Ast.Num feature_name
                | _ -> Ast.build_closed feature_name feature_values
            }

        /*   phon:*   */
        | feature_name=feature_name DDOT STAR
            { Ast.Open feature_name }

feature_name:
        | ci=ID { Ast.to_uname ci }

feature_values:
        | SHARP                                         { ["#"] }
        | x=separated_nonempty_list(COMA,feature_value) { x }

/*=============================================================================================*/
/* GLOBAL LABELS DEFINITION                                                                    */
/*=============================================================================================*/
labels:
        /*   labels { OBJ, SUBJ, DE_OBJ, ANT }   */
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
        | doc=option(COMMENT) MODULE det=boption(DETERMINISTIC) id_loc=simple_id_with_loc LACC l=option(labels) r=rules RACC
           {
            { Ast.module_id = fst id_loc;
              rules = r;
              deterministic = det;
              module_doc = (match doc with Some d -> d | None -> []);
              mod_loc = snd id_loc;
              mod_dir = "";
            }
          }

/*=============================================================================================*/
/* RULES DEFINITION                                                                            */
/*=============================================================================================*/
rules:
        | r = list(rule) { r }

rule:
        | doc=option(COMMENT) RULE id_loc=simple_id_with_loc LACC p=pos_item n=list(neg_item) cmds=commands RACC lex_par=list(lex_par)
            {
              { Ast.rule_id = fst id_loc;
                pattern = Ast.complete_pattern { Ast.pat_pos = p; Ast.pat_negs = n };
                commands = cmds;
                param = None;
                lex_par = None;
                lexicon_info = lex_par;
                rule_doc = begin match doc with Some d -> d | None -> [] end;
                rule_loc = snd id_loc;
                rule_dir = None;
              }
            }
        | doc=option(COMMENT) RULE id_loc=simple_id_with_loc param=param LACC p=pos_item n=list(neg_item) cmds=commands RACC lex_par=option(lex_par)
            {
              { Ast.rule_id = fst id_loc;
                pattern = Ast.complete_pattern { Ast.pat_pos = p; Ast.pat_negs = n };
                commands = cmds;
                param = Some param;
                lex_par = None;
                lexicon_info = []; (* TODOLEX *)
                rule_doc = begin match doc with Some d -> d | None -> [] end;
                rule_loc = snd id_loc;
                rule_dir = None;
              }
            }

lex_par:
        | lex_par = LEX_PAR  { (fst lex_par, Ast.Final (snd lex_par)) }

param:
        | LPAREN FEATURE vars=separated_nonempty_list(COMA,var) RPAREN                                       { ([],vars) }
        | LPAREN FEATURE vars=separated_nonempty_list(COMA,var) SEMIC files=separated_list(COMA,file) RPAREN { (files,vars) }

file:
        | FILE f=STRING     { f }
var:
        | i=DOLLAR_ID       { i }
        | i=AROBAS_ID       { i }

pos_item:
        | PATTERN i=pn_item   { i }

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
/* PATTERN DEFINITION                                                                            */
/*=============================================================================================*/
pat_item:
        /*   R [cat=V, lemma=$lemma]   */
        | id_loc=simple_id_with_loc feats=delimited(LBRACKET,separated_list_final_opt(COMA,node_features),RBRACKET)
            { Pat_node ({Ast.node_id = fst id_loc; position=None; fs= feats}, snd id_loc) }

        /*   e: A -> B   */
        | id_loc=simple_id_with_loc DDOT n1=simple_id EDGE n2=simple_id
            { let (id,loc) = id_loc in Pat_edge ({Ast.edge_id = Some id; src=n1; edge_label_cst=(Ast.Neg_list []); tar=n2}, loc) }

        /*   e: A -[X|Y]-> B   */
        | id_loc=simple_id_with_loc DDOT n1=simple_id labels=delimited(LTR_EDGE_LEFT,separated_nonempty_list(PIPE,pattern_label_ident),LTR_EDGE_RIGHT) n2=simple_id
            { let (id,loc) = id_loc in Pat_edge ({Ast.edge_id = Some id; src=n1; edge_label_cst=(Ast.Pos_list labels); tar=n2}, loc) }

        /*   e: A -[^X|Y]-> B   */
        | id_loc=simple_id_with_loc DDOT n1=simple_id labels=delimited(LTR_EDGE_LEFT_NEG,separated_nonempty_list(PIPE,pattern_label_ident),LTR_EDGE_RIGHT) n2=simple_id
            { let (id,loc) = id_loc in Pat_edge ({Ast.edge_id = Some id; src=n1; edge_label_cst=(Ast.Neg_list labels); tar=n2}, loc) }

        /*   e: A -[re"regexp"]-> B   */
        | id_loc=simple_id_with_loc DDOT n1=simple_id LTR_EDGE_LEFT re=REGEXP LTR_EDGE_RIGHT n2=simple_id
            { let (id,loc) = id_loc in Pat_edge ({Ast.edge_id = Some id; src=n1; edge_label_cst=Ast.Regexp re; tar=n2}, loc) }

        /*   A -> B   */
        | n1_loc=simple_id_with_loc EDGE n2=simple_id
            { let (n1,loc) = n1_loc in Pat_edge ({Ast.edge_id = None; src=n1; edge_label_cst=Ast.Neg_list []; tar=n2}, loc) }

        /*   A -> *   */
        | n1_loc=simple_id_with_loc EDGE STAR
            { let (n1,loc) = n1_loc in Pat_const (Ast.Cst_out (n1,Ast.Neg_list []), loc) }

        /*   * -> B   */
        | STAR EDGE n2_loc=simple_id_with_loc
            { let (n2,loc) = n2_loc in Pat_const (Ast.Cst_in (n2,Ast.Neg_list []), loc) }

        /*   A -[X|Y]-> B   */
        | n1_loc=simple_id_with_loc labels=delimited(LTR_EDGE_LEFT,separated_nonempty_list(PIPE,pattern_label_ident),LTR_EDGE_RIGHT) n2=simple_id
            { let (n1,loc) = n1_loc in Pat_edge ({Ast.edge_id = None; src=n1; edge_label_cst=Ast.Pos_list labels; tar=n2}, loc) }

        /*   A -[X|Y]-> *   */
        | n1_loc=simple_id_with_loc labels=delimited(LTR_EDGE_LEFT,separated_nonempty_list(PIPE,pattern_label_ident),LTR_EDGE_RIGHT) STAR
            { let (n1,loc) = n1_loc in Pat_const (Ast.Cst_out (n1,Ast.Pos_list labels), loc) }

        /*   * -[X|Y]-> B   */
        | STAR labels=delimited(LTR_EDGE_LEFT,separated_nonempty_list(PIPE,pattern_label_ident),LTR_EDGE_RIGHT) n2_loc=simple_id_with_loc
            { let (n2,loc) = n2_loc in Pat_const (Ast.Cst_in (n2,Ast.Pos_list labels), loc) }

        /*   A -[^X|Y]-> B   */
        | n1_loc=simple_id_with_loc labels=delimited(LTR_EDGE_LEFT_NEG,separated_nonempty_list(PIPE,pattern_label_ident),LTR_EDGE_RIGHT) n2=simple_id
            { let (n1,loc) = n1_loc in Pat_edge ({Ast.edge_id = None; src=n1; edge_label_cst=Ast.Neg_list labels; tar=n2}, loc) }

        /*   A -[re"regexp"]-> B   */
        | n1_loc=simple_id_with_loc LTR_EDGE_LEFT re=REGEXP LTR_EDGE_RIGHT n2=simple_id
            { let (n1,loc) = n1_loc in Pat_edge ({Ast.edge_id = None; src=n1; edge_label_cst=Ast.Regexp re; tar=n2}, loc) }

        /*   A -[^X|Y]-> *   */
        | n1_loc=simple_id_with_loc labels=delimited(LTR_EDGE_LEFT_NEG,separated_nonempty_list(PIPE,pattern_label_ident),LTR_EDGE_RIGHT) STAR
            { let (n1,loc) = n1_loc in Pat_const (Ast.Cst_out (n1,Ast.Neg_list labels), loc) }

        /*   * -[^X|Y]-> B   */
        | STAR labels=delimited(LTR_EDGE_LEFT_NEG,separated_nonempty_list(PIPE,pattern_label_ident),LTR_EDGE_RIGHT) n2_loc=simple_id_with_loc
            { let (n2,loc) = n2_loc in Pat_const (Ast.Cst_in (n2,Ast.Neg_list labels), loc) }

        /*   X.cat = lex.value   */
        /*   X.cat = Y.cat   */
        /*   X.cat = lex.value   */
        | feat_id1_loc=feature_ident_with_loc EQUAL rhs=ID
             { let (feat_id1,loc)=feat_id1_loc in
              match Ast.parse_simple_or_pointed rhs with
              | Ast.Simple value ->
                Pat_const (Ast.Feature_eq_cst (feat_id1, value), loc)
              | Ast.Pointed (s1, s2) ->
                Printf.printf "###%s--%s###\n%!" s1 s2;
                Pat_const (Ast.Feature_eq_lex_or_fs (feat_id1, (s1,s2)), loc)
             }

        /*   X.cat = "value"   */
        | feat_id1_loc=feature_ident_with_loc EQUAL rhs=STRING
            { let (feat_id1,loc)=feat_id1_loc in Pat_const (Ast.Feature_eq_cst (feat_id1, rhs), loc) }

        /*   X.cat = 12.34   */
        | feat_id1_loc=feature_ident_with_loc EQUAL rhs=FLOAT
            { let (feat_id1,loc)=feat_id1_loc in Pat_const (Ast.Feature_eq_float (feat_id1, rhs), loc) }

        /*   X.cat <> Y.cat   */
        /*   X.cat <> value   */
        /*   X.cat <> lex.value   */
        | feat_id1_loc=feature_ident_with_loc DISEQUAL rhs=ID
             { let (feat_id1,loc)=feat_id1_loc in
              match Ast.parse_simple_or_pointed rhs with
              | Ast.Simple value ->
                Pat_const (Ast.Feature_diff_cst (feat_id1, value), loc)
              | Ast.Pointed (s1, s2) ->
                Pat_const (Ast.Feature_diff_lex_or_fs (feat_id1, (s1,s2)), loc)
             }

        /*   X.cat <> "value"   */
        | feat_id1_loc=feature_ident_with_loc DISEQUAL rhs=STRING
            { let (feat_id1,loc)=feat_id1_loc in Pat_const (Ast.Feature_diff_cst (feat_id1, rhs), loc) }

        /*   X.cat <> 12.34   */
        | feat_id1_loc=feature_ident_with_loc DISEQUAL rhs=FLOAT
            { let (feat_id1,loc)=feat_id1_loc in Pat_const (Ast.Feature_diff_float (feat_id1, rhs), loc) }


        /*   X.cat = re"regexp"   */
        | feat_id_loc=feature_ident_with_loc EQUAL regexp=REGEXP
            { let (feat_id,loc)=feat_id_loc in Pat_const (Ast.Feature_eq_regexp (feat_id, regexp), loc) }

        | id1_loc=ineq_value_with_loc LT id2=ineq_value
            { let (id1,loc)=id1_loc in
              match (id1, id2) with
              (*   X.feat < Y.feat   *)
              | (Ineq_sofi (Ast.Pointed (n1, f1)), Ineq_sofi (Ast.Pointed (n2, f2))) ->
                Pat_const (Ast.Features_ineq (Ast.Lt, (n1,f1), (n2,f2)), loc)

              (*   X.feat < 12.34   *)
              | (Ineq_sofi (Ast.Pointed (n1, f1)), Ineq_float num) ->
                Pat_const (Ast.Feature_ineq_cst (Ast.Lt, (n1,f1), num), loc)

              (*   12.34 < Y.feat   *)
              | (Ineq_float num, Ineq_sofi (Ast.Pointed (n1, f1))) ->
                Pat_const (Ast.Feature_ineq_cst (Ast.Gt, (n1,f1), num), loc)

              (*   X < Y   *)
              | (Ineq_sofi (Ast.Simple n1), Ineq_sofi (Ast.Simple n2)) ->
                Pat_const (Ast.Immediate_prec (n1,n2), loc)

(* TODO : axe lex_field *)

              (*  __ERRORS__   *)
              | (Ineq_float _, Ineq_float _) -> Error.build "the '<' symbol can be used with 2 constants"
              | _ -> Error.build "the '<' symbol can be used with 2 nodes or with 2 features but not in a mix inequality"
            }

        | id1_loc=ineq_value_with_loc GT id2=ineq_value
            { let (id1,loc)=id1_loc in
              match (id1, id2) with
              (*   X.feat > Y.feat   *)
              | (Ineq_sofi (Ast.Pointed (n1, f1)), Ineq_sofi (Ast.Pointed (n2, f2))) ->
                Pat_const (Ast.Features_ineq (Ast.Gt, (n1,f1), (n2,f2)), loc)

              (*   X.feat > 12.34   *)
              | (Ineq_sofi (Ast.Pointed (n1, f1)), Ineq_float num) ->
                Pat_const (Ast.Feature_ineq_cst (Ast.Gt, (n1,f1), num), loc)

              (*   12.34 > Y.feat   *)
              | (Ineq_float num, Ineq_sofi (Ast.Pointed (n1, f1))) ->
                Pat_const (Ast.Feature_ineq_cst (Ast.Lt, (n1,f1), num), loc)

              (*   X > Y   *)
              | (Ineq_sofi (Ast.Simple n1), Ineq_sofi (Ast.Simple n2)) ->
                Pat_const (Ast.Immediate_prec (n2,n1), loc)

(* TODO : axe lex_field *)

              (*  __ERRORS__   *)
              | (Ineq_float _, Ineq_float _) -> Error.build "the '>' symbol can be used with 2 constants"

              | (Ineq_float _, Ineq_float _) -> Error.build "the '>' symbol can be used with 2 constants"
              | _ -> Error.build "the '>' symbol can be used with 2 nodes or with 2 features but not in a mix inequality"
            }

        /*   X.position <= Y.position   */
        | feat_id1_loc=feature_ident_with_loc LE feat_id2=feature_ident
            { let (feat_id1,loc)=feat_id1_loc in Pat_const (Ast.Features_ineq (Ast.Le, feat_id1, feat_id2), loc) }

        /*   X.position >= Y.position   */
        | feat_id1_loc=feature_ident_with_loc GE feat_id2=feature_ident
            { let (feat_id1,loc)=feat_id1_loc in Pat_const (Ast.Features_ineq (Ast.Ge, feat_id1, feat_id2), loc) }

        /*   X.feat >= 12.34   */
        | feat_id1_loc=feature_ident_with_loc GE num=FLOAT
        | num=FLOAT LE feat_id1_loc=feature_ident_with_loc
            { let (feat_id1,loc)=feat_id1_loc in Pat_const (Ast.Feature_ineq_cst (Ast.Ge, feat_id1, num), loc)  }

        /*   X.feat <= 12.34   */
        | feat_id1_loc=feature_ident_with_loc LE num=FLOAT
        | num=FLOAT GE feat_id1_loc=feature_ident_with_loc
            { let (feat_id1,loc)=feat_id1_loc in Pat_const (Ast.Feature_ineq_cst (Ast.Le, feat_id1, num), loc)  }

        /*   A << B   */
        | n1_loc=simple_id_with_loc LPREC n2=simple_id
            { let (n1,loc) = n1_loc in Pat_const (Ast.Large_prec (n1,n2), loc) }

        /*   A >> B   */
        | n1_loc=simple_id_with_loc LSUCC n2=simple_id
            { let (n1,loc) = n1_loc in Pat_const (Ast.Large_prec (n2,n1), loc) }

node_features:
        /*   cat = n|v|adj   */
        | name_loc=simple_id_with_loc EQUAL values=separated_nonempty_list(PIPE,pattern_feature_value)
            {
              let (name,loc) = name_loc in
              let uname = Ast.to_uname name in
              match values with
              | [Ast.Simple "*"] ->
                Printf.printf "###node_features[1.1] --> %s\n%!" name;
                ({Ast.kind = Ast.Disequality []; name=uname},loc)
              | [Ast.Pointed (lex,fn)] ->
                Printf.printf "###node_features[1.2] --> %s\n%!" name;
                ({Ast.kind = Ast.Equal_lex (lex,fn); name=uname }, loc)
              | l ->
                Printf.printf "###node_features[1.3] --> %s\n%!" name;
                let value_list = List.map (function
                  | Ast.Simple x -> x
                  | Ast.Pointed (lex,fn) -> Error.build "Lexical reference '%s.%s' cannot be used in a disjunction" lex fn
                ) l in ({Ast.kind = Ast.Equality value_list; name=uname }, loc)
            }

        /*   cat = *   */
        | name_loc=simple_id_with_loc EQUAL STAR
            { let (name,loc) = name_loc in
              Printf.printf "###node_features[2] --> %s\n%!" name;
              ({Ast.kind = Ast.Disequality []; name=Ast.to_uname name},loc) }

        /*   cat   */
        | name_loc=simple_id_with_loc
            { let (name,loc) = name_loc in
              Printf.printf "###node_features[3] --> %s\n%!" name;
              ({Ast.kind = Ast.Disequality []; name=Ast.to_uname name},loc) }

        /*    cat<>n|v|adj   */
        | name_loc=simple_id_with_loc DISEQUAL values=separated_nonempty_list(PIPE,pattern_feature_value)
            {
              let (name,loc) = name_loc in
              let uname = Ast.to_uname name in
              match values with
              | [Ast.Pointed (lex,fn)] ->
                Printf.printf "###node_features[4.2] --> %s\n%!" name;
                ({Ast.kind = Ast.Disequal_lex (lex,fn); name=uname }, loc)
              | l ->
                Printf.printf "###node_features[4.3] --> %s\n%!" name;
                let value_list = List.map (function
                  | Ast.Simple x -> x
                  | Ast.Pointed (lex,fn) -> Error.build "Lexical reference '%s.%s' cannot be used in a disjunction" lex fn
                ) l in ({Ast.kind = Ast.Disequality value_list; name=uname }, loc)
            }

        /*    lemma=$lem   */
        | name_loc=simple_id_with_loc EQUAL p=DOLLAR_ID
            { let (name,loc) = name_loc in
              Printf.printf "###node_features[5] --> %s\n%!" name;
              ( {Ast.kind = Ast.Equal_param p; name=Ast.to_uname name }, loc) }

        /*   !lemma   */
        | BANG name_loc=simple_id_with_loc
            { let (name,loc) = name_loc in
                            Printf.printf "###node_features[6] --> %s\n%!" name;
 ({Ast.kind = Ast.Absent; name=Ast.to_uname name}, loc) }

/*=============================================================================================*/
/* COMMANDS DEFINITION                                                                         */
/*=============================================================================================*/
commands:
        | COMMANDS x=delimited(LACC,separated_nonempty_list_final_opt(SEMIC,command),RACC) { x }

command:
        /*   del_edge e   */
        | DEL_EDGE n_loc=simple_id_with_loc
            { let (n,loc) = n_loc in (Ast.Del_edge_name n, loc) }

        /*   del_edge m -[x]-> n   */
        | DEL_EDGE src_loc=simple_id_with_loc label=delimited(LTR_EDGE_LEFT,label_ident,LTR_EDGE_RIGHT) tar=simple_id
            { let (src,loc) = src_loc in (Ast.Del_edge_expl (src, tar, label), loc) }

        /*   add_edge m -[x]-> n   */
        | ADD_EDGE src_loc=simple_id_with_loc label=delimited(LTR_EDGE_LEFT,label_ident,LTR_EDGE_RIGHT) tar=simple_id
            { let (src,loc) = src_loc in (Ast.Add_edge (src, tar, label), loc) }

        /*   add_edge e: m -> n   */
        | ADD_EDGE id_loc=simple_id_with_loc DDOT src=simple_id EDGE tar=simple_id
            { let (id,loc) = id_loc in (Ast.Add_edge_expl (src, tar, id), loc) }

        /*   shift_in m ==> n   */
        | SHIFT_IN src_loc=simple_id_with_loc ARROW tar=simple_id
            { let (src,loc) = src_loc in (Ast.Shift_in (src, tar, Ast.Neg_list []), loc) }

        /*   shift_in m =[x*|y]=> n   */
        | SHIFT_IN src_loc=simple_id_with_loc
          labels=delimited(ARROW_LEFT,separated_nonempty_list(PIPE,pattern_label_ident),ARROW_RIGHT)
          tar=simple_id
            { let (src,loc) = src_loc in (Ast.Shift_in (src, tar, Ast.Pos_list labels), loc) }

        /*   shift_in m =[^x*|y]=> n   */
        | SHIFT_IN src_loc=simple_id_with_loc
          labels=delimited(ARROW_LEFT_NEG,separated_nonempty_list(PIPE,pattern_label_ident),ARROW_RIGHT)
          tar=simple_id
            { let (src,loc) = src_loc in (Ast.Shift_in (src, tar, Ast.Neg_list labels), loc) }

        /*   shift_out m ==> n   */
        | SHIFT_OUT src_loc=simple_id_with_loc ARROW tar=simple_id
            { let (src,loc) = src_loc in (Ast.Shift_out (src, tar, Ast.Neg_list []), loc) }

        /*   shift_out m =[x*|y]=> n   */
        | SHIFT_OUT src_loc=simple_id_with_loc
          labels=delimited(ARROW_LEFT,separated_nonempty_list(PIPE,pattern_label_ident),ARROW_RIGHT)
          tar=simple_id
            { let (src,loc) = src_loc in (Ast.Shift_out (src, tar, Ast.Pos_list labels), loc) }

        /*   shift_out m =[^x*|y]=> n   */
        | SHIFT_OUT src_loc=simple_id_with_loc
          labels=delimited(ARROW_LEFT_NEG,separated_nonempty_list(PIPE,pattern_label_ident),ARROW_RIGHT)
          tar=simple_id
            { let (src,loc) = src_loc in (Ast.Shift_out (src, tar, Ast.Neg_list labels), loc) }

        /*   shift m ==> n   */
        | SHIFT src_loc=simple_id_with_loc ARROW tar=simple_id
            { let (src,loc) = src_loc in (Ast.Shift_edge (src, tar, Ast.Neg_list []), loc) }

        /*   shift m =[x*|y]=> n   */
        | SHIFT src_loc=simple_id_with_loc
          labels=delimited(ARROW_LEFT,separated_nonempty_list(PIPE,pattern_label_ident),ARROW_RIGHT)
          tar=simple_id
            { let (src,loc) = src_loc in (Ast.Shift_edge (src, tar, Ast.Pos_list labels), loc) }

        /*   shift m =[^x*|y]=> n   */
        | SHIFT src_loc=simple_id_with_loc
          labels=delimited(ARROW_LEFT_NEG,separated_nonempty_list(PIPE,pattern_label_ident),ARROW_RIGHT)
          tar=simple_id
            { let (src,loc) = src_loc in (Ast.Shift_edge (src, tar, Ast.Neg_list labels), loc) }

        /*   del_node n   */
        | DEL_NODE ci_loc=simple_id_with_loc
            { let (ci,loc) = ci_loc in (Ast.Del_node (ci), loc) }

        /*   add_node n   */
        | ADD_NODE new_ci_loc=simple_id_with_loc
            { let (new_ci,loc) = new_ci_loc in (Ast.New_node new_ci, loc) }

        /*   add_node n :< m   */
        | ADD_NODE new_ci_loc=simple_id_with_loc BEFORE old_ci=simple_id
            { let (new_ci,loc) = new_ci_loc in (Ast.New_before (new_ci,old_ci), loc) }

        /*   add_node n :> m   */
        | ADD_NODE new_ci_loc=simple_id_with_loc AFTER old_ci=simple_id
            { let (new_ci,loc) = new_ci_loc in (Ast.New_after (new_ci,old_ci), loc) }

        /*   del_feat m.cat   */
        | DEL_FEAT com_fead_id_loc= feature_ident_with_loc
            { let (com_fead_id,loc) = com_fead_id_loc in (Ast.Del_feat com_fead_id, loc) }

        /*   m.cat = n.x + "_" + nn.y   */
        | com_fead_id_loc= feature_ident_with_loc EQUAL items=separated_nonempty_list (PLUS, concat_item)
            { let (com_fead_id,loc) = com_fead_id_loc in (Ast.Update_feat (com_fead_id, items), loc) }

concat_item:
        | gi=ID
          {
            match Ast.parse_simple_or_pointed gi with
            | Ast.Simple value -> Ast.String_item value
            | Ast.Pointed (s1, s2) -> Ast.Qfn_or_lex_item (s1, s2)
          }
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
        /*   seq_name { ant; p7_to_p7p-mc}   */
        | doc = option(COMMENT) id_loc=simple_id_with_loc mod_names=delimited(LACC,separated_list_final_opt(SEMIC,simple_id),RACC)
            { let (strat_name,strat_loc) = id_loc in
              {
                Ast.strat_name;
                strat_def = Ast.Sequence mod_names;
                strat_doc = begin match doc with Some d -> d | None -> [] end;
                strat_loc;
              }
            }

/*=============================================================================================*/
/* ISOLATED PATTERN (grep mode)                                                                */
/*=============================================================================================*/
pattern:
        | p=pos_item n=list(neg_item) EOF { Ast.complete_pattern {Ast.pat_pos=p; pat_negs=n} }

/*=============================================================================================*/
/* Constituent tree (à la Sequoia)                                                             */
/*=============================================================================================*/
phrase_structure_tree:
        | LPAREN t=pst RPAREN  { t }

pst:
        | LPAREN pos=ID ff=ID RPAREN  { Grew_ast.Ast.T (get_loc(), pos, [Grew_ast.Ast.Leaf (get_loc(), ff)])  }
        | LPAREN cat=ID daugthers=nonempty_list (pst) RPAREN { Grew_ast.Ast.T (get_loc(), cat, daugthers) }


/*=============================================================================================*/
/*=============================================================================================*/
/*=============================================================================================*/
/*=============================================================================================*/
/*=============================================================================================*/
/*=============================================================================================*/
/*=============================================================================================*/
/*=============================================================================================*/
/*=============================================================================================*/
/*=============================================================================================*/
/*=============================================================================================*/
/*=============================================================================================*/

new_grs:
  | decls = list(decl) EOF { decls }

decl:
  | f=feature_group                                          { New_ast.Features f                           }
  | l=labels                                                  { New_ast.Labels l                             }
  | r=rule                                                    { New_ast.Rule r                               }
  | IMPORT f=STRING                                           { New_ast.Import f                             }
  | INCL f=STRING                                             { New_ast.Include f                            }
  | PACKAGE id_loc=simple_id_with_loc LACC l=list(decl) RACC  { New_ast.Package (snd id_loc, fst id_loc, l)  }
  | STRAT id_loc=simple_id_with_loc LACC d = strat_desc RACC  { New_ast.Strategy (snd id_loc, fst id_loc, d) }

strat_desc:
  | id = node_id                                                           { New_ast.Ref id }
  | PICK LPAREN s=strat_desc RPAREN                                        { New_ast.Pick s }
  | ALT LPAREN sl=separated_list_final_opt(COMA,strat_desc) RPAREN         { New_ast.Alt sl }
  | SEQ LPAREN sl=separated_list_final_opt(COMA,strat_desc) RPAREN         { New_ast.Seq sl }
  | ITER LPAREN s=strat_desc RPAREN                                        { New_ast.Iter s }
  | IF LPAREN s1=strat_desc COMA s2=strat_desc COMA s3=strat_desc RPAREN   { New_ast.If (s1,s2,s3) }
  | TRY LPAREN s=strat_desc RPAREN                                         { New_ast.Try s }
  | ONF LPAREN s=strat_desc RPAREN                                         { New_ast.Onf s }
  | EMPTY                                                                  { New_ast.Seq [] }

strat_alone:
  | s = strat_desc EOF  { s }
%%
