(**********************************************************************************)
(*    Libcaml-grew - a Graph Rewriting library dedicated to NLP applications      *)
(*                                                                                *)
(*    Copyright 2011-2022 Inria, Université de Lorraine                           *)
(*                                                                                *)
(*    Webpage: https://grew.fr                                                    *)
(*    License: CeCILL (see LICENSE folder or "http://cecill.info/")               *)
(*    Authors: see AUTHORS file                                                   *)
(**********************************************************************************)

%{
open Grew_utils
open Grew_ast

(* Some intermediate sum types used in sub-functions when building the ast *)
type clause_item =
  | Clause_node of Ast.node
  | Clause_edge of Ast.edge
  | Clause_const of Ast.const

type ineq_item =
  | Ineq_sofi of Ast.simple_or_pointed
  | Ineq_float of float

type req_item =
  | Pattern of Ast.basic
  | Without of Ast.basic
  | With of Ast.basic
  | Global of Ast.glob list

let get_loc () = Global.get_loc ()
let localize t = (t,get_loc ())
%}

%token LACC                        /* { */
%token RACC                        /* } */
%token LBRACKET                    /* [ */
%token RBRACKET                    /* ] */
%token LPAREN                      /* ( */
%token RPAREN                      /* ) */
%token DDOT                        /* : */
%token COMMA                       /* , */
%token SEMIC                       /* ; */
%token PLUS                        /* + */
%token EQUAL                       /* = */
%token DISEQUAL                    /* <> */
%token BANG                        /* ! */
%token SLASH                       /* / */
%token STAR                        /* * */
%token LT                          /* < */
%token GT                          /* > */
%token LE                          /* <= or ≤ */
%token GE                          /* >= or ≥ */
%token LPREC                       /* << */
%token LSUCC                       /* >> */
%token CROSSING                    /* >< */

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

%token INCL                        /* include */
%token IMPORT                      /* import */
%token FROM                        /* from */
%token PATTERN                     /* request */
%token WITHOUT                     /* without */
%token WITH                        /* with */
%token COMMANDS                    /* commands */
%token GLOBAL                      /* global */
%token STRAT                       /* strat */
%token PACKAGE                     /* package */
%token RULE                        /* rule */

%token DEL_EDGE                    /* del_edge */
%token ADD_EDGE                    /* add_edge */
%token SHIFT_IN                    /* shift_in */
%token SHIFT_OUT                   /* shift_out */
%token SHIFT                       /* shift */
%token DEL_NODE                    /* del_node */
%token ADD_NODE                    /* add_node */
%token DEL_FEAT                    /* del_feat */
%token APPEND_FEATS                /* append_feats */
%token PREPEND_FEATS               /* append_feats */
%token UNORDER                     /* unorder */
%token INSERT                      /* insert */

%token PICK                        /* Pick */
%token ALT                         /* Alt */
%token SEQ                         /* Seq */
%token ITER                        /* Iter */
%token IF                          /* If */
%token ONF                         /* Onf */
%token EMPTY                       /* Empty */
%token TRY                         /* Try */

%token <string> ID   /* the general notion of id */

/* %token <Grew_ast.Ast.complex_id>   COMPLEX_ID*/

%token <string>                STRING
%token <string>                REGEXP
%token <float>                 FLOAT
%token <int>                   INT
%token <string list>           COMMENT
%token <string * (int *string) list>  LEX_PAR

%token EOF                         /* end of file */

%start <Grew_ast.Ast.basic> basic
%start <Grew_ast.Ast.request> isolated_request

%start <Grew_ast.Ast.grs> grs
%start <Grew_ast.Ast.strat> strat_alone


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
number:
        | f=FLOAT     { f }
        | i=INT       { float_of_int i }

label_ident:
        | x=ID        { Ast.parse_label_ident x }
        | s=STRING    { s }

simple_id:
        | id=ID       { Ast.parse_simple_ident id }

simple_id_with_loc:
        | id=ID       { localize (Ast.parse_simple_ident id) }

simple_id_or_float:
        | id=ID       { Ast.parse_simple_ident id }
        | v=number     { Printf.sprintf "%g" v }

node_id:
        | id=ID       { Ast.parse_node_ident id }

feature_ident :
        | id=ID       { Ast.parse_feature_ident id }

feature_ident_with_loc :
        | id=ID      { localize (Ast.parse_feature_ident id) }

feature_value:
        | v=ID        { Ast.parse_simple_ident v }
        | v=STRING    { v }
        | v=number    { Printf.sprintf "%g" v }

simple_or_pointed :
        | id=ID       { Ast.parse_simple_or_pointed id }

simple_or_pointed_with_loc :
        | id=ID       { localize (Ast.parse_simple_or_pointed id) }

request_feature_value:
        | v=ID        { Ast.parse_simple_or_pointed v }
        | v=STRING    { Ast.Simple v }
        | v=number    { Ast.Simple (Printf.sprintf "%g" v) }

ineq_value:
        | v=ID    { Ineq_sofi (Ast.parse_simple_or_pointed v) }
        | v=number{ Ineq_float v }

ineq_value_with_loc:
        | v=ID    { localize (Ineq_sofi (Ast.parse_simple_or_pointed v)) }
        | v=number{ localize (Ineq_float v) }

/*=============================================================================================*/
/* RULES DEFINITION                                                                            */
/*=============================================================================================*/
rule:
        | doc=option(COMMENT) RULE id_loc=simple_id_with_loc file_lexicons = option(external_lexicons) LACC req=request cmds=commands RACC final_lexicons=list(final_lexicon)
            {
              let lexicons = match file_lexicons with
              | Some l -> l @ final_lexicons
              | None -> final_lexicons in
              { Ast.rule_id = fst id_loc;
                request = req;
                commands = cmds;
                lexicon_info = lexicons;
                rule_doc = begin match doc with Some d -> d | None -> [] end;
                rule_loc = snd id_loc;
                rule_dir = None;
                rule_path = "";
              }
            }

external_lexicons:
        | LPAREN external_lexicons= separated_nonempty_list_final_opt(COMMA, external_lexicon) RPAREN       { external_lexicons }

external_lexicon:
        | lex_id=simple_id FROM file=STRING { (lex_id, Ast.File file)}

final_lexicon:
        | final_lexicon = LEX_PAR  { (fst final_lexicon, Ast.Final (snd final_lexicon)) }

glob_decl:
        | GLOBAL l=delimited(LACC, separated_list_final_opt(SEMIC,glob_item),RACC) { l }

glob_item:
        /*   is_projective   */
        | item_loc = simple_id_with_loc { let (item,loc)=item_loc in (Ast.Glob_cst item, loc) }
        /*   text = v1|v2   */
        | meta_loc=simple_id_with_loc EQUAL values=separated_nonempty_list(PIPE,feature_value)
          { let (meta_id,loc)=meta_loc in (Ast.Glob_eq_list (meta_id, values), loc) }
        /*   text <> value   */
        | meta_loc=simple_id_with_loc DISEQUAL values=separated_nonempty_list(PIPE,feature_value)
          { let (meta_id,loc)=meta_loc in (Ast.Glob_diff_list (meta_id, values), loc) }
        /*   text = *   */
        | meta_loc=simple_id_with_loc EQUAL STAR
          { let (meta_id,loc)=meta_loc in (Ast.Glob_diff_list (meta_id, []), loc) }
        /*   !text   */
        | BANG meta_loc=simple_id_with_loc
          { let (meta_id,loc)=meta_loc in (Ast.Glob_absent meta_id, loc) }
        /*   text = re"pref.*"  */
        | meta_loc=simple_id_with_loc EQUAL re=REGEXP
          { let (meta_id,loc)=meta_loc in (Ast.Glob_regexp (meta_id, re), loc) }


pos_item:
        | PATTERN i=basic { i }

neg_filter:
        | WITHOUT i=basic { i }

pos_filter:
        | WITH i=basic { i }

basic:
        | l=delimited(LACC,separated_list_final_opt(SEMIC,clause_item),RACC)
            {
             {
              Ast.req_nodes = CCList.filter_map (function Clause_node n -> Some n | _ -> None) l;
              Ast.req_edges = CCList.filter_map (function Clause_edge n -> Some n | _ -> None) l;
              Ast.req_const = CCList.filter_map (function Clause_const n -> Some n | _ -> None) l;
            }
           }

/*=============================================================================================*/
/* PATTERN DEFINITION                                                                          */
/*=============================================================================================*/

edge_item:
        | id=ID       { Ast.parse_node_ident id }
        | v=STRING    { v }
        | v=number    { Printf.sprintf "%g" v }

label_atom:
        | name=simple_id_or_float EQUAL l=separated_nonempty_list(PIPE,edge_item) { Ast.Atom_eq (name, l)}
        | name=simple_id_or_float EQUAL STAR { Ast.Atom_diseq (name, [])}
        | name=simple_id_or_float DISEQUAL l=separated_nonempty_list(PIPE,edge_item) { Ast.Atom_diseq (name, l)}
        | BANG name=simple_id_or_float { Ast.Atom_absent name }

clause_item:
        /* =================================== */
        /* node                                */
        /* =================================== */
        /*   R [cat=V, lemma=$lemma]   */
        | id_loc=simple_id_with_loc feats=delimited(LBRACKET,separated_list_final_opt(COMMA,node_features),RBRACKET)
            { Clause_node ({Ast.node_id = fst id_loc; fs= feats}, snd id_loc) }

        /* =================================== */
        /* edge                                */
        /* =================================== */
        /*   A -> B   */
        | n1_loc=simple_id_with_loc EDGE n2=simple_id
            { let (n1,loc) = n1_loc in Clause_edge ({Ast.edge_id = None; src=n1; edge_label_cst=Ast.Neg_list []; tar=n2}, loc) }

        /*   A -[X|Y]-> B   */
        | n1_loc=simple_id_with_loc labels=delimited(LTR_EDGE_LEFT,separated_nonempty_list(PIPE,label_ident),LTR_EDGE_RIGHT) n2=simple_id
            { let (n1,loc) = n1_loc in Clause_edge ({Ast.edge_id = None; src=n1; edge_label_cst=Ast.Pos_list labels; tar=n2}, loc) }

        /*   A -[^X|Y]-> B   */
        | n1_loc=simple_id_with_loc labels=delimited(LTR_EDGE_LEFT_NEG,separated_nonempty_list(PIPE,label_ident),LTR_EDGE_RIGHT) n2=simple_id
            { let (n1,loc) = n1_loc in Clause_edge ({Ast.edge_id = None; src=n1; edge_label_cst=Ast.Neg_list labels; tar=n2}, loc) }

        /*   A -[re"regexp"]-> B   */
        | n1_loc=simple_id_with_loc LTR_EDGE_LEFT re=REGEXP LTR_EDGE_RIGHT n2=simple_id
            { let (n1,loc) = n1_loc in Clause_edge ({Ast.edge_id = None; src=n1; edge_label_cst=Ast.Regexp re; tar=n2}, loc) }

        /*   A -[1=subj, 2=*, !3]-> B   */
        | n1_loc=simple_id_with_loc LTR_EDGE_LEFT atom_list = separated_nonempty_list(COMMA, label_atom) LTR_EDGE_RIGHT n2=simple_id
            { let (n1,loc) = n1_loc in Clause_edge ({Ast.edge_id = None; src=n1; edge_label_cst=Ast.Atom_list atom_list; tar=n2}, loc) }

        /*   e:A -[1=subj, 2=*, !3]-> B   */
        | id_loc=simple_id_with_loc DDOT n1=simple_id LTR_EDGE_LEFT atom_list = separated_nonempty_list(COMMA, label_atom) LTR_EDGE_RIGHT n2=simple_id
            { let (id,loc) = id_loc in Clause_edge ({Ast.edge_id = Some id; src=n1; edge_label_cst=Ast.Atom_list atom_list; tar=n2}, loc) }

        /*   e: A -> B   */
        | id_loc=simple_id_with_loc DDOT n1=simple_id EDGE n2=simple_id
            { let (id,loc) = id_loc in Clause_edge ({Ast.edge_id = Some id; src=n1; edge_label_cst=(Ast.Neg_list []); tar=n2}, loc) }

        /*   e: A -[X|Y]-> B   */
        | id_loc=simple_id_with_loc DDOT n1=simple_id labels=delimited(LTR_EDGE_LEFT,separated_nonempty_list(PIPE,label_ident),LTR_EDGE_RIGHT) n2=simple_id
            { let (id,loc) = id_loc in Clause_edge ({Ast.edge_id = Some id; src=n1; edge_label_cst=(Ast.Pos_list labels); tar=n2}, loc) }

        /*   e: A -[^X|Y]-> B   */
        | id_loc=simple_id_with_loc DDOT n1=simple_id labels=delimited(LTR_EDGE_LEFT_NEG,separated_nonempty_list(PIPE,label_ident),LTR_EDGE_RIGHT) n2=simple_id
            { let (id,loc) = id_loc in Clause_edge ({Ast.edge_id = Some id; src=n1; edge_label_cst=(Ast.Neg_list labels); tar=n2}, loc) }

        /*   e: A -[re"regexp"]-> B   */
        | id_loc=simple_id_with_loc DDOT n1=simple_id LTR_EDGE_LEFT re=REGEXP LTR_EDGE_RIGHT n2=simple_id
            { let (id,loc) = id_loc in Clause_edge ({Ast.edge_id = Some id; src=n1; edge_label_cst=Ast.Regexp re; tar=n2}, loc) }

        /* =================================== */
        /* edge constraints                    */
        /* =================================== */

        /*   A -[X|Y]-> *   */
        | n1_loc=simple_id_with_loc labels=delimited(LTR_EDGE_LEFT,separated_nonempty_list(PIPE,label_ident),LTR_EDGE_RIGHT) STAR
            { let (n1,loc) = n1_loc in Clause_const (Ast.Cst_out (n1,Ast.Pos_list labels), loc) }

        /*   * -[X|Y]-> B   */
        | STAR labels=delimited(LTR_EDGE_LEFT,separated_nonempty_list(PIPE,label_ident),LTR_EDGE_RIGHT) n2_loc=simple_id_with_loc
            { let (n2,loc) = n2_loc in Clause_const (Ast.Cst_in (n2,Ast.Pos_list labels), loc) }

        /*   A -> *   */
        | n1_loc=simple_id_with_loc EDGE STAR
            { let (n1,loc) = n1_loc in Clause_const (Ast.Cst_out (n1,Ast.Neg_list []), loc) }

        /*   * -> B   */
        | STAR EDGE n2_loc=simple_id_with_loc
            { let (n2,loc) = n2_loc in Clause_const (Ast.Cst_in (n2,Ast.Neg_list []), loc) }

        /*   A -[^X|Y]-> *   */
        | n1_loc=simple_id_with_loc labels=delimited(LTR_EDGE_LEFT_NEG,separated_nonempty_list(PIPE,label_ident),LTR_EDGE_RIGHT) STAR
            { let (n1,loc) = n1_loc in Clause_const (Ast.Cst_out (n1,Ast.Neg_list labels), loc) }

        /*   * -[^X|Y]-> B   */
        | STAR labels=delimited(LTR_EDGE_LEFT_NEG,separated_nonempty_list(PIPE,label_ident),LTR_EDGE_RIGHT) n2_loc=simple_id_with_loc
            { let (n2,loc) = n2_loc in Clause_const (Ast.Cst_in (n2,Ast.Neg_list labels), loc) }

        /*   A -[1=subj, 2=*, !3]-> *   */
        | n1_loc=simple_id_with_loc atom_list = delimited(LTR_EDGE_LEFT, separated_nonempty_list(COMMA, label_atom), LTR_EDGE_RIGHT) STAR
            { let (n1,loc) = n1_loc in Clause_const (Ast.Cst_out (n1,Ast.Atom_list atom_list), loc) }

        /*   * -[1=subj, 2=*, !3]-> B   */
        | STAR atom_list=delimited(LTR_EDGE_LEFT,separated_nonempty_list(COMMA, label_atom),LTR_EDGE_RIGHT) n2_loc=simple_id_with_loc
            { let (n2,loc) = n2_loc in Clause_const (Ast.Cst_in (n2,Ast.Atom_list atom_list), loc) }

        /*   A -[re"mod.*"]-> *   */
        | n1_loc=simple_id_with_loc LTR_EDGE_LEFT re=REGEXP LTR_EDGE_RIGHT STAR
            { let (n1,loc) = n1_loc in Clause_const (Ast.Cst_out (n1,Ast.Regexp re), loc) }

        /*   * -[re"mod.*"]-> B   */
        | STAR LTR_EDGE_LEFT re=REGEXP LTR_EDGE_RIGHT n2_loc=simple_id_with_loc
            { let (n2,loc) = n2_loc in Clause_const (Ast.Cst_in (n2,Ast.Regexp re), loc) }


        /* =================================== */
        /* other constraints                   */
        /* =================================== */

        /*   X.cat = Y.cat   */
        /*   X.cat = value   */
        /*   X.cat = lex.value   */
        | feat_id1_loc=feature_ident_with_loc EQUAL rhs=ID
             { let (feat_id1,loc)=feat_id1_loc in
              match Ast.parse_simple_or_pointed rhs with
              | Ast.Simple value ->
                Clause_const (Ast.Feature_cmp_value (Eq,feat_id1, String value), loc)
              | Ast.Pointed (s1, s2) ->
                Clause_const (Ast.Feature_cmp (Eq, feat_id1, (s1, s2)), loc)
             }

        /*   X.cat = "value"   */
        | feat_id1_loc=feature_ident_with_loc EQUAL rhs=STRING
            { let (feat_id1,loc)=feat_id1_loc in Clause_const (Ast.Feature_cmp_value (Eq, feat_id1, String rhs), loc) }

        /*   X.cat = 12.34   */
        | feat_id1_loc=feature_ident_with_loc EQUAL rhs=number
            { let (feat_id1,loc)=feat_id1_loc in Clause_const (Ast.Feature_cmp_value (Eq, feat_id1, Float rhs), loc) }

        /*   X.cat <> value   */
        /*   X.cat <> Y.cat   */
        /*   X.cat <> lex.value   */
        /*   e1 <> e2   */
        | lhs_loc=simple_or_pointed_with_loc DISEQUAL rhs =simple_or_pointed
             {  match (lhs_loc,rhs) with
              | ((Ast.Pointed feat_id,loc), Ast.Simple value) ->
                Clause_const (Ast.Feature_cmp_value (Neq, feat_id, String value), loc)
              | ((Ast.Pointed feat_id,loc), Ast.Pointed (s1, s2)) ->
                Clause_const (Ast.Feature_cmp (Neq, feat_id, (s1, s2)), loc)
              | ((Ast.Simple edge_id1,loc), Ast.Simple edge_id2) ->
                Clause_const (Ast.Edge_disjoint (edge_id1, edge_id2), loc)
              | ((_,loc),_) -> Error.build ~loc "syntax error in constraint"
             }

        /* NB: the three next clauses use [simple_or_pointed_with_loc] instead of [feature_ident_with_loc] to avoid a Menhir conflict */
        /*   X.cat <> "value"   */
        | lhs_loc=simple_or_pointed_with_loc DISEQUAL rhs=STRING
            { match lhs_loc with
              | (Ast.Pointed feat_id, loc) -> Clause_const (Ast.Feature_cmp_value (Neq, feat_id, String rhs), loc)
              | (_,loc) -> Error.build ~loc "syntax error in constraint"
            }

        /*   X.cat <> 12.34   */
        | lhs_loc=simple_or_pointed_with_loc DISEQUAL rhs=number
            { match lhs_loc with
              | (Ast.Pointed feat_id, loc) -> Clause_const (Ast.Feature_cmp_value (Neq, feat_id, Float rhs), loc)
              | (_,loc) -> Error.build ~loc "syntax error in constraint"
            }

        /*   X.cat <> re"regexp"   */
        | lhs_loc=simple_or_pointed_with_loc DISEQUAL regexp=REGEXP
            { match lhs_loc with
              | (Ast.Pointed feat_id, loc) -> Clause_const (Ast.Feature_cmp_regexp (Neq, feat_id, regexp), loc)
              | (_,loc) -> Error.build ~loc "syntax error in constraint"
            }

        /*   X.cat = re"regexp"   */
        | feat_id_loc=feature_ident_with_loc EQUAL regexp=REGEXP
            { let (feat_id,loc)=feat_id_loc in Clause_const (Ast.Feature_cmp_regexp (Eq, feat_id, regexp), loc) }

        /*   X.feat < Y.feat    */
        /*   X < Y     */
        | id1_loc=ineq_value_with_loc LT id2=ineq_value
            { let (id1,loc)=id1_loc in
              match (id1, id2) with
              (*   X.feat < Y.feat   *)
              | (Ineq_sofi (Ast.Pointed (n1, f1)), Ineq_sofi (Ast.Pointed (n2, f2))) ->
                Clause_const (Ast.Feature_ineq (Ast.Lt, (n1,f1), (n2,f2)), loc)

              (*   X.feat < 12.34   *)
              | (Ineq_sofi (Ast.Pointed (n1, f1)), Ineq_float num) ->
                Clause_const (Ast.Feature_ineq_cst (Ast.Lt, (n1,f1), num), loc)

              (*   12.34 < Y.feat   *)
              | (Ineq_float num, Ineq_sofi (Ast.Pointed (n1, f1))) ->
                Clause_const (Ast.Feature_ineq_cst (Ast.Gt, (n1,f1), num), loc)

              (*   X < Y   *)
              | (Ineq_sofi (Ast.Simple n1), Ineq_sofi (Ast.Simple n2)) ->
                 Clause_edge ({Ast.edge_id = None; src=n1; edge_label_cst=Ast.Pred; tar=n2}, loc)

 (* TODO : axe lex_field *)

              (*  __ERRORS__   *)
              | (Ineq_float _, Ineq_float _) -> Error.build "the '<' symbol can be used with 2 constants"
              | _ -> Error.build "the '<' symbol can be used with 2 nodes or with 2 features but not in a mix inequality"
            }

        /*   X.feat > Y.feat    */
        /*   X > Y     */
        | id1_loc=ineq_value_with_loc GT id2=ineq_value
            { let (id1,loc)=id1_loc in
              match (id1, id2) with
              (*   X.feat > Y.feat   *)
              | (Ineq_sofi (Ast.Pointed (n1, f1)), Ineq_sofi (Ast.Pointed (n2, f2))) ->
                Clause_const (Ast.Feature_ineq (Ast.Gt, (n1,f1), (n2,f2)), loc)

              (*   X.feat > 12.34   *)
              | (Ineq_sofi (Ast.Pointed (n1, f1)), Ineq_float num) ->
                Clause_const (Ast.Feature_ineq_cst (Ast.Gt, (n1,f1), num), loc)

              (*   12.34 > Y.feat   *)
              | (Ineq_float num, Ineq_sofi (Ast.Pointed (n1, f1))) ->
                Clause_const (Ast.Feature_ineq_cst (Ast.Lt, (n1,f1), num), loc)

              (*   X > Y   *)
              | (Ineq_sofi (Ast.Simple n1), Ineq_sofi (Ast.Simple n2)) ->
                Clause_edge ({Ast.edge_id = None; src=n2; edge_label_cst=Ast.Pred; tar=n1}, loc)

(* TODO : axe lex_field *)

              (*  __ERRORS__   *)
              | (Ineq_float _, Ineq_float _) -> Error.build "the '>' symbol can be used with 2 constants"
              | _ -> Error.build "the '>' symbol can be used with 2 nodes or with 2 features but not in a mix inequality"
            }

        /*   X.position <= Y.position   */
        | feat_id1_loc=feature_ident_with_loc LE feat_id2=feature_ident
            { let (feat_id1,loc)=feat_id1_loc in Clause_const (Ast.Feature_ineq (Ast.Le, feat_id1, feat_id2), loc) }

        /*   X.position >= Y.position   */
        | feat_id1_loc=feature_ident_with_loc GE feat_id2=feature_ident
            { let (feat_id1,loc)=feat_id1_loc in Clause_const (Ast.Feature_ineq (Ast.Ge, feat_id1, feat_id2), loc) }

        /*   X.feat >= 12.34   */
        | feat_id1_loc=feature_ident_with_loc GE num=number
        | num=number LE feat_id1_loc=feature_ident_with_loc
            { let (feat_id1,loc)=feat_id1_loc in Clause_const (Ast.Feature_ineq_cst (Ast.Ge, feat_id1, num), loc)  }

        /*   X.feat <= 12.34   */
        | feat_id1_loc=feature_ident_with_loc LE num=number
        | num=number GE feat_id1_loc=feature_ident_with_loc
            { let (feat_id1,loc)=feat_id1_loc in Clause_const (Ast.Feature_ineq_cst (Ast.Le, feat_id1, num), loc)  }

        /*   A << B   */
        | n1_loc=simple_id_with_loc LPREC n2=simple_id
            { let (n1,loc) = n1_loc in Clause_const (Ast.Large_prec (n1,n2), loc) }

        /*   A >> B   */
        | n1_loc=simple_id_with_loc LSUCC n2=simple_id
            { let (n1,loc) = n1_loc in Clause_const (Ast.Large_prec (n2,n1), loc) }

        /*   e1 >< e2   */
        | n1_loc=simple_id_with_loc CROSSING n2=simple_id
            { let (n1,loc) = n1_loc in Clause_const (Ast.Edge_crossing (n1,n2), loc) }

        /* Next items are temporarily kept for producing dedicated error message when using out of date syntax  /*

        /*   id(N) < id(M)   */
        | id1_loc=simple_id_with_loc LPAREN simple_id RPAREN LT id2=simple_id LPAREN simple_id RPAREN
            { let (id1,loc) = id1_loc in
              match (id1, id2) with
              | ("id", "id") -> Error.build ~loc "The syntax `id(N) < id(M)` is no more available, see [Grew doc](https://grew.fr/trans_14)";
              | ("id", n) | (n, "id") -> Error.build ~loc "Unexpected operator '%s'" n
              | (n, m) -> Error.build ~loc "Unexpected operators '%s' and '%s'" n m
            }

        /*   id(N) > id(M)   */
        | id1_loc=simple_id_with_loc LPAREN simple_id RPAREN GT id2=simple_id LPAREN simple_id RPAREN
            { let (id1,loc) = id1_loc in
              match (id1, id2) with
              | ("id", "id") -> Error.build ~loc "The syntax `id(N) > id(M)` is no more available, see [Grew doc](https://grew.fr/trans_14)";
              | ("id", n) | (n, "id") -> Error.build ~loc "Unexpected operator '%s'" n
              | (n, m) -> Error.build ~loc "Unexpected operators '%s' and '%s'" n m
            }

        /*  DEPRECATED  label(e1) = label(e2)   */
        | id1_loc=simple_id_with_loc LPAREN simple_id RPAREN EQUAL id2=simple_id LPAREN simple_id RPAREN
            { let (id1,loc) = id1_loc in
              match (id1, id2) with
              | ("label", "label") -> Error.build ~loc "The syntax `label(N) = label(M)` is no more available, see [Grew doc](https://grew.fr/trans_14)";
              | ("label", n) | (n, "label") -> Error.build ~loc "Unexpected operator '%s'" n
              | (n, m) -> Error.build ~loc "Unexpected operators '%s' and '%s'" n m
            }

        /*   label(e1) <> label(e2)   */
        /* don't want to make "label" a keyword, matched as a ident and control the value */
        | id1_loc=simple_id_with_loc LPAREN simple_id RPAREN DISEQUAL id2=simple_id LPAREN simple_id RPAREN
            { let (id1,loc) = id1_loc in
              match (id1, id2) with
              | ("label", "label") -> Error.build ~loc "The syntax `label(N) <> label(M)` is no more available, see [Grew doc](https://grew.fr/trans_14)";
              | ("label", n) | (n, "label") -> Error.build ~loc "Unexpected operator '%s'" n
              | (n, m) -> Error.build ~loc "Unexpected operators '%s' and '%s'" n m
            }
/*** end clause_item ***/




node_features:
        /*   cat = n|v|adj   */
        | name_loc=simple_id_with_loc EQUAL values=separated_nonempty_list(PIPE,request_feature_value)
            {
              let (name,loc) = name_loc in
              match values with
              | [Ast.Simple "*"] ->
                ({Ast.kind = Ast.Feat_kind_list (Neq,[]); name},loc)
              | [Ast.Pointed (lex,fn)] ->
                ({Ast.kind = Ast.Feat_kind_lex (Eq,lex,fn); name }, loc)
              | l ->
                let value_list = List.map (function
                  | Ast.Simple x -> x
                  | Ast.Pointed (lex,fn) -> Error.build "Lexical reference '%s.%s' cannot be used in a disjunction" lex fn
                ) l in ({Ast.kind = Ast.Feat_kind_list (Eq,value_list); name }, loc)
            }

        /*   cat = *   */
        | name_loc=simple_id_with_loc EQUAL STAR
            { let (name,loc) = name_loc in
              ({Ast.kind = Ast.Feat_kind_list (Neq,[]); name},loc) }

        /*   cat   */
        | name_loc=simple_id_with_loc
            { let (name,loc) = name_loc in
              ({Ast.kind = Ast.Feat_kind_list (Neq,[]); name},loc) }

        /*    cat<>n|v|adj   */
        | name_loc=simple_id_with_loc DISEQUAL values=separated_nonempty_list(PIPE,request_feature_value)
            {
              let (name,loc) = name_loc in
              match values with
              | [Ast.Pointed (lex,fn)] ->
                ({Ast.kind = Ast.Feat_kind_lex (Neq, lex,fn); name }, loc)
              | l ->
                let value_list = List.map (function
                  | Ast.Simple x -> x
                  | Ast.Pointed (lex,fn) -> Error.build "Lexical reference '%s.%s' cannot be used in a disjunction" lex fn
                ) l in ({Ast.kind = Ast.Feat_kind_list (Neq,value_list); name }, loc)
            }

        /*   form = re".*ing"   */
        | name_loc=simple_id_with_loc EQUAL regexp=REGEXP
            { let (name,loc) = name_loc in
              ({Ast.kind = Ast.Feat_kind_re (Eq, regexp); name},loc) }

        /*   form <> re".*ing"   */
        | name_loc=simple_id_with_loc DISEQUAL regexp=REGEXP
            { let (name,loc) = name_loc in
              ({Ast.kind = Ast.Feat_kind_re (Neq, regexp); name},loc) }

        /*   !lemma   */
        | BANG name_loc=simple_id_with_loc
            { let (name,loc) = name_loc in ({Ast.kind = Ast.Absent; name}, loc) }

        /*   mwepos=ADV/upos=ADV   */
        | name1_loc=simple_id_with_loc EQUAL fv1=feature_value SLASH name2=simple_id EQUAL fv2=feature_value
            { let (name1,loc) = name1_loc in ({Ast.kind = Ast.Else (fv1,name2,fv2); name= name1}, loc) }


/*=============================================================================================*/
/* COMMANDS DEFINITION                                                                         */
/*=============================================================================================*/
commands:
        | COMMANDS x=delimited(LACC,separated_list_final_opt(SEMIC,command),RACC) { x }

sub_edges:
        | name=simple_id_or_float EQUAL value=edge_item { (name, value) }

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

        /*   add_edge m -[1=obj, 2=e.2]-> n   */
        | ADD_EDGE src_loc=simple_id_with_loc label=delimited(LTR_EDGE_LEFT,separated_list_final_opt(COMMA,sub_edges),LTR_EDGE_RIGHT) tar=simple_id
            { let (src,loc) = src_loc in (Ast.Add_edge_items (src, tar, label), loc) }

        /*   shift_in m ==> n   */
        | SHIFT_IN src_loc=simple_id_with_loc ARROW tar=simple_id
            { let (src,loc) = src_loc in (Ast.Shift_in (src, tar, Ast.Neg_list []), loc) }

        /*   shift_in m =[x|y]=> n   */
        | SHIFT_IN src_loc=simple_id_with_loc
          labels=delimited(ARROW_LEFT,separated_nonempty_list(PIPE,label_ident),ARROW_RIGHT)
          tar=simple_id
            { let (src,loc) = src_loc in (Ast.Shift_in (src, tar, Ast.Pos_list labels), loc) }

        /*   shift_in m =[^x|y]=> n   */
        | SHIFT_IN src_loc=simple_id_with_loc
          labels=delimited(ARROW_LEFT_NEG,separated_nonempty_list(PIPE,label_ident),ARROW_RIGHT)
          tar=simple_id
            { let (src,loc) = src_loc in (Ast.Shift_in (src, tar, Ast.Neg_list labels), loc) }

        /*   shift_in m =[1=subj, 2=*, !3]=> n   */
        | SHIFT_IN src_loc=simple_id_with_loc atom_list=delimited(ARROW_LEFT, separated_nonempty_list(COMMA, label_atom), ARROW_RIGHT) tar=simple_id
            { let (src,loc) = src_loc in (Ast.Shift_in (src, tar, Ast.Atom_list atom_list), loc) }

        /*   shift_out m ==> n   */
        | SHIFT_OUT src_loc=simple_id_with_loc ARROW tar=simple_id
            { let (src,loc) = src_loc in (Ast.Shift_out (src, tar, Ast.Neg_list []), loc) }

        /*   shift_out m =[x|y]=> n   */
        | SHIFT_OUT src_loc=simple_id_with_loc
          labels=delimited(ARROW_LEFT,separated_nonempty_list(PIPE,label_ident),ARROW_RIGHT)
          tar=simple_id
            { let (src,loc) = src_loc in (Ast.Shift_out (src, tar, Ast.Pos_list labels), loc) }

        /*   shift_out m =[^x|y]=> n   */
        | SHIFT_OUT src_loc=simple_id_with_loc
          labels=delimited(ARROW_LEFT_NEG,separated_nonempty_list(PIPE,label_ident),ARROW_RIGHT)
          tar=simple_id
            { let (src,loc) = src_loc in (Ast.Shift_out (src, tar, Ast.Neg_list labels), loc) }

        /*   shift_out m =[1=subj, 2=*, !3]=> n   */
        | SHIFT_OUT src_loc=simple_id_with_loc atom_list=delimited(ARROW_LEFT, separated_nonempty_list(COMMA, label_atom), ARROW_RIGHT) tar=simple_id
            { let (src,loc) = src_loc in (Ast.Shift_out (src, tar, Ast.Atom_list atom_list), loc) }

        /*   shift m ==> n   */
        | SHIFT src_loc=simple_id_with_loc ARROW tar=simple_id
            { let (src,loc) = src_loc in (Ast.Shift_edge (src, tar, Ast.Neg_list []), loc) }

        /*   shift m =[x|y]=> n   */
        | SHIFT src_loc=simple_id_with_loc
          labels=delimited(ARROW_LEFT,separated_nonempty_list(PIPE,label_ident),ARROW_RIGHT)
          tar=simple_id
            { let (src,loc) = src_loc in (Ast.Shift_edge (src, tar, Ast.Pos_list labels), loc) }

        /*   shift m =[^x|y]=> n   */
        | SHIFT src_loc=simple_id_with_loc
          labels=delimited(ARROW_LEFT_NEG,separated_nonempty_list(PIPE,label_ident),ARROW_RIGHT)
          tar=simple_id
            { let (src,loc) = src_loc in (Ast.Shift_edge (src, tar, Ast.Neg_list labels), loc) }

        /*   shift m =[1=subj, 2=*, !3]=> n   */
        | SHIFT src_loc=simple_id_with_loc atom_list=delimited(ARROW_LEFT, separated_nonempty_list(COMMA, label_atom), ARROW_RIGHT) tar=simple_id
            { let (src,loc) = src_loc in (Ast.Shift_edge (src, tar, Ast.Atom_list atom_list), loc) }

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

        /*   append_feats M ==> N   */
        | APPEND_FEATS src_loc=simple_id_with_loc ARROW tar=simple_id
            { let (src,loc) = src_loc in (Ast.Concat_feats (Append, src, tar, ".*", ""), loc) }

        /*   prepend_feats M ==> N   */
        | PREPEND_FEATS src_loc=simple_id_with_loc ARROW tar=simple_id
            { let (src,loc) = src_loc in (Ast.Concat_feats (Prepend, src, tar, ".*", ""), loc) }

        /*   append_feats "+" M ==> N   */
        | APPEND_FEATS sep=STRING src_loc=simple_id_with_loc ARROW tar=simple_id
            { let (src,loc) = src_loc in (Ast.Concat_feats (Append, src, tar, ".*", sep), loc) }

        /*   prepend_feats "+" M ==> N   */
        | PREPEND_FEATS sep=STRING src_loc=simple_id_with_loc ARROW tar=simple_id
            { let (src,loc) = src_loc in (Ast.Concat_feats (Prepend, src, tar, ".*", sep), loc) }

        /*   append_feats M =[re"_MISC_.*"]=> N   */
        | APPEND_FEATS src_loc=simple_id_with_loc ARROW_LEFT regexp=REGEXP ARROW_RIGHT tar=simple_id
            { let (src,loc) = src_loc in (Ast.Concat_feats (Append, src, tar, regexp, ""), loc) }

        /*   prepend_feats M =[re"_MISC_.*"]=> N   */
        | PREPEND_FEATS src_loc=simple_id_with_loc ARROW_LEFT regexp=REGEXP ARROW_RIGHT tar=simple_id
            { let (src,loc) = src_loc in (Ast.Concat_feats (Prepend, src, tar, regexp, ""), loc) }

        /*   append_feats "+" M =[re"_MISC_.*"]=> N   */
        | APPEND_FEATS sep=STRING src_loc=simple_id_with_loc ARROW_LEFT regexp=REGEXP ARROW_RIGHT tar=simple_id
            { let (src,loc) = src_loc in (Ast.Concat_feats (Append, src, tar, regexp, sep), loc) }

        /*   append_feats "+" M =[re"_MISC_.*"]=> N   */
        | PREPEND_FEATS sep=STRING src_loc=simple_id_with_loc ARROW_LEFT regexp=REGEXP ARROW_RIGHT tar=simple_id
            { let (src,loc) = src_loc in (Ast.Concat_feats (Prepend, src, tar, regexp, sep), loc) }

        /*   unorder N   */
        | UNORDER node_id_loc=simple_id_with_loc
            { let (node_id,loc) = node_id_loc in (Ast.Unorder node_id, loc) }

        /*   insert n :< m   */
        | INSERT ci1_loc=simple_id_with_loc BEFORE ci2=simple_id
            { let (ci1,loc) = ci1_loc in (Ast.Insert_before (ci1,ci2), loc) }

        /*   insert n :> m   */
        | INSERT ci1_loc=simple_id_with_loc AFTER ci2=simple_id
            { let (ci1,loc) = ci1_loc in (Ast.Insert_after (ci1,ci2), loc) }


concat_item:
        | gi=ID
          {
            match Ast.parse_simple_or_pointed gi with
            | Ast.Simple value -> Ast.String_item (value, (None,None))
            | Ast.Pointed (s1, s2) -> Ast.Qfn_or_lex_item ((s1, s2),(None,None))
          }
        | gi=ID r=range /* Python style substring */
          {
            match Ast.parse_simple_or_pointed gi with
            | Ast.Simple value -> Ast.String_item (value, r)
            | Ast.Pointed (s1, s2) -> Ast.Qfn_or_lex_item ((s1, s2), r)
          }
        | s=STRING         { Ast.String_item (s, (None,None)) }
        | f=number         { Ast.String_item (Printf.sprintf "%g" f, (None,None)) }

range:
        | LBRACKET x=INT DDOT y=INT RBRACKET  { (Some x, Some y) }
        | LBRACKET DDOT y=INT RBRACKET        { (None, Some y) }
        | LBRACKET x=INT DDOT RBRACKET        { (Some x, None) }
        | LBRACKET DDOT RBRACKET              { (None, None) }
        
/*=============================================================================================*/
/* ISOLATED PATTERN (grep mode)                                                                */
/*=============================================================================================*/
isolated_request:
        | r=request EOF { r }

request:
  | l = list (request_item)
    {
      let pos = List.fold_left
        (fun acc i ->
          match i with
          | Pattern i -> Ast.concat_basic i acc
          | _ -> acc
          ) Ast.empty_basic l in
      let external_lexicons = List.fold_left
        (fun acc i ->
          match i with
          | Without i -> (i, false) :: acc
          | With i -> (i, true) :: acc
          | _ -> acc
          ) [] l in
      let glob = List.fold_left
        (fun acc i ->
          match i with
          | Global i -> i @ acc
          | _ -> acc
          ) [] l in
      Ast.complete_and_check_request {
          Ast.req_glob = glob;
          Ast.req_pos = pos;
          Ast.req_exts = external_lexicons;
        }
    }

request_item:
  | i=pos_item  { Pattern i }
  | i=neg_filter  { Without i }
  | i=pos_filter  { With i }
  | i=glob_decl { Global i }

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

grs:
  | decls = list(decl) EOF { decls }

decl:
  | r=rule                                                    { Ast.Rule r                               }
  | IMPORT f=STRING                                           { Ast.Import f                             }
  | INCL f=STRING                                             { Ast.Include f                            }
  | PACKAGE id_loc=simple_id_with_loc LACC l=list(decl) RACC  { Ast.Package (snd id_loc, fst id_loc, l)  }
  | STRAT id_loc=simple_id_with_loc LACC d = strat_desc RACC  { Ast.Strategy (snd id_loc, fst id_loc, d) }

strat_desc:
  | id = node_id                                                           { Ast.Ref id        }
  | PICK LPAREN s=strat_desc RPAREN                                        { Ast.Pick s        }
  | ALT LPAREN sl=separated_list_final_opt(COMMA,strat_desc) RPAREN        { Ast.Alt sl        }
  | SEQ LPAREN sl=separated_list_final_opt(COMMA,strat_desc) RPAREN        { Ast.Seq sl        }
  | ITER LPAREN s=strat_desc RPAREN                                        { Ast.Iter s        }
  | IF LPAREN s1=strat_desc COMMA s2=strat_desc COMMA s3=strat_desc RPAREN { Ast.If (s1,s2,s3) }
  | TRY LPAREN s=strat_desc RPAREN                                         { Ast.Try s         }
  | ONF LPAREN s=strat_desc RPAREN                                         { Ast.Onf s         }
  | EMPTY                                                                  { Ast.Seq []        }

strat_alone:
  | s = strat_desc EOF  { s }
%%
