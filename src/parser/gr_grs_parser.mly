%{

open Grew_ast
open Grew_utils

(* Some intermediate sum types used in sub-functions when building the ast *)
type pat_item = 
  | Pat_node of Ast.node
  | Pat_edge of Ast.edge
  | Pat_const of Ast.const

type graph_item =
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
%token PLUS                        /* + */
%token EQUAL                       /* = */
%token DISEQUAL                    /* <> */
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
%token LABELS                      /* labels */
%token BAD_LABELS                  /* bad_labels */
%token MATCH                       /* match */
%token WITHOUT                     /* without */
%token COMMANDS                    /* commands */
%token MODULE                      /* module */
%token CONFLUENT                   /* confluent */
%token RULE                        /* rule */
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

%token <string>  IDENT             /* indentifier */
%token <Grew_ast.Ast.qfn> QFN               /* ident.ident */
%token <string>  STRING
%token <int>     INT
%token <string>  COMMENT

%token EOF                         /* end of file */

%start <Grew_ast.Ast.grs_with_include> grs_with_include
%start <Grew_ast.Ast.grs> grs
%start <Grew_ast.Ast.gr> gr
%start <Grew_ast.Ast.module_or_include list> included
%%

/*=============================================================================================*/
/*  GREW GRAPH                                                                                 */
/*=============================================================================================*/

gr: 
        | GRAPH LACC items = separated_list(SEMIC,option(gr_item)) RACC EOF 
            {
             {
              Ast.nodes = List_.opt_map (function Some (Graph_node n) -> Some n | _ -> None) items;
              Ast.edges = List_.opt_map (function Some (Graph_edge n) -> Some n | _ -> None) items;
            }
           }
                  
gr_item:
        | id = IDENT position = option(delimited(LPAREN,index,RPAREN)) feats = delimited(LBRACKET,separated_list(COMA,node_features),RBRACKET) 
            { Graph_node (localize {Ast.node_id = id; position=position; fs=feats}) }

        | n1 = IDENT labels = delimited(LTR_EDGE_LEFT_NEG,separated_nonempty_list(PIPE,IDENT),LTR_EDGE_RIGHT) n2 = IDENT
            { Graph_edge (localize {Ast.edge_id = None; src=n1; edge_labels=labels; tar=n2; negative=true; }) }

        | n1 = IDENT labels = delimited(LTR_EDGE_LEFT,separated_nonempty_list(PIPE,IDENT),LTR_EDGE_RIGHT) n2 = IDENT
            { Graph_edge (localize {Ast.edge_id = None; src=n1; edge_labels=labels; tar=n2; negative=false; }) }

index:
        | INT { $1 }










/*=============================================================================================*/
/*  GREW GRAPH SYSTEM REWRITING                                                                */
/*=============================================================================================*/


grs_with_include:
        | f = features_group g = global_labels m = module_or_include_list s = sequences EOF 
            {
             { Ast.domain_wi=List_.opt f; 
               labels_wi=g; 
               modules_wi=m; 
               sequences_wi=s;
             }
           }
     
grs:
        | f = features_group g = global_labels m = modules s = sequences EOF 
            {
             { Ast.domain=List_.opt f; 
               labels=g; 
               modules=m; 
               sequences=s;
             }
           }
       
module_or_include_list:
        | x = list(module_or_include) { x }

module_or_include:
        | m = grew_module        { Ast.Modul m }
        | INCLUDE sub = subfile SEMIC { Ast.Includ sub } 

subfile:
        | f = STRING  { localize f }

/*=============================================================================================*/
/*                                                                                             */
/* FEATURES DOMAIN DEFINITION                                                                  */
/*                                                                                             */
/* features {                                                                                  */
/*      cat: n, np, v, adj;                                                                    */
/*      mood: inf, ind, subj, pastp, presp;                                                    */
/*      lemma: *; phon: *                                                                      */
/* }                                                                                           */
/*                                                                                             */
/*=============================================================================================*/

features_group:
        | FEATURES x = features { x }
        
%inline features:
        | LACC x = separated_nonempty_list(SEMIC,option(feature)) RACC { x }
        
%inline feature:
        | name = feature_name DDOT values = features_values 
            { 
              if List.length values == 1 && List.hd values = "*"
              then Ast.Open name 
              else Ast.Closed (name,List.sort Pervasives.compare values)
            }

feature_name:
        | word = IDENT { word }

features_values:
        | STAR { ["*"] }
        | x = separated_nonempty_list(COMA,feature_value) { x }


/*=============================================================================================*/
/*                                                                                             */
/* GLOBAL LABELS DEFINITION                                                                    */
/*                                                                                             */
/* labels { OBJ, SUBJ, DE_OBJ, ANT }                                                           */
/*                                                                                             */
/*=============================================================================================*/

%inline labels:
        | x = delimited(LACC,separated_nonempty_list(COMA,label),RACC) { x }
        
%inline label:
        | x = IDENT color = option(ddot_color)  { (x, color) }

ddot_color:
        | DDOT color = IDENT { color }

global_labels:
        | LABELS x = labels { x }


/*=============================================================================================*/
/*                                                                                             */
/* MODULE DEFINITION                                                                           */
/*                                                                                             */
/* module p7_to_p7p-mc {                                                                       */
/*   ...                                                                                       */
/* }                                                                                           */
/*                                                                                             */
/*=============================================================================================*/

included:
        | x = list (module_or_include) EOF { x }

modules:
        | x = list(grew_module) { x }
        
grew_module: 
        | doc = option(module_doc) MODULE conf = boption(CONFLUENT) id = module_id LACC l = option(local_labels) b = option(local_bad_labels) r = option(rules) RACC 
           {
            { Ast.module_id = fst id; 
              local_labels = begin match l with None -> [] | Some l -> l; end;
              bad_labels = begin match b with None -> [] | Some b -> b; end;
              rules = begin match r with None -> [] | Some r -> r; end;
              confluent = conf;
              module_doc = (match doc with Some d -> d | None -> "");
              mod_loc = (!Parser_global.current_file, snd id); 
            }
          }

module_id:
        | id = IDENT { (id,!Parser_global.current_line+1) }

module_doc:
        | comment = COMMENT { comment } 

/*=============================================================================================*/
/*                                                                                             */
/* LOCAL LABELS DEFINITION                                                                     */
/*                                                                                             */
/* labels {ANT_TMP}                                                                            */
/*                                                                                             */
/* bad_labels { SUJ, OBJ }                                                                     */
/*=============================================================================================*/



local_labels:
        | LABELS x = labels { x }

local_bad_labels:
        | BAD_LABELS x = bad_labels { x }
         
%inline bad_labels:
        | x = delimited(LACC,separated_nonempty_list(COMA,IDENT),RACC) { x }



/*=============================================================================================*/
/*                                                                                             */
/* RULES DEFINITION                                                                            */
/*                                                                                             */
/* rule ant_prorel_init {                                                                      */
/*   ...                                                                                       */
/* }                                                                                           */
/*=============================================================================================*/

rules:
        | r = list(rule) { r }

rule: 
        | doc = option(rule_doc) RULE id = rule_id LACC p = pos_item n = list(neg_item) cmds = commands RACC 
            { 
              { Ast.rule_id = fst id; 
                pos_pattern = p;
                neg_patterns = n;
                commands = List_.opt cmds; 
                rule_doc = begin match doc with Some d -> d | None -> "" end;
                rule_loc = (!Parser_global.current_file,snd id);
              }
            }         
            
pos_item:
        | MATCH i = pn_item { i }

neg_item:
        | WITHOUT i = pn_item { i }
          
pn_item: 
        | l = delimited(LACC,separated_nonempty_list(SEMIC,option(pat_item)),RACC)
            {
             {
              Ast.pat_nodes = List_.opt_map (function Some (Pat_node n) -> Some n | _ -> None) l;
              Ast.pat_edges = List_.opt_map (function Some (Pat_edge n) -> Some n | _ -> None) l;
              Ast.pat_const = List_.opt_map (function Some (Pat_const n) -> Some n | _ -> None) l;
            }
           }
        
rule_id:
        | id = IDENT { (id,!Parser_global.current_line+1) }

rule_doc:
        | comment = COMMENT { comment } 
        
/*=============================================================================================*/
/*                                                                                             */
/* MATCH DEFINITION                                                                            */
/*                                                                                             */
/* match {                                                                                     */
/*      P [cat = prorel];                                                                      */
/*      N -> P;                                                                                */
/*                                                                                             */
/*      without { P -[ANT]-> * }                                                               */
/*      without { P -[ANT_TMP]-> * }                                                           */
/*    }                                                                                        */
/*                                                                                             */
/*=============================================================================================*/


pat_item:
        | n = pat_node { Pat_node n }
        | e = pat_edge { Pat_edge e }
        | c = pat_const { Pat_const c }

pat_node:
        | id = IDENT feats = delimited(LBRACKET,separated_list(COMA,node_features),RBRACKET) 
            { localize ({Ast.node_id = id; position=None; fs= feats}) }




node_features:
        | name = IDENT EQUAL STAR 
            { localize {Ast.kind = Ast.Disequality; name=name; values=[]; } } 
        | name = IDENT EQUAL values = separated_nonempty_list(PIPE,feature_value) 
            { localize {Ast.kind = Ast.Equality; name=name; values=values; } } 
        | name = IDENT DISEQUAL values = separated_nonempty_list(PIPE,feature_value) 
            { localize {Ast.kind = Ast.Disequality; name=name; values=values; } } 

feature_value:
        | v = IDENT { v }
        | v = STRING { v }
        | v = INT { string_of_int v }

pat_edge:
        (* "e: A -> B" OR "e: A -[*]-> B" *)
        | id = edge_id n1 = IDENT GOTO_NODE n2 = IDENT
        | id = edge_id n1 = IDENT LTR_EDGE_LEFT_NEG STAR LTR_EDGE_RIGHT n2 = IDENT
               { localize ({Ast.edge_id = Some id; src=n1; edge_labels=[]; tar=n2; negative=true}) }

        (* "A -> B" *)
        | n1 = IDENT GOTO_NODE n2 = IDENT
               { localize ({Ast.edge_id = None; src=n1; edge_labels=[]; tar=n2; negative=true}) }

        (* "e: A -[^X|Y]-> B" *)
        | id = edge_id n1 = IDENT labels = delimited(LTR_EDGE_LEFT_NEG,separated_nonempty_list(PIPE,IDENT),LTR_EDGE_RIGHT) n2 = IDENT
            { localize ({Ast.edge_id = Some id; src=n1; edge_labels=labels; tar=n2; negative=true}) }
            
        (* "A -[^X|Y]-> B"*)
        | n1 = IDENT labels = delimited(LTR_EDGE_LEFT_NEG,separated_nonempty_list(PIPE,IDENT),LTR_EDGE_RIGHT) n2 = IDENT
            { localize ({Ast.edge_id = None; src=n1; edge_labels=labels; tar=n2; negative=true}) }

        (* "e: A -[X|Y]-> B" *)
        | id = edge_id n1 = IDENT labels = delimited(LTR_EDGE_LEFT,separated_nonempty_list(PIPE,IDENT),LTR_EDGE_RIGHT) n2 = IDENT
            { localize ({Ast.edge_id = Some id; src=n1; edge_labels=labels; tar=n2; negative=false}) }       

        (* "A -[X|Y]-> B" *)
        | n1 = IDENT labels = delimited(LTR_EDGE_LEFT,separated_nonempty_list(PIPE,IDENT),LTR_EDGE_RIGHT) n2 = IDENT
            { localize ({Ast.edge_id = None; src=n1; edge_labels=labels; tar=n2; negative=false}) }


edge_id:
        | id = IDENT DDOT { id }
                

pat_const:
        (* "A -[X|Y]-> *" *)
        | n1 = IDENT labels = delimited(LTR_EDGE_LEFT,separated_nonempty_list(PIPE,IDENT),LTR_EDGE_RIGHT) STAR
            { localize (Ast.Start (n1,labels)) }

        (* "A -> *" *)
        | n1 = IDENT GOTO_NODE STAR
            { localize (Ast.No_out n1) }

        (* "* -[X|Y]-> A" *)
        | STAR labels = delimited(LTR_EDGE_LEFT,separated_nonempty_list(PIPE,IDENT),LTR_EDGE_RIGHT) n2 = IDENT
            { localize (Ast.End (n2,labels)) }

        (* "* -> A" *)
        | STAR GOTO_NODE n2 = IDENT
            { localize (Ast.No_in n2) }

        | qfn1 = QFN EQUAL qfn2 = QFN
            { localize (Ast.Feature_eq (qfn1, qfn2)) }

/*=============================================================================================*/
/*                                                                                             */
/* COMMANDS DEFINITION                                                                         */
/*                                                                                             */
/* commands {                                                                                  */
/*      del_edge A -[OBJ]-> B;                                                                 */
/*      del_edge e;                                                                            */
/*                                                                                             */
/*      add_edge A -[ATS]-> B;                                                                 */
/*                                                                                             */
/*      shift A ==> B;                                                                         */
/*                                                                                             */
/*      del_node A;                                                                            */
/*                                                                                             */
/*      add_node D: <-[SUJ]- A;                                                                */
/*                                                                                             */
/*      A.mood = inf;                                                                          */
/*                                                                                             */
/*      B.tense = A.tense;                                                                     */
/*                                                                                             */
/*      del_feat A.tense;                                                                      */
/* }                                                                                           */
/*                                                                                             */
/*=============================================================================================*/


commands:
        | COMMANDS x = delimited(LACC,separated_nonempty_list(SEMIC,option(command)),RACC) { x }

command:
        | DEL_EDGE n = IDENT
            { localize (Ast.Del_edge_name n) }
        | DEL_EDGE n1 = IDENT label = delimited(LTR_EDGE_LEFT,IDENT,LTR_EDGE_RIGHT) n2 = IDENT 
            { localize (Ast.Del_edge_expl (n1,n2,label)) }
        | ADD_EDGE n1 = IDENT label = delimited(LTR_EDGE_LEFT,IDENT,LTR_EDGE_RIGHT) n2 = IDENT
            { localize (Ast.Add_edge (n1,n2,label)) }
        | SHIFT_IN n1 = IDENT LONGARROW n2 = IDENT 
            { localize (Ast.Shift_in (n1,n2)) }
        | SHIFT_OUT n1 = IDENT LONGARROW n2 = IDENT 
            { localize (Ast.Shift_out (n1,n2)) }
        | SHIFT n1 = IDENT LONGARROW n2 = IDENT 
            { localize (Ast.Shift_edge (n1,n2)) }
        | MERGE n1 = IDENT LONGARROW n2 = IDENT 
            { localize (Ast.Merge_node (n1,n2)) }
        | DEL_NODE n = IDENT 
            { localize (Ast.Del_node n) }
        | ADD_NODE n1 = IDENT DDOT label = delimited(RTL_EDGE_LEFT,IDENT,RTL_EDGE_RIGHT) n2 = IDENT 
            { localize (Ast.New_neighbour (n1,n2,label)) }
        | DEL_FEAT qfn = QFN 
            { localize (Ast.Del_feat qfn) }
        | qfn = QFN EQUAL items = separated_nonempty_list (PLUS, concat_item)
            { localize (Ast.Update_feat (qfn, items)) }

concat_item:
        | qfn = QFN  { Ast.Qfn_item qfn }
        | s = IDENT   { Ast.String_item s }
        | s = STRING   { Ast.String_item s }

/*=============================================================================================*/
/*                                                                                             */
/* SEQUENCE DEFINITION                                                                         */
/*                                                                                             */
/* sequence { ant; p7_to_p7p-mc}                                                               */
/*                                                                                             */
/*=============================================================================================*/

sequences:
        | SEQUENCES seq = delimited(LACC,list(sequence),RACC) { seq }

sequence:
        | doc = option(COMMENT) id = sequence_id mod_names = delimited(LACC,separated_nonempty_list(SEMIC,option( IDENT)),RACC) 
            { 
              { Ast.seq_name = fst id; 
                seq_mod = List_.opt mod_names ; 
                seq_doc = begin match doc with Some d -> d | None -> "" end; 
                seq_loc = (!Parser_global.current_file,snd id);
              } 
            }

sequence_id:
        | id = IDENT { (id,!Parser_global.current_line+1) }

%%
