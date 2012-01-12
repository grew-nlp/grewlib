
{
  open Log
  open Grew_ast
  open Gr_grs_parser

  exception Error of string
  
  let tmp_string = ref ""
  let escaped = ref false

  let parse_qfn string_feat = 
    match Str.split (Str.regexp "\\.") string_feat with
    | [node; feat_name] -> (node, feat_name)
    | _ -> Log.fcritical "[BUG] \"%s\" is not a feature" string_feat
}

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']

(* an identifier is either a single letter or its lenght is >=2 and it doesn't end with a '-' *)
let ident = letter | (letter | '_') (letter | digit | '_' | '\'' | '-')* (letter | digit | '_' | '\'')

rule comment target = parse
| '\n' { incr Parser_global.current_line; Lexing.new_line lexbuf; target lexbuf }
| _  { comment target lexbuf }

and comment_multi_doc target = shortest
| (_* as comment)"--%" {
  let start = ref 0 in
  try while (Str.search_forward (Str.regexp "\n") comment !start != -1) do
    start := Str.match_end ();
    incr Parser_global.current_line; 
    Lexing.new_line lexbuf;
  done; assert false
  with Not_found ->
    COMMENT(comment) 
}
 
and comment_multi target = parse
| "*/" { target lexbuf }
| '\n' { incr Parser_global.current_line; Lexing.new_line lexbuf; comment_multi target lexbuf }
| _  { comment_multi target lexbuf }
    
and string_lex target = parse
| "\\" { escaped := true; tmp_string := !tmp_string^"\\"; string_lex target lexbuf }
| '\n' { incr Parser_global.current_line; Lexing.new_line lexbuf; tmp_string := !tmp_string^"\n"; string_lex target lexbuf }
| '\"' { if !escaped then (tmp_string := !tmp_string^"\"";  escaped := false; string_lex target lexbuf) else ( STRING(!tmp_string) ) }
| _ as c { escaped := false; tmp_string := !tmp_string^(Printf.sprintf "%c" c); string_lex target lexbuf }
    
    
and global = parse
| [' ' '\t'] { global lexbuf }
    
| "%--"      { comment_multi_doc global lexbuf }
| "/*"       { comment_multi global lexbuf }
| '%'        { comment global lexbuf }

| '\n'       { incr Parser_global.current_line; Lexing.new_line lexbuf; global lexbuf}


| "include"     { INCLUDE }
| "features"    { FEATURES }
| "feature"     { FEATURE }
| "file"        { FILE }
| "labels"      { LABELS }
| "bad_labels"  { BAD_LABELS }
| "match"       { MATCH }
| "without"     { WITHOUT }
| "commands"    { COMMANDS }

| "add_edge"    { ADD_EDGE }
| "del_edge"    { DEL_EDGE }
| "shift_in"    { SHIFT_IN }
| "shift_out"   { SHIFT_OUT }
| "shift"       { SHIFT }
| "merge"       { MERGE }
| "del_node"    { DEL_NODE }
| "add_node"    { ADD_NODE }
| "del_feat"    { DEL_FEAT }

| "module"      { MODULE }
| "confluent"   { CONFLUENT }
| "rule"        { RULE }
| "lex_rule"    { LEX_RULE }
| "sequences"   { SEQUENCES }

| "graph"       { GRAPH }

| digit+ as number         { INT (int_of_string number) }
| ident ['.'] ident as qfn { QFN (parse_qfn qfn) }
| ident as id              { IDENT id }
| '$' ident as pat_var     { PAT pat_var}
| '@' ident as cmd_var     { CMD cmd_var }

| '{'   { LACC }
| '}'   { RACC }
| '['   { LBRACKET }
| ']'   { RBRACKET }
| '('   { LPAREN }
| ')'   { RPAREN }
| ':'   { DDOT }
| ';'   { SEMIC }
| ','   { COMA }
| '+'   { PLUS }
| '*'   { STAR }
| '='   { EQUAL }
| "<>"  { DISEQUAL }
| '|'   { PIPE }
| "->"  { GOTO_NODE }
| "-[^" { LTR_EDGE_LEFT_NEG }
| "-["  { LTR_EDGE_LEFT }
| "]->" { LTR_EDGE_RIGHT }
| "<-[" { RTL_EDGE_LEFT }
| "]-"  { RTL_EDGE_RIGHT }
| "==>" { LONGARROW }
| '"'   { tmp_string := ""; string_lex global lexbuf }



| eof   { EOF }
| _ as c { raise (Error (Printf.sprintf "At line %d: unexpected character '%c'.\n" (lexbuf.Lexing.lex_start_p.Lexing.pos_lnum) c)) }



