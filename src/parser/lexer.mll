
{
  open Printf
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

  let split_comment com =
    let raw = Str.split (Str.regexp "\n") com in
    List.filter (fun l -> not (Str.string_match (Str.regexp "[ \t]*$") l 0)) raw

  let lp_buff = Buffer.create 32
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
    COMMENT(split_comment comment)
}

and comment_multi target = parse
| "*/" { target lexbuf }
| '\n' { incr Parser_global.current_line; Lexing.new_line lexbuf; comment_multi target lexbuf }
| _  { comment_multi target lexbuf }

and string_lex target = parse
| "\\" { escaped := true; tmp_string := !tmp_string^"\\"; string_lex target lexbuf }
| '\n' { incr Parser_global.current_line; Lexing.new_line lexbuf; tmp_string := !tmp_string^"\n"; string_lex target lexbuf }
| '\"' { if !escaped then (tmp_string := !tmp_string^"\"";  escaped := false; string_lex target lexbuf) else ( STRING(!tmp_string) ) }
| _ as c { escaped := false; tmp_string := !tmp_string^(sprintf "%c" c); string_lex target lexbuf }

and lp_lex target = parse
| '\n'                    { incr Parser_global.current_line; Lexing.new_line lexbuf; bprintf lp_buff "\n"; lp_lex target lexbuf }
| _ as c                  { bprintf lp_buff "%c" c; lp_lex target lexbuf }
| "#END" [' ' '\t']* '\n' { incr Parser_global.current_line; LP (Str.split (Str.regexp "\n") (Buffer.contents lp_buff)) }

and global = parse
| [' ' '\t'] { global lexbuf }

| "%--"      { comment_multi_doc global lexbuf }
| "/*"       { comment_multi global lexbuf }
| '%'        { comment global lexbuf }

| "#BEGIN" [' ' '\t']* '\n' { incr Parser_global.current_line; Buffer.clear lp_buff; lp_lex global lexbuf}

| '\n'       { incr Parser_global.current_line; Lexing.new_line lexbuf; global lexbuf}


| "include"     { INCLUDE }
| "features"    { FEATURES }
| "feature"     { FEATURE }
| "file"        { FILE }
| "labels"      { LABELS }
| "new_nodes"   { NEW_NODES }
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
| "filter"      { FILTER }
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
| '#'   { SHARP }
| '='   { EQUAL }
| "<>"  { DISEQUAL }

| "<"        { LT }
| ">"        { GT }
| "<=" | "≤" { LE }
| ">=" | "≥" { GE }

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
| _ as c { raise (Error (sprintf "At line %d: unexpected character '%c'" (lexbuf.Lexing.lex_start_p.Lexing.pos_lnum) c)) }



