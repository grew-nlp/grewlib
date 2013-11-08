
{
  open Printf
  open Log
  open Grew_ast
  open Gr_grs_parser

  exception Error of string

  let tmp_string = ref ""
  let escaped = ref false

  let parse_couple string =
    match Str.split (Str.regexp "\\.") string with
    | [s1; s2] -> (s1, s2)
    | _ -> Log.fcritical "[BUG] \"%s\" is not a couple" string

  let parse_ext_ident string =
    match Str.split (Str.regexp "#") string with
    | [base; ext] -> (base, Some ext)
    | _ -> Log.fcritical "[BUG] \"%s\" is not an extented ident" string

  let parse_ext_qfn string =
    match Str.split (Str.regexp "\\.") string with
    | [ext_ident; feat_name] -> (parse_ext_ident ext_ident, feat_name)
    | _ -> Log.fcritical "[BUG] \"%s\" is not an extented qfn" string




  let split_comment com =
    let raw = Str.split (Str.regexp "\n") com in
    List.filter (fun l -> not (Str.string_match (Str.regexp "[ \t]*$") l 0)) raw

  let lp_buff = Buffer.create 32
}

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
(* an identifier is either a single letter, "_", or its lenght is >=2 and it doesn't end with a '-' *)
let ident = (letter | '_') | (letter | '_') (letter | digit | '_' | '\'' | '-')* (letter | digit | '_' | '\'')

let hex = ['0'-'9' 'a'-'f' 'A'-'F']
let color = hex hex hex hex hex hex

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
| ident ['.'] ident as c   { QFN (parse_couple c) }
| ident as id              { IDENT id }
| '$' ident as pat_var     { DOLLAR_ID pat_var}
| '@' ident as cmd_var     { AROBAS_ID cmd_var }
| "@#" color as col        { COLOR col }

| ident '#' ident as ext_ident              { EXT_IDENT (parse_ext_ident ext_ident) }
| ident '#' ident ['.'] ident as ext_qfn    { EXT_QFN (parse_ext_qfn ext_qfn) }


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



