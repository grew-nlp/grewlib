
{
  open Gr_grs_parser

  exception Error of string
  
  let tmp_string = ref ""
  let escaped = ref false
}

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let ident = (letter | '_') (letter | digit | '_' | '\'' | '-')* 

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
| _ as c { tmp_string := !tmp_string^(Printf.sprintf "%c" c); string_lex target lexbuf }


and global = parse
| [' ' '\t'] { global lexbuf }

| "%--"  { comment_multi_doc global lexbuf }
| "/*"  { comment_multi global lexbuf }
| '%'  { comment global lexbuf }

| '\n' { incr Parser_global.current_line; Lexing.new_line lexbuf; global lexbuf}


| "features"	{ FEATURES }
| "labels"	{ LABELS }
| "bad_labels" { BAD_LABELS }
| "match"	{ MATCH }
| "without"	{ WITHOUT }
| "commands"	{ COMMANDS }

| "add_edge"	{ ADD_EDGE }
| "del_edge"	{ DEL_EDGE }
| "shift"	{ SHIFT }
| "merge"	{ MERGE }
| "del_node"	{ DEL_NODE }
| "add_node"	{ ADD_NODE }
| "del_feat"	{ DEL_FEAT }

| "module"	{ MODULE }
| "confluent"	{ CONFLUENT }
| "rule"	{ RULE }
| "sequences"   { SEQUENCES }

| "graph" { GRAPH }

| digit+ as number { NUMBER (int_of_string number) }
| ident ['.'] ident as feat { FEAT feat }

| ident as id { IDENT id }

| '{'	{ LACC }
| '}'	{ RACC }
| '['	{ LBRACKET }
| ']'	{ RBRACKET }
| '('	{ LPAREN }
| ')'	{ RPAREN }
| ':'	{ DDOT }
| ';'	{ SEMIC }
| ','	{ COMA }
| '*'	{ STAR }
| '='	{ EQUAL }
| "<>"	{ DISEQUAL }
| '|'	{ PIPE }
| "->"	{ GOTO_NODE }
| "-[^"	{ LTR_EDGE_LEFT_NEG }
| "-["	{ LTR_EDGE_LEFT }
| "]->"	{ LTR_EDGE_RIGHT }
| "<-["	{ RTL_EDGE_LEFT }
| "]-"	{ RTL_EDGE_RIGHT }
| "==>" { LONGARROW }
| '"'	{ tmp_string := ""; string_lex global lexbuf }



| eof	{ EOF }
| _ as c { raise (Error (Printf.sprintf "At line %d: unexpected character '%c'.\n" (lexbuf.Lexing.lex_start_p.Lexing.pos_lnum) c)) }



