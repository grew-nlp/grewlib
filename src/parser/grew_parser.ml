module Grew_parser = struct

	exception Parse_error of string


	(**
		[parse_string str] where [str] is a string following the grew syntax
		@param str the string to parse
		@return a syntactic tree of the parsed file
	*)
	let parse_string_to_grs str = 
		let to_parse = Lexing.from_string str in
		begin
		try
		  	Gr_grs_parser.grs Lexer.global to_parse
		with
			| Lexer.Error msg -> raise (Parse_error msg)
			| Gr_grs_parser.Error -> 
				let cp = to_parse.Lexing.lex_curr_p in
				 raise (Parse_error (Printf.sprintf "Syntax error\nLine %d : %s\n%!" cp.Lexing.pos_lnum (Lexing.lexeme to_parse)))
			| Failure msg -> 
				let cp = to_parse.Lexing.lex_curr_p in
				 raise (Parse_error (Printf.sprintf "Syntax error\nLine %d\n%s\n%!" cp.Lexing.pos_lnum msg))
			| err -> raise (Parse_error (Printexc.to_string err))
		end
		
	
		
	(**
		[parse_string file] where [file] is a file following the grew syntax
		@param file the file to parse
		@return a syntactic tree of the parsed file
	*)
	let parse_file_to_grs file = 
		try
			let in_ch = open_in file in
			let to_parse = Lexing.from_channel in_ch in
			begin
			try
				Parser_global.current_file := file;
			  	let res = Gr_grs_parser.grs Lexer.global to_parse in close_in in_ch; res
			with
				| Lexer.Error msg -> raise (Parse_error msg)
				| Gr_grs_parser.Error -> 
					let cp = to_parse.Lexing.lex_curr_p in
					 raise (Parse_error (Printf.sprintf "Syntax error\nFile %s\nLine %d : %s\n%!" file cp.Lexing.pos_lnum (Lexing.lexeme to_parse)))
				| Failure msg -> 
					let cp = to_parse.Lexing.lex_curr_p in
					 raise (Parse_error (Printf.sprintf "Syntax error\nFile %s\nLine %d\n%s\n%!" file cp.Lexing.pos_lnum msg))
				| err -> raise (Parse_error (Printexc.to_string err))
			end
		with Sys_error msg-> raise (Parse_error msg)



	let parse_string_to_gr str = 
		let to_parse = Lexing.from_string str in
		begin
		try
		  		Gr_grs_parser.gr Lexer.global to_parse
		with
			| Lexer.Error msg -> raise (Parse_error msg)
			| Gr_grs_parser.Error -> 
				let cp = to_parse.Lexing.lex_curr_p in
				 raise (Parse_error (Printf.sprintf "Syntax error\nLine %d : %s\n%!" cp.Lexing.pos_lnum (Lexing.lexeme to_parse)))
			| Failure msg -> 
				let cp = to_parse.Lexing.lex_curr_p in
				 raise (Parse_error (Printf.sprintf "Syntax error\nLine %d\n%s\n%!" cp.Lexing.pos_lnum msg))
			| err -> raise (Parse_error (Printexc.to_string err))
		end
	
	let parse_file_to_gr file = 
		try
			let in_ch = open_in file in
			let to_parse = Lexing.from_channel in_ch in

			begin
			try
				Parser_global.current_file := file;
			  	let res = Gr_grs_parser.gr Lexer.global to_parse in close_in in_ch; res
			with
				| Lexer.Error msg -> raise (Parse_error msg)
				| Gr_grs_parser.Error -> 
					let cp = to_parse.Lexing.lex_curr_p in
					 raise (Parse_error (Printf.sprintf "Syntax error\nFile %s\nLine %d : %s\n%!" file cp.Lexing.pos_lnum (Lexing.lexeme to_parse)))
				| Failure msg -> 
					let cp = to_parse.Lexing.lex_curr_p in
					 raise (Parse_error (Printf.sprintf "Syntax error\nFile %s\nLine %d\n%s\n%!" file cp.Lexing.pos_lnum msg))
				| err -> raise (Parse_error (Printexc.to_string err))
			end
		with Sys_error msg -> raise (Parse_error msg)


end
