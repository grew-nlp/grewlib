open Grew_ast

module Html :
  sig
    val proceed: dep:bool -> string -> string -> Ast.grs -> unit

    val html_sentences: string -> (string option * int * string) list -> unit
  end
