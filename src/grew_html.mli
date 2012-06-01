open Grew_ast

module Html :
  sig
    val proceed : string -> string -> Ast.grs -> unit
  end
