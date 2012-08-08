open Grew_ast

module Html :
  sig
    val proceed : dep:bool -> string -> string -> Ast.grs -> unit
  end
