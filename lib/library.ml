module TAst = TypedAst
module Sym = Symbol

let library_functions =
  [
    (Symbol.symbol "read_integer", TAst.FunTyp {ret = TAst.Int; params = []});
    (Symbol.symbol "print_integer", TAst.FunTyp {ret = TAst.Void; params = [TAst.Param {paramname = TAst.ident_of_string "i"; typ = TAst.Int}]})
  ]