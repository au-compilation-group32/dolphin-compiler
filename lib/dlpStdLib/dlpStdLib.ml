module TAst = Lib.TypedAst
module Sym = Lib.Symbol

let library_functions =
  [
    (Sym.symbol "read_integer", TAst.FunTyp {ret = TAst.Int; params = []});
    (Sym.symbol "print_integer", TAst.FunTyp {ret = TAst.Void; params = [TAst.Param {paramname = TAst.ident_of_string "i"; typ = TAst.Int}]})
  ]