module TAst = TypedAst
module Sym = Symbol

let make_ident name = TAst.Ident {sym = Sym.symbol name}

let library_functions =
  [
    (Symbol.symbol "read_integer", TAst.FunTyp {ret = TAst.Int; params = []});
    (Symbol.symbol "print_integer", TAst.FunTyp {ret = TAst.Void; params = [Param {paramname = make_ident "value"; typ = TAst.Int}]})
  ]