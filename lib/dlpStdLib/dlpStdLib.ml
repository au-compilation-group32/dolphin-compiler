module Ast = Lib.Ast
module TAst = Lib.TypedAst
module Sym = Lib.Symbol

let library_functions = SrcProcessor.header_file_to_ast "lib/dlpStdLib/stdlib.dlp"

(* let conv fs =
  let Ast.FuncSig {name = Ast.Ident {name}; ret_tp; params; _} =fs in
  Sym.symbol name, TAst.FunTyp {ret = ret_tp; params = params}

let library_functions =
  [
    (Sym.symbol "read_integer", TAst.FunTyp {ret = TAst.Int; params = []});
    (Sym.symbol "print_integer", TAst.FunTyp {ret = TAst.Void; params = [TAst.Param {paramname = TAst.ident_of_string "i"; typ = TAst.Int}]})
  ] *)