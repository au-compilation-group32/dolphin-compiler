module Ast = Lib.Ast
module TAst = Lib.TypedAst
module Sym = Lib.Symbol
module Ll = Lib.Ll

let library_functions = SrcProcessor.header_file_to_ast "lib/dlpStdLib/stdlib.dlp"

let reserved_record_names =
  [
    (Sym.symbol "array_type", Ll.Struct [Ll.I64; Ll.Array (0, Ll.I8)]);
    (Sym.symbol "stream", Ll.Struct []);
    (* TAst.RecDecl {rec_name = TAst.RecordName {sym = Sym.symbol "array_type"}; fields = []};
    TAst.RecDecl {rec_name = TAst.RecordName {sym = Sym.symbol "stream"}; fields = []}; *)
    (* TAst.RecDecl {rec_name = TAst.RecordName {sym = Sym.symbol "socket"}; fields = []};
    TAst.RecDecl {rec_name = TAst.RecordName {sym = Sym.symbol "socket_address"}; fields = []};
    TAst.RecDecl {rec_name = TAst.RecordName {sym = Sym.symbol "ip_address"}; fields = []};
    TAst.RecDecl {rec_name = TAst.RecordName {sym = Sym.symbol "ip_version"}; fields = []};
    TAst.RecDecl {rec_name = TAst.RecordName {sym = Sym.symbol "connection_type"}; fields = []};
    TAst.RecDecl {rec_name = TAst.RecordName {sym = Sym.symbol "accepted_connection"}; fields = []};
    TAst.RecDecl {rec_name = TAst.RecordName {sym = Sym.symbol "udp_recvfrom_result"}; fields = []}; *)
  ]