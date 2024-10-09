open Lib.TestUtils
open Lib.Ast

let test1: test_case = 
  "test7",
  "negative scope test",
  [
    VarDeclStm {name = int_x; tp = None; body = Integer {int = 1L}};

    IfThenElseStm {
      cond = BinOp {left = Lval (Var int_x); op = Eq; right = Integer {int = 3L}};
      thbr = CompoundStm {stms = 
        [
          ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_y)]})}
        ]
      };
      elbro = None;
    };

    IfThenElseStm {
      cond = BinOp {left = Lval (Var int_x); op = Eq; right = Assignment {lvl = Var int_y; rhs = BinOp {left = Lval(Var int_x); op = Plus; right = Integer {int = 2L}}}};
      thbr = CompoundStm {stms = 
        [
          ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_y)]})}
        ]
      };
      elbro = None;
    };

    IfThenElseStm {
      cond = BinOp {left = Lval (Var int_x); op = Eq; right = Integer {int = 3L}};
      thbr = CompoundStm {stms = 
        [
          VarDeclStm {name = int_y; tp = None; body = Assignment {lvl = Var int_x; rhs = BinOp {left = Lval(Var int_x); op = Plus; right = Integer {int = 2L}}}};
          ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_y)]})};

          IfThenElseStm {
            cond = Boolean {bool = true};
            thbr = CompoundStm {stms = 
              [
                IfThenElseStm {
                  cond = Boolean {bool = true};
                  thbr = CompoundStm {stms = 
                    [
                      VarDeclStm {name = int_z; tp = None; body = Integer {int = 1L}};
                      ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_x)]})};
                      ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_y)]})};
                      ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_z)]})};
                    ]
                  };
                  elbro = None;
                };
                ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_x)]})};
                ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_y)]})};
                ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_z)]})};
              ]
            };
            elbro = None;
          };
        ]
      };
      elbro = None;
    };
 
    IfThenElseStm {
      cond = Boolean {bool = true};
      thbr = CompoundStm {stms = 
        [
          VarDeclStm {name = int_y; tp = None; body = Assignment {lvl = Var int_x; rhs = BinOp {left = Lval(Var int_x); op = Plus; right = Integer {int = 2L}}}};
          ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_y)]})};

          IfThenElseStm {
            cond = Boolean {bool = true};
            thbr = CompoundStm {stms = 
              [
                IfThenElseStm {
                  cond = Boolean {bool = true};
                  thbr = CompoundStm {stms = 
                    [
                      VarDeclStm {name = int_y; tp = None; body = Boolean {bool = false}};
                      VarDeclStm {name = int_z; tp = None; body = Integer {int = 1L}};
                      ExprStm {expr = Some (Call {fname = print_integer; args = [BinOp {left = Lval(Var int_y); op = Plus; right =Lval(Var int_z)}]})};
                      VarDeclStm {name = int_y; tp = None; body = Integer {int = 1L }};
                      ExprStm {expr = Some (Call {fname = print_integer; args = [BinOp {left = Lval(Var int_y); op = Plus; right =Lval(Var int_z)}]})};
                      VarDeclStm {name = int_y; tp = None; body = Boolean {bool = false}};
                    ]
                  };
                  elbro = None;
                };
                ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_x)]})};
                ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_y)]})};
                ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_z)]})};
                ExprStm {expr = Some (Call {fname = print_integer; args = [BinOp {left = Lval(Var int_x); op = Plus; right =Lval(Var int_y)}]})};
                VarDeclStm {name = int_y; tp = None; body = Boolean {bool = false}};
                ExprStm {expr = Some (Call {fname = print_integer; args = [BinOp {left = Lval(Var int_x); op = Plus; right =Lval(Var int_y)}]})};
              ]
            };
            elbro = None;
          };
        ]
      };
      elbro = None;
    };
    ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_x)]})};
    ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_y)]})};
    ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_z)]})};
    ReturnStm {ret = Lval (Var int_x)}
  ]

let _ = run_testcase test1
