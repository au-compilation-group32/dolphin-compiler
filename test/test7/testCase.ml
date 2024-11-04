open Lib.TestUtils
open Lib.Ast

let test7: test_case = 
  "test7",
  "negative scope test",
  let delc11 = Declaration {name = int_x; tp = None; body = Integer {int = 1L}} in
  let delc_block1 = [delc11] in
  let declar_block1 : declaration_block = DeclBlock delc_block1 in
  let delc21 = Declaration {name = int_y; tp = None; body = Assignment {lvl = Var int_x; rhs = BinOp {left = Lval(Var int_x); op = Plus; right = Integer {int = 2L}}}} in
  let delc_block2 = [delc21] in
  let declar_block2 : declaration_block = DeclBlock delc_block2 in
  let delc31 = Declaration {name = int_z; tp = None; body = Integer {int = 1L}} in
  let delc_block3 = [delc31] in
  let declar_block3 : declaration_block = DeclBlock delc_block3 in
  let delc41 = Declaration {name = int_y; tp = None; body = Boolean {bool = false}} in
  let delc_block4 = [delc41] in
  let declar_block4 : declaration_block = DeclBlock delc_block4 in
  let delc51 = Declaration {name = int_y; tp = None; body = Integer {int = 1L }} in
  let delc_block5 = [delc51] in
  let declar_block5 : declaration_block = DeclBlock delc_block5 in
  let delc61 = Declaration {name = int_y; tp = None; body = Assignment {lvl = Var int_x; rhs = BinOp {left = Lval(Var int_x); op = Plus; right = Integer {int = 2L}}}} in
  let delc_block6 = [delc61] in
  let declar_block6 : declaration_block = DeclBlock delc_block6 in
  [
    VarDeclStm declar_block1;

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
          VarDeclStm declar_block2;
          ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_y)]})};

          IfThenElseStm {
            cond = Boolean {bool = true};
            thbr = CompoundStm {stms = 
              [
                IfThenElseStm {
                  cond = Boolean {bool = true};
                  thbr = CompoundStm {stms = 
                    [
                      VarDeclStm declar_block3;
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
          VarDeclStm declar_block6;
          ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_y)]})};

          IfThenElseStm {
            cond = Boolean {bool = true};
            thbr = CompoundStm {stms = 
              [
                IfThenElseStm {
                  cond = Boolean {bool = true};
                  thbr = CompoundStm {stms = 
                    [
                      VarDeclStm declar_block4;
                      VarDeclStm declar_block3;
                      ExprStm {expr = Some (Call {fname = print_integer; args = [BinOp {left = Lval(Var int_y); op = Plus; right =Lval(Var int_z)}]})};
                      VarDeclStm declar_block5;
                      ExprStm {expr = Some (Call {fname = print_integer; args = [BinOp {left = Lval(Var int_y); op = Plus; right =Lval(Var int_z)}]})};
                      VarDeclStm declar_block4;
                    ]
                  };
                  elbro = None;
                };
                ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_x)]})};
                ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_y)]})};
                ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_z)]})};
                ExprStm {expr = Some (Call {fname = print_integer; args = [BinOp {left = Lval(Var int_x); op = Plus; right =Lval(Var int_y)}]})};
                VarDeclStm declar_block4;
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

let _ = run_testcase test7
