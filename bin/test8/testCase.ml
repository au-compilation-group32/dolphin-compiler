open Lib.TestUtils
open Lib.Ast

let test8: test_case = 
  "test8",
  "positive scope test",
  let delc11 = Declaration {name = int_x; tp = None; body = Call {fname = read_integer; args = []}} in
  let delc_block1 = [delc11] in
  let declar_blockx : declaration_block = DeclBlock delc_block1 in
  let delc21 = Declaration {name = int_y; tp = None; body = Call {fname = read_integer; args = []}} in
  let delc_block2 = [delc21] in
  let declar_blocky : declaration_block = DeclBlock delc_block2 in
  let delc31 = Declaration {name = int_z; tp = None; body = Call {fname = read_integer; args = []}} in
  let delc_block3 = [delc31] in
  let declar_blockz : declaration_block = DeclBlock delc_block3 in
  [
    VarDeclStm declar_blockx;
    ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_x)]})};
    VarDeclStm declar_blocky;
    ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_y)]})};
    VarDeclStm declar_blockz;
    ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_z)]})};

    IfThenElseStm {
      cond = Boolean {bool = true};
      thbr = CompoundStm {stms = 
        [
          VarDeclStm declar_blocky;
          ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_y)]})}
        ]
      };
      elbro = None;
    };
    ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_y)]})};

    IfThenElseStm {
      cond = BinOp {left = Lval (Var int_x); op = Le; right = Assignment {lvl = Var int_y; rhs = BinOp {left = Lval(Var int_x); op = Plus; right = Integer {int = 2L}}}};
      thbr = CompoundStm {stms = 
        [
          ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_y)]})};
          VarDeclStm declar_blocky;
          ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_y)]})}
        ]
      };
      elbro = None;
    };
    ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_y)]})};

    IfThenElseStm {
      cond = Boolean {bool = true};
      thbr = CompoundStm {stms = 
        [
          VarDeclStm declar_blocky;
          ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_y)]})};

          IfThenElseStm {
            cond = Boolean {bool = true};
            thbr = CompoundStm {stms = 
              [
                IfThenElseStm {
                  cond = Boolean {bool = true};
                  thbr = CompoundStm {stms = 
                    [
                      VarDeclStm declar_blockz;
                      ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_z)]})};
                      ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_x)]})};
                      ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_y)]})};
                      ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_z)]})};

                      ExprStm {expr = Some (Assignment {lvl = Var int_x; rhs = Integer {int = 200L}})};
                      ExprStm {expr = Some (Assignment {lvl = Var int_y; rhs = Integer {int = 693L}})};
                      ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_x)]})};
                      ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_y)]})};
                      ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_z)]})};


                    ]
                  };
                  elbro = None;
                };
                VarDeclStm declar_blockz;
                ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_z)]})};
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
 
    ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_x)]})};
    ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_y)]})};
    ExprStm {expr = Some (Call {fname = print_integer; args = [Lval(Var int_z)]})};
    ReturnStm {ret = Lval (Var int_x)}
  ]

let _ = run_testcase test8
