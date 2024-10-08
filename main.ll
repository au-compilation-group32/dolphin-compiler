declare void @print_integer(i64)
declare i64 @read_integer()

define i64 @dolphin_main () {
 %z3 = alloca i64
 %y1 = alloca i64
 %x0 = alloca i64
 store i64 1, i64* %x0
 %tmp2 = load i64, i64* %x0
 store i64 %tmp2, i64* %y1
 %tmp4 = load i64, i64* %x0
 %tmp5 = load i64, i64* %y1
 %tmp6 = add i64 %tmp4, %tmp5
 store i64 %tmp6, i64* %z3
 %tmp7 = load i64, i64* %z3
 ret i64 %tmp7
}
