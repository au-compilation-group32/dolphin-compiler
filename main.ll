declare void @print_integer(i64)
declare i64 @read_integer()

define i64 @dolphin_main () {
 %z4 = alloca i64
 %y1 = alloca i64
 %x0 = alloca i64
 store i64 1, i64* %x0
 %tmp2 = load i64, i64* %x0
 %tmp3 = add i64 %tmp2, 2
 store i64 %tmp3, i64* %x0
 store i64 %tmp3, i64* %y1
 %tmp5 = load i64, i64* %x0
 %tmp6 = load i64, i64* %y1
 %tmp7 = add i64 %tmp5, %tmp6
 store i64 %tmp7, i64* %z4
 %tmp8 = load i64, i64* %z4
 ret i64 %tmp8
}
