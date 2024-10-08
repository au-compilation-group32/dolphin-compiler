declare void @print_integer(i64)
declare i64 @read_integer()

define i64 @dolphin_main () {
 %t8 = alloca i64
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
 store i64 0, i64* %t8
 %tmp9 = load i64, i64* %x0
 %tmp10 = icmp eq i64 %tmp9, 1
 br i1 %tmp10, label %label11, label %label12
label11:
 %tmp14 = load i64, i64* %y1
 store i64 %tmp14, i64* %t8
 br label %label13
label12:
 %tmp15 = load i64, i64* %z4
 store i64 %tmp15, i64* %t8
 br label %label13
label13:
 %tmp16 = load i64, i64* %t8
 ret i64 %tmp16
}
