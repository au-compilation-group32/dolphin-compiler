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
 %tmp9 = call i64 @read_integer ()
 store i64 %tmp9, i64* %t8
 %tmp10 = load i64, i64* %x0
 call void @print_integer (i64 %tmp10)
 %tmp11 = load i64, i64* %y1
 call void @print_integer (i64 %tmp11)
 %tmp12 = load i64, i64* %z4
 call void @print_integer (i64 %tmp12)
 %tmp13 = load i64, i64* %t8
 call void @print_integer (i64 %tmp13)
 %tmp14 = load i64, i64* %x0
 %tmp15 = icmp eq i64 %tmp14, 1
 br i1 %tmp15, label %label16, label %label17
label16:
 %tmp19 = load i64, i64* %y1
 store i64 %tmp19, i64* %t8
 br label %label18
label17:
 br label %label18
label18:
 %tmp20 = load i64, i64* %x0
 %tmp21 = icmp eq i64 %tmp20, 1
 br i1 %tmp21, label %label22, label %label23
label22:
 %tmp25 = load i64, i64* %y1
 store i64 %tmp25, i64* %t8
 br label %label24
label23:
 %tmp26 = load i64, i64* %z4
 store i64 %tmp26, i64* %t8
 br label %label24
label24:
 %tmp27 = load i64, i64* %x0
 call void @print_integer (i64 %tmp27)
 %tmp28 = load i64, i64* %y1
 call void @print_integer (i64 %tmp28)
 %tmp29 = load i64, i64* %z4
 call void @print_integer (i64 %tmp29)
 %tmp30 = load i64, i64* %t8
 call void @print_integer (i64 %tmp30)
 %tmp31 = load i64, i64* %t8
 ret i64 %tmp31
}