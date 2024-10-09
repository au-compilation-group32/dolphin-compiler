declare void @print_integer(i64)
declare i64 @read_integer()

define i64 @dolphin_main () {
 %int_u16 = alloca i64
 %int_t8 = alloca i64
 %int_z4 = alloca i64
 %int_y1 = alloca i64
 %int_x0 = alloca i64
 store i64 1, i64* %int_x0
 %tmp2 = load i64, i64* %int_x0
 %tmp3 = add i64 %tmp2, 2
 store i64 %tmp3, i64* %int_x0
 store i64 %tmp3, i64* %int_y1
 %tmp5 = load i64, i64* %int_x0
 %tmp6 = load i64, i64* %int_y1
 %tmp7 = add i64 %tmp5, %tmp6
 store i64 %tmp7, i64* %int_z4
 %tmp9 = call i64 @read_integer ()
 store i64 %tmp9, i64* %int_t8
 %tmp10 = load i64, i64* %int_x0
 %tmp11 = icmp eq i64 %tmp10, 3
 br i1 %tmp11, label %label12, label %label13
label12:
 %tmp15 = load i64, i64* %int_t8
 call void @print_integer (i64 %tmp15)
 br label %label14
label13:
 br label %label14
label14:
 store i64 0, i64* %int_u16
 %tmp17 = load i64, i64* %int_t8
 %tmp18 = load i64, i64* %int_x0
 %tmp19 = icmp sle i64 %tmp17, %tmp18
 br i1 %tmp19, label %label20, label %label21
label20:
 %tmp23 = load i64, i64* %int_z4
 store i64 %tmp23, i64* %int_u16
 br label %label22
label21:
 %tmp24 = load i64, i64* %int_t8
 store i64 %tmp24, i64* %int_u16
 br label %label22
label22:
 %tmp25 = load i64, i64* %int_x0
 call void @print_integer (i64 %tmp25)
 %tmp26 = load i64, i64* %int_y1
 call void @print_integer (i64 %tmp26)
 %tmp27 = load i64, i64* %int_z4
 call void @print_integer (i64 %tmp27)
 %tmp28 = load i64, i64* %int_t8
 call void @print_integer (i64 %tmp28)
 %tmp29 = load i64, i64* %int_u16
 call void @print_integer (i64 %tmp29)
 %tmp30 = load i64, i64* %int_u16
 ret i64 %tmp30
}
