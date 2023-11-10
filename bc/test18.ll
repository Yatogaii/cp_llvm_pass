; ModuleID = 'test18.bc'
source_filename = "test18.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @plus(i32 %a, i32 %b) #0 !dbg !7 {
entry:
  %a.addr = alloca i32, align 4
  %b.addr = alloca i32, align 4
  store i32 %a, i32* %a.addr, align 4
  call void @llvm.dbg.declare(metadata i32* %a.addr, metadata !11, metadata !DIExpression()), !dbg !12
  store i32 %b, i32* %b.addr, align 4
  call void @llvm.dbg.declare(metadata i32* %b.addr, metadata !13, metadata !DIExpression()), !dbg !14
  %0 = load i32, i32* %a.addr, align 4, !dbg !15
  %1 = load i32, i32* %b.addr, align 4, !dbg !16
  %add = add nsw i32 %0, %1, !dbg !17
  ret i32 %add, !dbg !18
}

; Function Attrs: nounwind readnone speculatable willreturn
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @minus(i32 %a, i32 %b) #0 !dbg !19 {
entry:
  %a.addr = alloca i32, align 4
  %b.addr = alloca i32, align 4
  store i32 %a, i32* %a.addr, align 4
  call void @llvm.dbg.declare(metadata i32* %a.addr, metadata !20, metadata !DIExpression()), !dbg !21
  store i32 %b, i32* %b.addr, align 4
  call void @llvm.dbg.declare(metadata i32* %b.addr, metadata !22, metadata !DIExpression()), !dbg !23
  %0 = load i32, i32* %a.addr, align 4, !dbg !24
  %1 = load i32, i32* %b.addr, align 4, !dbg !25
  %sub = sub nsw i32 %0, %1, !dbg !26
  ret i32 %sub, !dbg !27
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 (i32, i32)* @foo(i32 %a, i32 %b, i32 (i32, i32)* %a_fptr, i32 (i32, i32)* %b_fptr) #0 !dbg !28 {
entry:
  %a.addr = alloca i32, align 4
  %b.addr = alloca i32, align 4
  %a_fptr.addr = alloca i32 (i32, i32)*, align 8
  %b_fptr.addr = alloca i32 (i32, i32)*, align 8
  store i32 %a, i32* %a.addr, align 4
  call void @llvm.dbg.declare(metadata i32* %a.addr, metadata !32, metadata !DIExpression()), !dbg !33
  store i32 %b, i32* %b.addr, align 4
  call void @llvm.dbg.declare(metadata i32* %b.addr, metadata !34, metadata !DIExpression()), !dbg !35
  store i32 (i32, i32)* %a_fptr, i32 (i32, i32)** %a_fptr.addr, align 8
  call void @llvm.dbg.declare(metadata i32 (i32, i32)** %a_fptr.addr, metadata !36, metadata !DIExpression()), !dbg !37
  store i32 (i32, i32)* %b_fptr, i32 (i32, i32)** %b_fptr.addr, align 8
  call void @llvm.dbg.declare(metadata i32 (i32, i32)** %b_fptr.addr, metadata !38, metadata !DIExpression()), !dbg !39
  %0 = load i32 (i32, i32)*, i32 (i32, i32)** %a_fptr.addr, align 8, !dbg !40
  ret i32 (i32, i32)* %0, !dbg !41
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 (i32, i32)* @clever(i32 %a, i32 %b, i32 (i32, i32)* %a_fptr, i32 (i32, i32)* %b_fptr) #0 !dbg !42 {
entry:
  %a.addr = alloca i32, align 4
  %b.addr = alloca i32, align 4
  %a_fptr.addr = alloca i32 (i32, i32)*, align 8
  %b_fptr.addr = alloca i32 (i32, i32)*, align 8
  store i32 %a, i32* %a.addr, align 4
  call void @llvm.dbg.declare(metadata i32* %a.addr, metadata !43, metadata !DIExpression()), !dbg !44
  store i32 %b, i32* %b.addr, align 4
  call void @llvm.dbg.declare(metadata i32* %b.addr, metadata !45, metadata !DIExpression()), !dbg !46
  store i32 (i32, i32)* %a_fptr, i32 (i32, i32)** %a_fptr.addr, align 8
  call void @llvm.dbg.declare(metadata i32 (i32, i32)** %a_fptr.addr, metadata !47, metadata !DIExpression()), !dbg !48
  store i32 (i32, i32)* %b_fptr, i32 (i32, i32)** %b_fptr.addr, align 8
  call void @llvm.dbg.declare(metadata i32 (i32, i32)** %b_fptr.addr, metadata !49, metadata !DIExpression()), !dbg !50
  %0 = load i32, i32* %a.addr, align 4, !dbg !51
  %1 = load i32, i32* %b.addr, align 4, !dbg !52
  %2 = load i32 (i32, i32)*, i32 (i32, i32)** %a_fptr.addr, align 8, !dbg !53
  %3 = load i32 (i32, i32)*, i32 (i32, i32)** %b_fptr.addr, align 8, !dbg !54
  %call = call i32 (i32, i32)* @foo(i32 %0, i32 %1, i32 (i32, i32)* %2, i32 (i32, i32)* %3), !dbg !55
  ret i32 (i32, i32)* %call, !dbg !56
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 (i32, i32)* @clever1(i32 (i32, i32)* (i32, i32, i32 (i32, i32)*, i32 (i32, i32)*)* %goo_ptr, i32 %a, i32 %b, i32 (i32, i32)* %a_fptr, i32 (i32, i32)* %b_fptr) #0 !dbg !57 {
entry:
  %goo_ptr.addr = alloca i32 (i32, i32)* (i32, i32, i32 (i32, i32)*, i32 (i32, i32)*)*, align 8
  %a.addr = alloca i32, align 4
  %b.addr = alloca i32, align 4
  %a_fptr.addr = alloca i32 (i32, i32)*, align 8
  %b_fptr.addr = alloca i32 (i32, i32)*, align 8
  store i32 (i32, i32)* (i32, i32, i32 (i32, i32)*, i32 (i32, i32)*)* %goo_ptr, i32 (i32, i32)* (i32, i32, i32 (i32, i32)*, i32 (i32, i32)*)** %goo_ptr.addr, align 8
  call void @llvm.dbg.declare(metadata i32 (i32, i32)* (i32, i32, i32 (i32, i32)*, i32 (i32, i32)*)** %goo_ptr.addr, metadata !61, metadata !DIExpression()), !dbg !62
  store i32 %a, i32* %a.addr, align 4
  call void @llvm.dbg.declare(metadata i32* %a.addr, metadata !63, metadata !DIExpression()), !dbg !64
  store i32 %b, i32* %b.addr, align 4
  call void @llvm.dbg.declare(metadata i32* %b.addr, metadata !65, metadata !DIExpression()), !dbg !66
  store i32 (i32, i32)* %a_fptr, i32 (i32, i32)** %a_fptr.addr, align 8
  call void @llvm.dbg.declare(metadata i32 (i32, i32)** %a_fptr.addr, metadata !67, metadata !DIExpression()), !dbg !68
  store i32 (i32, i32)* %b_fptr, i32 (i32, i32)** %b_fptr.addr, align 8
  call void @llvm.dbg.declare(metadata i32 (i32, i32)** %b_fptr.addr, metadata !69, metadata !DIExpression()), !dbg !70
  %0 = load i32 (i32, i32)* (i32, i32, i32 (i32, i32)*, i32 (i32, i32)*)*, i32 (i32, i32)* (i32, i32, i32 (i32, i32)*, i32 (i32, i32)*)** %goo_ptr.addr, align 8, !dbg !71
  %1 = load i32, i32* %a.addr, align 4, !dbg !72
  %2 = load i32, i32* %b.addr, align 4, !dbg !73
  %3 = load i32 (i32, i32)*, i32 (i32, i32)** %b_fptr.addr, align 8, !dbg !74
  %4 = load i32 (i32, i32)*, i32 (i32, i32)** %a_fptr.addr, align 8, !dbg !75
  %call = call i32 (i32, i32)* %0(i32 %1, i32 %2, i32 (i32, i32)* %3, i32 (i32, i32)* %4), !dbg !71
  ret i32 (i32, i32)* %call, !dbg !76
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @moo(i8 signext %x, i32 %op1, i32 %op2) #0 !dbg !77 {
entry:
  %x.addr = alloca i8, align 1
  %op1.addr = alloca i32, align 4
  %op2.addr = alloca i32, align 4
  %a_fptr = alloca i32 (i32, i32)*, align 8
  %s_fptr = alloca i32 (i32, i32)*, align 8
  %goo_ptr = alloca i32 (i32, i32)* (i32, i32, i32 (i32, i32)*, i32 (i32, i32)*)*, align 8
  %t_fptr = alloca i32 (i32, i32)*, align 8
  store i8 %x, i8* %x.addr, align 1
  call void @llvm.dbg.declare(metadata i8* %x.addr, metadata !81, metadata !DIExpression()), !dbg !82
  store i32 %op1, i32* %op1.addr, align 4
  call void @llvm.dbg.declare(metadata i32* %op1.addr, metadata !83, metadata !DIExpression()), !dbg !84
  store i32 %op2, i32* %op2.addr, align 4
  call void @llvm.dbg.declare(metadata i32* %op2.addr, metadata !85, metadata !DIExpression()), !dbg !86
  call void @llvm.dbg.declare(metadata i32 (i32, i32)** %a_fptr, metadata !87, metadata !DIExpression()), !dbg !88
  store i32 (i32, i32)* @plus, i32 (i32, i32)** %a_fptr, align 8, !dbg !88
  call void @llvm.dbg.declare(metadata i32 (i32, i32)** %s_fptr, metadata !89, metadata !DIExpression()), !dbg !90
  store i32 (i32, i32)* @minus, i32 (i32, i32)** %s_fptr, align 8, !dbg !90
  call void @llvm.dbg.declare(metadata i32 (i32, i32)* (i32, i32, i32 (i32, i32)*, i32 (i32, i32)*)** %goo_ptr, metadata !91, metadata !DIExpression()), !dbg !92
  store i32 (i32, i32)* (i32, i32, i32 (i32, i32)*, i32 (i32, i32)*)* @clever, i32 (i32, i32)* (i32, i32, i32 (i32, i32)*, i32 (i32, i32)*)** %goo_ptr, align 8, !dbg !92
  %0 = load i8, i8* %x.addr, align 1, !dbg !93
  %conv = sext i8 %0 to i32, !dbg !93
  %cmp = icmp eq i32 %conv, 43, !dbg !95
  br i1 %cmp, label %if.then, label %if.end, !dbg !96

if.then:                                          ; preds = %entry
  store i32 (i32, i32)* (i32, i32, i32 (i32, i32)*, i32 (i32, i32)*)* @foo, i32 (i32, i32)* (i32, i32, i32 (i32, i32)*, i32 (i32, i32)*)** %goo_ptr, align 8, !dbg !97
  br label %if.end, !dbg !99

if.end:                                           ; preds = %if.then, %entry
  call void @llvm.dbg.declare(metadata i32 (i32, i32)** %t_fptr, metadata !100, metadata !DIExpression()), !dbg !101
  %1 = load i32 (i32, i32)* (i32, i32, i32 (i32, i32)*, i32 (i32, i32)*)*, i32 (i32, i32)* (i32, i32, i32 (i32, i32)*, i32 (i32, i32)*)** %goo_ptr, align 8, !dbg !102
  %2 = load i32, i32* %op1.addr, align 4, !dbg !103
  %3 = load i32, i32* %op2.addr, align 4, !dbg !104
  %4 = load i32 (i32, i32)*, i32 (i32, i32)** %s_fptr, align 8, !dbg !105
  %5 = load i32 (i32, i32)*, i32 (i32, i32)** %a_fptr, align 8, !dbg !106
  %call = call i32 (i32, i32)* @clever1(i32 (i32, i32)* (i32, i32, i32 (i32, i32)*, i32 (i32, i32)*)* %1, i32 %2, i32 %3, i32 (i32, i32)* %4, i32 (i32, i32)* %5), !dbg !107
  store i32 (i32, i32)* %call, i32 (i32, i32)** %t_fptr, align 8, !dbg !101
  %6 = load i32 (i32, i32)*, i32 (i32, i32)** %t_fptr, align 8, !dbg !108
  %7 = load i32, i32* %op1.addr, align 4, !dbg !109
  %8 = load i32, i32* %op2.addr, align 4, !dbg !110
  %call2 = call i32 %6(i32 %7, i32 %8), !dbg !108
  ret i32 0, !dbg !111
}

attributes #0 = { noinline nounwind optnone uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind readnone speculatable willreturn }

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!3, !4, !5}
!llvm.ident = !{!6}

!0 = distinct !DICompileUnit(language: DW_LANG_C99, file: !1, producer: "clang version 10.0.0 ", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, enums: !2, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "test18.c", directory: "/root/assign2/test")
!2 = !{}
!3 = !{i32 7, !"Dwarf Version", i32 4}
!4 = !{i32 2, !"Debug Info Version", i32 3}
!5 = !{i32 1, !"wchar_size", i32 4}
!6 = !{!"clang version 10.0.0 "}
!7 = distinct !DISubprogram(name: "plus", scope: !1, file: !1, line: 2, type: !8, scopeLine: 2, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !2)
!8 = !DISubroutineType(types: !9)
!9 = !{!10, !10, !10}
!10 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!11 = !DILocalVariable(name: "a", arg: 1, scope: !7, file: !1, line: 2, type: !10)
!12 = !DILocation(line: 2, column: 14, scope: !7)
!13 = !DILocalVariable(name: "b", arg: 2, scope: !7, file: !1, line: 2, type: !10)
!14 = !DILocation(line: 2, column: 21, scope: !7)
!15 = !DILocation(line: 3, column: 11, scope: !7)
!16 = !DILocation(line: 3, column: 13, scope: !7)
!17 = !DILocation(line: 3, column: 12, scope: !7)
!18 = !DILocation(line: 3, column: 4, scope: !7)
!19 = distinct !DISubprogram(name: "minus", scope: !1, file: !1, line: 6, type: !8, scopeLine: 6, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !2)
!20 = !DILocalVariable(name: "a", arg: 1, scope: !19, file: !1, line: 6, type: !10)
!21 = !DILocation(line: 6, column: 15, scope: !19)
!22 = !DILocalVariable(name: "b", arg: 2, scope: !19, file: !1, line: 6, type: !10)
!23 = !DILocation(line: 6, column: 22, scope: !19)
!24 = !DILocation(line: 7, column: 11, scope: !19)
!25 = !DILocation(line: 7, column: 13, scope: !19)
!26 = !DILocation(line: 7, column: 12, scope: !19)
!27 = !DILocation(line: 7, column: 4, scope: !19)
!28 = distinct !DISubprogram(name: "foo", scope: !1, file: !1, line: 10, type: !29, scopeLine: 10, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !2)
!29 = !DISubroutineType(types: !30)
!30 = !{!31, !10, !10, !31, !31}
!31 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !8, size: 64)
!32 = !DILocalVariable(name: "a", arg: 1, scope: !28, file: !1, line: 10, type: !10)
!33 = !DILocation(line: 10, column: 15, scope: !28)
!34 = !DILocalVariable(name: "b", arg: 2, scope: !28, file: !1, line: 10, type: !10)
!35 = !DILocation(line: 10, column: 22, scope: !28)
!36 = !DILocalVariable(name: "a_fptr", arg: 3, scope: !28, file: !1, line: 10, type: !31)
!37 = !DILocation(line: 10, column: 31, scope: !28)
!38 = !DILocalVariable(name: "b_fptr", arg: 4, scope: !28, file: !1, line: 10, type: !31)
!39 = !DILocation(line: 10, column: 55, scope: !28)
!40 = !DILocation(line: 11, column: 11, scope: !28)
!41 = !DILocation(line: 11, column: 4, scope: !28)
!42 = distinct !DISubprogram(name: "clever", scope: !1, file: !1, line: 13, type: !29, scopeLine: 13, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !2)
!43 = !DILocalVariable(name: "a", arg: 1, scope: !42, file: !1, line: 13, type: !10)
!44 = !DILocation(line: 13, column: 18, scope: !42)
!45 = !DILocalVariable(name: "b", arg: 2, scope: !42, file: !1, line: 13, type: !10)
!46 = !DILocation(line: 13, column: 25, scope: !42)
!47 = !DILocalVariable(name: "a_fptr", arg: 3, scope: !42, file: !1, line: 13, type: !31)
!48 = !DILocation(line: 13, column: 34, scope: !42)
!49 = !DILocalVariable(name: "b_fptr", arg: 4, scope: !42, file: !1, line: 13, type: !31)
!50 = !DILocation(line: 13, column: 58, scope: !42)
!51 = !DILocation(line: 14, column: 15, scope: !42)
!52 = !DILocation(line: 14, column: 17, scope: !42)
!53 = !DILocation(line: 14, column: 19, scope: !42)
!54 = !DILocation(line: 14, column: 26, scope: !42)
!55 = !DILocation(line: 14, column: 11, scope: !42)
!56 = !DILocation(line: 14, column: 4, scope: !42)
!57 = distinct !DISubprogram(name: "clever1", scope: !1, file: !1, line: 17, type: !58, scopeLine: 17, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !2)
!58 = !DISubroutineType(types: !59)
!59 = !{!31, !60, !10, !10, !31, !31}
!60 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !29, size: 64)
!61 = !DILocalVariable(name: "goo_ptr", arg: 1, scope: !57, file: !1, line: 17, type: !60)
!62 = !DILocation(line: 17, column: 24, scope: !57)
!63 = !DILocalVariable(name: "a", arg: 2, scope: !57, file: !1, line: 17, type: !10)
!64 = !DILocation(line: 17, column: 96, scope: !57)
!65 = !DILocalVariable(name: "b", arg: 3, scope: !57, file: !1, line: 17, type: !10)
!66 = !DILocation(line: 17, column: 103, scope: !57)
!67 = !DILocalVariable(name: "a_fptr", arg: 4, scope: !57, file: !1, line: 17, type: !31)
!68 = !DILocation(line: 17, column: 112, scope: !57)
!69 = !DILocalVariable(name: "b_fptr", arg: 5, scope: !57, file: !1, line: 17, type: !31)
!70 = !DILocation(line: 17, column: 136, scope: !57)
!71 = !DILocation(line: 18, column: 11, scope: !57)
!72 = !DILocation(line: 18, column: 19, scope: !57)
!73 = !DILocation(line: 18, column: 21, scope: !57)
!74 = !DILocation(line: 18, column: 23, scope: !57)
!75 = !DILocation(line: 18, column: 30, scope: !57)
!76 = !DILocation(line: 18, column: 4, scope: !57)
!77 = distinct !DISubprogram(name: "moo", scope: !1, file: !1, line: 20, type: !78, scopeLine: 20, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !2)
!78 = !DISubroutineType(types: !79)
!79 = !{!10, !80, !10, !10}
!80 = !DIBasicType(name: "char", size: 8, encoding: DW_ATE_signed_char)
!81 = !DILocalVariable(name: "x", arg: 1, scope: !77, file: !1, line: 20, type: !80)
!82 = !DILocation(line: 20, column: 14, scope: !77)
!83 = !DILocalVariable(name: "op1", arg: 2, scope: !77, file: !1, line: 20, type: !10)
!84 = !DILocation(line: 20, column: 21, scope: !77)
!85 = !DILocalVariable(name: "op2", arg: 3, scope: !77, file: !1, line: 20, type: !10)
!86 = !DILocation(line: 20, column: 30, scope: !77)
!87 = !DILocalVariable(name: "a_fptr", scope: !77, file: !1, line: 21, type: !31)
!88 = !DILocation(line: 21, column: 11, scope: !77)
!89 = !DILocalVariable(name: "s_fptr", scope: !77, file: !1, line: 22, type: !31)
!90 = !DILocation(line: 22, column: 11, scope: !77)
!91 = !DILocalVariable(name: "goo_ptr", scope: !77, file: !1, line: 23, type: !60)
!92 = !DILocation(line: 23, column: 14, scope: !77)
!93 = !DILocation(line: 25, column: 9, scope: !94)
!94 = distinct !DILexicalBlock(scope: !77, file: !1, line: 25, column: 9)
!95 = !DILocation(line: 25, column: 11, scope: !94)
!96 = !DILocation(line: 25, column: 9, scope: !77)
!97 = !DILocation(line: 27, column: 14, scope: !98)
!98 = distinct !DILexicalBlock(scope: !94, file: !1, line: 26, column: 5)
!99 = !DILocation(line: 28, column: 5, scope: !98)
!100 = !DILocalVariable(name: "t_fptr", scope: !77, file: !1, line: 30, type: !31)
!101 = !DILocation(line: 30, column: 11, scope: !77)
!102 = !DILocation(line: 30, column: 39, scope: !77)
!103 = !DILocation(line: 30, column: 48, scope: !77)
!104 = !DILocation(line: 30, column: 53, scope: !77)
!105 = !DILocation(line: 30, column: 58, scope: !77)
!106 = !DILocation(line: 30, column: 66, scope: !77)
!107 = !DILocation(line: 30, column: 31, scope: !77)
!108 = !DILocation(line: 31, column: 5, scope: !77)
!109 = !DILocation(line: 31, column: 12, scope: !77)
!110 = !DILocation(line: 31, column: 17, scope: !77)
!111 = !DILocation(line: 33, column: 5, scope: !77)
