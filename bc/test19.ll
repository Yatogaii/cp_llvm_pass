; ModuleID = 'test19.bc'
source_filename = "test19.c"
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
  %f_fptr = alloca i32 (i32, i32)*, align 8
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
  call void @llvm.dbg.declare(metadata i32 (i32, i32)** %f_fptr, metadata !71, metadata !DIExpression()), !dbg !72
  %0 = load i32 (i32, i32)* (i32, i32, i32 (i32, i32)*, i32 (i32, i32)*)*, i32 (i32, i32)* (i32, i32, i32 (i32, i32)*, i32 (i32, i32)*)** %goo_ptr.addr, align 8, !dbg !73
  %1 = load i32, i32* %a.addr, align 4, !dbg !74
  %2 = load i32, i32* %b.addr, align 4, !dbg !75
  %3 = load i32 (i32, i32)*, i32 (i32, i32)** %b_fptr.addr, align 8, !dbg !76
  %4 = load i32 (i32, i32)*, i32 (i32, i32)** %a_fptr.addr, align 8, !dbg !77
  %call = call i32 (i32, i32)* %0(i32 %1, i32 %2, i32 (i32, i32)* %3, i32 (i32, i32)* %4), !dbg !73
  store i32 (i32, i32)* %call, i32 (i32, i32)** %f_fptr, align 8, !dbg !72
  %5 = load i32, i32* %a.addr, align 4, !dbg !78
  %cmp = icmp eq i32 %5, 43, !dbg !80
  br i1 %cmp, label %if.then, label %if.end, !dbg !81

if.then:                                          ; preds = %entry
  store i32 (i32, i32)* @plus, i32 (i32, i32)** %f_fptr, align 8, !dbg !82
  store i32 (i32, i32)* @minus, i32 (i32, i32)** %a_fptr.addr, align 8, !dbg !84
  br label %if.end, !dbg !85

if.end:                                           ; preds = %if.then, %entry
  %6 = load i32 (i32, i32)* (i32, i32, i32 (i32, i32)*, i32 (i32, i32)*)*, i32 (i32, i32)* (i32, i32, i32 (i32, i32)*, i32 (i32, i32)*)** %goo_ptr.addr, align 8, !dbg !86
  %7 = load i32, i32* %a.addr, align 4, !dbg !87
  %8 = load i32, i32* %b.addr, align 4, !dbg !88
  %9 = load i32 (i32, i32)*, i32 (i32, i32)** %f_fptr, align 8, !dbg !89
  %10 = load i32 (i32, i32)*, i32 (i32, i32)** %a_fptr.addr, align 8, !dbg !90
  %call1 = call i32 (i32, i32)* %6(i32 %7, i32 %8, i32 (i32, i32)* %9, i32 (i32, i32)* %10), !dbg !86
  ret i32 (i32, i32)* %call1, !dbg !91
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @moo(i8 signext %x, i32 %op1, i32 %op2) #0 !dbg !92 {
entry:
  %x.addr = alloca i8, align 1
  %op1.addr = alloca i32, align 4
  %op2.addr = alloca i32, align 4
  %a_fptr = alloca i32 (i32, i32)*, align 8
  %s_fptr = alloca i32 (i32, i32)*, align 8
  %goo_ptr = alloca i32 (i32, i32)* (i32, i32, i32 (i32, i32)*, i32 (i32, i32)*)*, align 8
  %t_fptr = alloca i32 (i32, i32)*, align 8
  store i8 %x, i8* %x.addr, align 1
  call void @llvm.dbg.declare(metadata i8* %x.addr, metadata !96, metadata !DIExpression()), !dbg !97
  store i32 %op1, i32* %op1.addr, align 4
  call void @llvm.dbg.declare(metadata i32* %op1.addr, metadata !98, metadata !DIExpression()), !dbg !99
  store i32 %op2, i32* %op2.addr, align 4
  call void @llvm.dbg.declare(metadata i32* %op2.addr, metadata !100, metadata !DIExpression()), !dbg !101
  call void @llvm.dbg.declare(metadata i32 (i32, i32)** %a_fptr, metadata !102, metadata !DIExpression()), !dbg !103
  store i32 (i32, i32)* @plus, i32 (i32, i32)** %a_fptr, align 8, !dbg !103
  call void @llvm.dbg.declare(metadata i32 (i32, i32)** %s_fptr, metadata !104, metadata !DIExpression()), !dbg !105
  store i32 (i32, i32)* @minus, i32 (i32, i32)** %s_fptr, align 8, !dbg !105
  call void @llvm.dbg.declare(metadata i32 (i32, i32)* (i32, i32, i32 (i32, i32)*, i32 (i32, i32)*)** %goo_ptr, metadata !106, metadata !DIExpression()), !dbg !107
  store i32 (i32, i32)* (i32, i32, i32 (i32, i32)*, i32 (i32, i32)*)* @clever, i32 (i32, i32)* (i32, i32, i32 (i32, i32)*, i32 (i32, i32)*)** %goo_ptr, align 8, !dbg !107
  %0 = load i8, i8* %x.addr, align 1, !dbg !108
  %conv = sext i8 %0 to i32, !dbg !108
  %cmp = icmp eq i32 %conv, 43, !dbg !110
  br i1 %cmp, label %if.then, label %if.end, !dbg !111

if.then:                                          ; preds = %entry
  store i32 (i32, i32)* (i32, i32, i32 (i32, i32)*, i32 (i32, i32)*)* @foo, i32 (i32, i32)* (i32, i32, i32 (i32, i32)*, i32 (i32, i32)*)** %goo_ptr, align 8, !dbg !112
  br label %if.end, !dbg !114

if.end:                                           ; preds = %if.then, %entry
  call void @llvm.dbg.declare(metadata i32 (i32, i32)** %t_fptr, metadata !115, metadata !DIExpression()), !dbg !116
  %1 = load i32 (i32, i32)* (i32, i32, i32 (i32, i32)*, i32 (i32, i32)*)*, i32 (i32, i32)* (i32, i32, i32 (i32, i32)*, i32 (i32, i32)*)** %goo_ptr, align 8, !dbg !117
  %2 = load i32, i32* %op1.addr, align 4, !dbg !118
  %3 = load i32, i32* %op2.addr, align 4, !dbg !119
  %4 = load i32 (i32, i32)*, i32 (i32, i32)** %s_fptr, align 8, !dbg !120
  %5 = load i32 (i32, i32)*, i32 (i32, i32)** %a_fptr, align 8, !dbg !121
  %call = call i32 (i32, i32)* @clever1(i32 (i32, i32)* (i32, i32, i32 (i32, i32)*, i32 (i32, i32)*)* %1, i32 %2, i32 %3, i32 (i32, i32)* %4, i32 (i32, i32)* %5), !dbg !122
  store i32 (i32, i32)* %call, i32 (i32, i32)** %t_fptr, align 8, !dbg !116
  %6 = load i32 (i32, i32)*, i32 (i32, i32)** %t_fptr, align 8, !dbg !123
  %7 = load i32, i32* %op1.addr, align 4, !dbg !124
  %8 = load i32, i32* %op2.addr, align 4, !dbg !125
  %call2 = call i32 %6(i32 %7, i32 %8), !dbg !123
  ret i32 0, !dbg !126
}

attributes #0 = { noinline nounwind optnone uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind readnone speculatable willreturn }

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!3, !4, !5}
!llvm.ident = !{!6}

!0 = distinct !DICompileUnit(language: DW_LANG_C99, file: !1, producer: "clang version 10.0.0 ", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, enums: !2, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "test19.c", directory: "/root/assign2/test")
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
!71 = !DILocalVariable(name: "f_fptr", scope: !57, file: !1, line: 18, type: !31)
!72 = !DILocation(line: 18, column: 10, scope: !57)
!73 = !DILocation(line: 18, column: 30, scope: !57)
!74 = !DILocation(line: 18, column: 38, scope: !57)
!75 = !DILocation(line: 18, column: 40, scope: !57)
!76 = !DILocation(line: 18, column: 42, scope: !57)
!77 = !DILocation(line: 18, column: 49, scope: !57)
!78 = !DILocation(line: 19, column: 8, scope: !79)
!79 = distinct !DILexicalBlock(scope: !57, file: !1, line: 19, column: 8)
!80 = !DILocation(line: 19, column: 10, scope: !79)
!81 = !DILocation(line: 19, column: 8, scope: !57)
!82 = !DILocation(line: 21, column: 16, scope: !83)
!83 = distinct !DILexicalBlock(scope: !79, file: !1, line: 20, column: 4)
!84 = !DILocation(line: 22, column: 16, scope: !83)
!85 = !DILocation(line: 23, column: 4, scope: !83)
!86 = !DILocation(line: 24, column: 11, scope: !57)
!87 = !DILocation(line: 24, column: 19, scope: !57)
!88 = !DILocation(line: 24, column: 21, scope: !57)
!89 = !DILocation(line: 24, column: 23, scope: !57)
!90 = !DILocation(line: 24, column: 30, scope: !57)
!91 = !DILocation(line: 24, column: 4, scope: !57)
!92 = distinct !DISubprogram(name: "moo", scope: !1, file: !1, line: 26, type: !93, scopeLine: 26, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !2)
!93 = !DISubroutineType(types: !94)
!94 = !{!10, !95, !10, !10}
!95 = !DIBasicType(name: "char", size: 8, encoding: DW_ATE_signed_char)
!96 = !DILocalVariable(name: "x", arg: 1, scope: !92, file: !1, line: 26, type: !95)
!97 = !DILocation(line: 26, column: 14, scope: !92)
!98 = !DILocalVariable(name: "op1", arg: 2, scope: !92, file: !1, line: 26, type: !10)
!99 = !DILocation(line: 26, column: 21, scope: !92)
!100 = !DILocalVariable(name: "op2", arg: 3, scope: !92, file: !1, line: 26, type: !10)
!101 = !DILocation(line: 26, column: 30, scope: !92)
!102 = !DILocalVariable(name: "a_fptr", scope: !92, file: !1, line: 27, type: !31)
!103 = !DILocation(line: 27, column: 11, scope: !92)
!104 = !DILocalVariable(name: "s_fptr", scope: !92, file: !1, line: 28, type: !31)
!105 = !DILocation(line: 28, column: 11, scope: !92)
!106 = !DILocalVariable(name: "goo_ptr", scope: !92, file: !1, line: 29, type: !60)
!107 = !DILocation(line: 29, column: 14, scope: !92)
!108 = !DILocation(line: 31, column: 9, scope: !109)
!109 = distinct !DILexicalBlock(scope: !92, file: !1, line: 31, column: 9)
!110 = !DILocation(line: 31, column: 11, scope: !109)
!111 = !DILocation(line: 31, column: 9, scope: !92)
!112 = !DILocation(line: 33, column: 17, scope: !113)
!113 = distinct !DILexicalBlock(scope: !109, file: !1, line: 32, column: 5)
!114 = !DILocation(line: 34, column: 5, scope: !113)
!115 = !DILocalVariable(name: "t_fptr", scope: !92, file: !1, line: 36, type: !31)
!116 = !DILocation(line: 36, column: 11, scope: !92)
!117 = !DILocation(line: 36, column: 39, scope: !92)
!118 = !DILocation(line: 36, column: 48, scope: !92)
!119 = !DILocation(line: 36, column: 53, scope: !92)
!120 = !DILocation(line: 36, column: 58, scope: !92)
!121 = !DILocation(line: 36, column: 66, scope: !92)
!122 = !DILocation(line: 36, column: 31, scope: !92)
!123 = !DILocation(line: 37, column: 5, scope: !92)
!124 = !DILocation(line: 37, column: 12, scope: !92)
!125 = !DILocation(line: 37, column: 17, scope: !92)
!126 = !DILocation(line: 39, column: 5, scope: !92)
