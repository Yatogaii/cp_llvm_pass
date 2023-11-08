; ModuleID = 'test11.bc'
source_filename = "test11.c"
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
define dso_local i32 @clever(i32 %a, i32 %b, i32 (i32, i32)* %a_fptr, i32 (i32, i32)* %b_fptr) #0 !dbg !42 {
entry:
  %a.addr = alloca i32, align 4
  %b.addr = alloca i32, align 4
  %a_fptr.addr = alloca i32 (i32, i32)*, align 8
  %b_fptr.addr = alloca i32 (i32, i32)*, align 8
  %s_fptr = alloca i32 (i32, i32)*, align 8
  store i32 %a, i32* %a.addr, align 4
  call void @llvm.dbg.declare(metadata i32* %a.addr, metadata !45, metadata !DIExpression()), !dbg !46
  store i32 %b, i32* %b.addr, align 4
  call void @llvm.dbg.declare(metadata i32* %b.addr, metadata !47, metadata !DIExpression()), !dbg !48
  store i32 (i32, i32)* %a_fptr, i32 (i32, i32)** %a_fptr.addr, align 8
  call void @llvm.dbg.declare(metadata i32 (i32, i32)** %a_fptr.addr, metadata !49, metadata !DIExpression()), !dbg !50
  store i32 (i32, i32)* %b_fptr, i32 (i32, i32)** %b_fptr.addr, align 8
  call void @llvm.dbg.declare(metadata i32 (i32, i32)** %b_fptr.addr, metadata !51, metadata !DIExpression()), !dbg !52
  call void @llvm.dbg.declare(metadata i32 (i32, i32)** %s_fptr, metadata !53, metadata !DIExpression()), !dbg !54
  %0 = load i32, i32* %a.addr, align 4, !dbg !55
  %1 = load i32, i32* %b.addr, align 4, !dbg !56
  %2 = load i32 (i32, i32)*, i32 (i32, i32)** %a_fptr.addr, align 8, !dbg !57
  %3 = load i32 (i32, i32)*, i32 (i32, i32)** %b_fptr.addr, align 8, !dbg !58
  %call = call i32 (i32, i32)* @foo(i32 %0, i32 %1, i32 (i32, i32)* %2, i32 (i32, i32)* %3), !dbg !59
  store i32 (i32, i32)* %call, i32 (i32, i32)** %s_fptr, align 8, !dbg !60
  %4 = load i32 (i32, i32)*, i32 (i32, i32)** %s_fptr, align 8, !dbg !61
  %5 = load i32, i32* %a.addr, align 4, !dbg !62
  %6 = load i32, i32* %b.addr, align 4, !dbg !63
  %call1 = call i32 %4(i32 %5, i32 %6), !dbg !61
  ret i32 %call1, !dbg !64
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @moo(i8 signext %x, i32 %op1, i32 %op2) #0 !dbg !65 {
entry:
  %x.addr = alloca i8, align 1
  %op1.addr = alloca i32, align 4
  %op2.addr = alloca i32, align 4
  %a_fptr = alloca i32 (i32, i32)*, align 8
  %s_fptr = alloca i32 (i32, i32)*, align 8
  %t_fptr = alloca i32 (i32, i32)*, align 8
  %result = alloca i32, align 4
  store i8 %x, i8* %x.addr, align 1
  call void @llvm.dbg.declare(metadata i8* %x.addr, metadata !69, metadata !DIExpression()), !dbg !70
  store i32 %op1, i32* %op1.addr, align 4
  call void @llvm.dbg.declare(metadata i32* %op1.addr, metadata !71, metadata !DIExpression()), !dbg !72
  store i32 %op2, i32* %op2.addr, align 4
  call void @llvm.dbg.declare(metadata i32* %op2.addr, metadata !73, metadata !DIExpression()), !dbg !74
  call void @llvm.dbg.declare(metadata i32 (i32, i32)** %a_fptr, metadata !75, metadata !DIExpression()), !dbg !76
  store i32 (i32, i32)* @plus, i32 (i32, i32)** %a_fptr, align 8, !dbg !76
  call void @llvm.dbg.declare(metadata i32 (i32, i32)** %s_fptr, metadata !77, metadata !DIExpression()), !dbg !78
  store i32 (i32, i32)* @minus, i32 (i32, i32)** %s_fptr, align 8, !dbg !78
  call void @llvm.dbg.declare(metadata i32 (i32, i32)** %t_fptr, metadata !79, metadata !DIExpression()), !dbg !80
  store i32 (i32, i32)* null, i32 (i32, i32)** %t_fptr, align 8, !dbg !80
  %0 = load i8, i8* %x.addr, align 1, !dbg !81
  %conv = sext i8 %0 to i32, !dbg !81
  %cmp = icmp eq i32 %conv, 43, !dbg !83
  br i1 %cmp, label %if.then, label %if.else, !dbg !84

if.then:                                          ; preds = %entry
  %1 = load i32 (i32, i32)*, i32 (i32, i32)** %a_fptr, align 8, !dbg !85
  store i32 (i32, i32)* %1, i32 (i32, i32)** %t_fptr, align 8, !dbg !87
  br label %if.end6, !dbg !88

if.else:                                          ; preds = %entry
  %2 = load i8, i8* %x.addr, align 1, !dbg !89
  %conv2 = sext i8 %2 to i32, !dbg !89
  %cmp3 = icmp eq i32 %conv2, 45, !dbg !91
  br i1 %cmp3, label %if.then5, label %if.end, !dbg !92

if.then5:                                         ; preds = %if.else
  %3 = load i32 (i32, i32)*, i32 (i32, i32)** %s_fptr, align 8, !dbg !93
  store i32 (i32, i32)* %3, i32 (i32, i32)** %t_fptr, align 8, !dbg !95
  br label %if.end, !dbg !96

if.end:                                           ; preds = %if.then5, %if.else
  br label %if.end6

if.end6:                                          ; preds = %if.end, %if.then
  call void @llvm.dbg.declare(metadata i32* %result, metadata !97, metadata !DIExpression()), !dbg !99
  %4 = load i32, i32* %op1.addr, align 4, !dbg !100
  %5 = load i32, i32* %op2.addr, align 4, !dbg !101
  %6 = load i32 (i32, i32)*, i32 (i32, i32)** %a_fptr, align 8, !dbg !102
  %7 = load i32 (i32, i32)*, i32 (i32, i32)** %t_fptr, align 8, !dbg !103
  %call = call i32 @clever(i32 %4, i32 %5, i32 (i32, i32)* %6, i32 (i32, i32)* %7), !dbg !104
  store i32 %call, i32* %result, align 4, !dbg !99
  ret i32 0, !dbg !105
}

attributes #0 = { noinline nounwind optnone uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind readnone speculatable willreturn }

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!3, !4, !5}
!llvm.ident = !{!6}

!0 = distinct !DICompileUnit(language: DW_LANG_C99, file: !1, producer: "clang version 10.0.0 ", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, enums: !2, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "test11.c", directory: "/root/assign2/test")
!2 = !{}
!3 = !{i32 7, !"Dwarf Version", i32 4}
!4 = !{i32 2, !"Debug Info Version", i32 3}
!5 = !{i32 1, !"wchar_size", i32 4}
!6 = !{!"clang version 10.0.0 "}
!7 = distinct !DISubprogram(name: "plus", scope: !1, file: !1, line: 1, type: !8, scopeLine: 1, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !2)
!8 = !DISubroutineType(types: !9)
!9 = !{!10, !10, !10}
!10 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!11 = !DILocalVariable(name: "a", arg: 1, scope: !7, file: !1, line: 1, type: !10)
!12 = !DILocation(line: 1, column: 14, scope: !7)
!13 = !DILocalVariable(name: "b", arg: 2, scope: !7, file: !1, line: 1, type: !10)
!14 = !DILocation(line: 1, column: 21, scope: !7)
!15 = !DILocation(line: 2, column: 11, scope: !7)
!16 = !DILocation(line: 2, column: 13, scope: !7)
!17 = !DILocation(line: 2, column: 12, scope: !7)
!18 = !DILocation(line: 2, column: 4, scope: !7)
!19 = distinct !DISubprogram(name: "minus", scope: !1, file: !1, line: 5, type: !8, scopeLine: 5, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !2)
!20 = !DILocalVariable(name: "a", arg: 1, scope: !19, file: !1, line: 5, type: !10)
!21 = !DILocation(line: 5, column: 15, scope: !19)
!22 = !DILocalVariable(name: "b", arg: 2, scope: !19, file: !1, line: 5, type: !10)
!23 = !DILocation(line: 5, column: 22, scope: !19)
!24 = !DILocation(line: 6, column: 11, scope: !19)
!25 = !DILocation(line: 6, column: 13, scope: !19)
!26 = !DILocation(line: 6, column: 12, scope: !19)
!27 = !DILocation(line: 6, column: 4, scope: !19)
!28 = distinct !DISubprogram(name: "foo", scope: !1, file: !1, line: 9, type: !29, scopeLine: 9, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !2)
!29 = !DISubroutineType(types: !30)
!30 = !{!31, !10, !10, !31, !31}
!31 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !8, size: 64)
!32 = !DILocalVariable(name: "a", arg: 1, scope: !28, file: !1, line: 9, type: !10)
!33 = !DILocation(line: 9, column: 15, scope: !28)
!34 = !DILocalVariable(name: "b", arg: 2, scope: !28, file: !1, line: 9, type: !10)
!35 = !DILocation(line: 9, column: 22, scope: !28)
!36 = !DILocalVariable(name: "a_fptr", arg: 3, scope: !28, file: !1, line: 9, type: !31)
!37 = !DILocation(line: 9, column: 31, scope: !28)
!38 = !DILocalVariable(name: "b_fptr", arg: 4, scope: !28, file: !1, line: 9, type: !31)
!39 = !DILocation(line: 9, column: 55, scope: !28)
!40 = !DILocation(line: 10, column: 11, scope: !28)
!41 = !DILocation(line: 10, column: 4, scope: !28)
!42 = distinct !DISubprogram(name: "clever", scope: !1, file: !1, line: 13, type: !43, scopeLine: 13, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !2)
!43 = !DISubroutineType(types: !44)
!44 = !{!10, !10, !10, !31, !31}
!45 = !DILocalVariable(name: "a", arg: 1, scope: !42, file: !1, line: 13, type: !10)
!46 = !DILocation(line: 13, column: 16, scope: !42)
!47 = !DILocalVariable(name: "b", arg: 2, scope: !42, file: !1, line: 13, type: !10)
!48 = !DILocation(line: 13, column: 23, scope: !42)
!49 = !DILocalVariable(name: "a_fptr", arg: 3, scope: !42, file: !1, line: 13, type: !31)
!50 = !DILocation(line: 13, column: 32, scope: !42)
!51 = !DILocalVariable(name: "b_fptr", arg: 4, scope: !42, file: !1, line: 13, type: !31)
!52 = !DILocation(line: 13, column: 56, scope: !42)
!53 = !DILocalVariable(name: "s_fptr", scope: !42, file: !1, line: 14, type: !31)
!54 = !DILocation(line: 14, column: 10, scope: !42)
!55 = !DILocation(line: 15, column: 17, scope: !42)
!56 = !DILocation(line: 15, column: 20, scope: !42)
!57 = !DILocation(line: 15, column: 23, scope: !42)
!58 = !DILocation(line: 15, column: 31, scope: !42)
!59 = !DILocation(line: 15, column: 13, scope: !42)
!60 = !DILocation(line: 15, column: 11, scope: !42)
!61 = !DILocation(line: 16, column: 11, scope: !42)
!62 = !DILocation(line: 16, column: 18, scope: !42)
!63 = !DILocation(line: 16, column: 21, scope: !42)
!64 = !DILocation(line: 16, column: 4, scope: !42)
!65 = distinct !DISubprogram(name: "moo", scope: !1, file: !1, line: 20, type: !66, scopeLine: 20, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !2)
!66 = !DISubroutineType(types: !67)
!67 = !{!10, !68, !10, !10}
!68 = !DIBasicType(name: "char", size: 8, encoding: DW_ATE_signed_char)
!69 = !DILocalVariable(name: "x", arg: 1, scope: !65, file: !1, line: 20, type: !68)
!70 = !DILocation(line: 20, column: 14, scope: !65)
!71 = !DILocalVariable(name: "op1", arg: 2, scope: !65, file: !1, line: 20, type: !10)
!72 = !DILocation(line: 20, column: 21, scope: !65)
!73 = !DILocalVariable(name: "op2", arg: 3, scope: !65, file: !1, line: 20, type: !10)
!74 = !DILocation(line: 20, column: 30, scope: !65)
!75 = !DILocalVariable(name: "a_fptr", scope: !65, file: !1, line: 21, type: !31)
!76 = !DILocation(line: 21, column: 11, scope: !65)
!77 = !DILocalVariable(name: "s_fptr", scope: !65, file: !1, line: 22, type: !31)
!78 = !DILocation(line: 22, column: 11, scope: !65)
!79 = !DILocalVariable(name: "t_fptr", scope: !65, file: !1, line: 23, type: !31)
!80 = !DILocation(line: 23, column: 11, scope: !65)
!81 = !DILocation(line: 25, column: 9, scope: !82)
!82 = distinct !DILexicalBlock(scope: !65, file: !1, line: 25, column: 9)
!83 = !DILocation(line: 25, column: 11, scope: !82)
!84 = !DILocation(line: 25, column: 9, scope: !65)
!85 = !DILocation(line: 26, column: 17, scope: !86)
!86 = distinct !DILexicalBlock(scope: !82, file: !1, line: 25, column: 19)
!87 = !DILocation(line: 26, column: 15, scope: !86)
!88 = !DILocation(line: 27, column: 5, scope: !86)
!89 = !DILocation(line: 28, column: 14, scope: !90)
!90 = distinct !DILexicalBlock(scope: !82, file: !1, line: 28, column: 14)
!91 = !DILocation(line: 28, column: 16, scope: !90)
!92 = !DILocation(line: 28, column: 14, scope: !82)
!93 = !DILocation(line: 29, column: 17, scope: !94)
!94 = distinct !DILexicalBlock(scope: !90, file: !1, line: 28, column: 24)
!95 = !DILocation(line: 29, column: 15, scope: !94)
!96 = !DILocation(line: 30, column: 5, scope: !94)
!97 = !DILocalVariable(name: "result", scope: !65, file: !1, line: 32, type: !98)
!98 = !DIBasicType(name: "unsigned int", size: 32, encoding: DW_ATE_unsigned)
!99 = !DILocation(line: 32, column: 14, scope: !65)
!100 = !DILocation(line: 32, column: 30, scope: !65)
!101 = !DILocation(line: 32, column: 35, scope: !65)
!102 = !DILocation(line: 32, column: 40, scope: !65)
!103 = !DILocation(line: 32, column: 48, scope: !65)
!104 = !DILocation(line: 32, column: 23, scope: !65)
!105 = !DILocation(line: 33, column: 5, scope: !65)
