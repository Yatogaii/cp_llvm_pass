; ModuleID = 'test08.bc'
source_filename = "test08.c"
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
define dso_local i32 @clever(i32 %a, i32 %b, i32 (i32, i32)* %a_fptr) #0 !dbg !28 {
entry:
  %a.addr = alloca i32, align 4
  %b.addr = alloca i32, align 4
  %a_fptr.addr = alloca i32 (i32, i32)*, align 8
  store i32 %a, i32* %a.addr, align 4
  call void @llvm.dbg.declare(metadata i32* %a.addr, metadata !32, metadata !DIExpression()), !dbg !33
  store i32 %b, i32* %b.addr, align 4
  call void @llvm.dbg.declare(metadata i32* %b.addr, metadata !34, metadata !DIExpression()), !dbg !35
  store i32 (i32, i32)* %a_fptr, i32 (i32, i32)** %a_fptr.addr, align 8
  call void @llvm.dbg.declare(metadata i32 (i32, i32)** %a_fptr.addr, metadata !36, metadata !DIExpression()), !dbg !37
  %0 = load i32 (i32, i32)*, i32 (i32, i32)** %a_fptr.addr, align 8, !dbg !38
  %1 = load i32, i32* %a.addr, align 4, !dbg !39
  %2 = load i32, i32* %b.addr, align 4, !dbg !40
  %call = call i32 %0(i32 %1, i32 %2), !dbg !38
  ret i32 %call, !dbg !41
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @moo(i8 signext %x, i32 %op1, i32 %op2) #0 !dbg !42 {
entry:
  %x.addr = alloca i8, align 1
  %op1.addr = alloca i32, align 4
  %op2.addr = alloca i32, align 4
  %a_fptr = alloca i32 (i32, i32)*, align 8
  %s_fptr = alloca i32 (i32, i32)*, align 8
  %t_fptr = alloca i32 (i32, i32)*, align 8
  %result = alloca i32, align 4
  store i8 %x, i8* %x.addr, align 1
  call void @llvm.dbg.declare(metadata i8* %x.addr, metadata !46, metadata !DIExpression()), !dbg !47
  store i32 %op1, i32* %op1.addr, align 4
  call void @llvm.dbg.declare(metadata i32* %op1.addr, metadata !48, metadata !DIExpression()), !dbg !49
  store i32 %op2, i32* %op2.addr, align 4
  call void @llvm.dbg.declare(metadata i32* %op2.addr, metadata !50, metadata !DIExpression()), !dbg !51
  call void @llvm.dbg.declare(metadata i32 (i32, i32)** %a_fptr, metadata !52, metadata !DIExpression()), !dbg !53
  store i32 (i32, i32)* @plus, i32 (i32, i32)** %a_fptr, align 8, !dbg !53
  call void @llvm.dbg.declare(metadata i32 (i32, i32)** %s_fptr, metadata !54, metadata !DIExpression()), !dbg !55
  store i32 (i32, i32)* @minus, i32 (i32, i32)** %s_fptr, align 8, !dbg !55
  call void @llvm.dbg.declare(metadata i32 (i32, i32)** %t_fptr, metadata !56, metadata !DIExpression()), !dbg !57
  store i32 (i32, i32)* null, i32 (i32, i32)** %t_fptr, align 8, !dbg !57
  %0 = load i8, i8* %x.addr, align 1, !dbg !58
  %conv = sext i8 %0 to i32, !dbg !58
  %cmp = icmp eq i32 %conv, 43, !dbg !60
  br i1 %cmp, label %if.then, label %if.else, !dbg !61

if.then:                                          ; preds = %entry
  %1 = load i32 (i32, i32)*, i32 (i32, i32)** %a_fptr, align 8, !dbg !62
  store i32 (i32, i32)* %1, i32 (i32, i32)** %t_fptr, align 8, !dbg !64
  br label %if.end6, !dbg !65

if.else:                                          ; preds = %entry
  %2 = load i8, i8* %x.addr, align 1, !dbg !66
  %conv2 = sext i8 %2 to i32, !dbg !66
  %cmp3 = icmp eq i32 %conv2, 45, !dbg !68
  br i1 %cmp3, label %if.then5, label %if.end, !dbg !69

if.then5:                                         ; preds = %if.else
  %3 = load i32 (i32, i32)*, i32 (i32, i32)** %s_fptr, align 8, !dbg !70
  store i32 (i32, i32)* %3, i32 (i32, i32)** %t_fptr, align 8, !dbg !72
  br label %if.end, !dbg !73

if.end:                                           ; preds = %if.then5, %if.else
  br label %if.end6

if.end6:                                          ; preds = %if.end, %if.then
  call void @llvm.dbg.declare(metadata i32* %result, metadata !74, metadata !DIExpression()), !dbg !76
  %4 = load i32, i32* %op1.addr, align 4, !dbg !77
  %5 = load i32, i32* %op2.addr, align 4, !dbg !78
  %6 = load i32 (i32, i32)*, i32 (i32, i32)** %t_fptr, align 8, !dbg !79
  %call = call i32 @clever(i32 %4, i32 %5, i32 (i32, i32)* %6), !dbg !80
  store i32 %call, i32* %result, align 4, !dbg !76
  ret i32 0, !dbg !81
}

attributes #0 = { noinline nounwind optnone uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind readnone speculatable willreturn }

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!3, !4, !5}
!llvm.ident = !{!6}

!0 = distinct !DICompileUnit(language: DW_LANG_C99, file: !1, producer: "clang version 10.0.0 ", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, enums: !2, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "test08.c", directory: "/root/assign2/test")
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
!28 = distinct !DISubprogram(name: "clever", scope: !1, file: !1, line: 9, type: !29, scopeLine: 9, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !2)
!29 = !DISubroutineType(types: !30)
!30 = !{!10, !10, !10, !31}
!31 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !8, size: 64)
!32 = !DILocalVariable(name: "a", arg: 1, scope: !28, file: !1, line: 9, type: !10)
!33 = !DILocation(line: 9, column: 16, scope: !28)
!34 = !DILocalVariable(name: "b", arg: 2, scope: !28, file: !1, line: 9, type: !10)
!35 = !DILocation(line: 9, column: 23, scope: !28)
!36 = !DILocalVariable(name: "a_fptr", arg: 3, scope: !28, file: !1, line: 9, type: !31)
!37 = !DILocation(line: 9, column: 32, scope: !28)
!38 = !DILocation(line: 10, column: 12, scope: !28)
!39 = !DILocation(line: 10, column: 19, scope: !28)
!40 = !DILocation(line: 10, column: 22, scope: !28)
!41 = !DILocation(line: 10, column: 5, scope: !28)
!42 = distinct !DISubprogram(name: "moo", scope: !1, file: !1, line: 14, type: !43, scopeLine: 14, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !2)
!43 = !DISubroutineType(types: !44)
!44 = !{!10, !45, !10, !10}
!45 = !DIBasicType(name: "char", size: 8, encoding: DW_ATE_signed_char)
!46 = !DILocalVariable(name: "x", arg: 1, scope: !42, file: !1, line: 14, type: !45)
!47 = !DILocation(line: 14, column: 14, scope: !42)
!48 = !DILocalVariable(name: "op1", arg: 2, scope: !42, file: !1, line: 14, type: !10)
!49 = !DILocation(line: 14, column: 21, scope: !42)
!50 = !DILocalVariable(name: "op2", arg: 3, scope: !42, file: !1, line: 14, type: !10)
!51 = !DILocation(line: 14, column: 30, scope: !42)
!52 = !DILocalVariable(name: "a_fptr", scope: !42, file: !1, line: 15, type: !31)
!53 = !DILocation(line: 15, column: 11, scope: !42)
!54 = !DILocalVariable(name: "s_fptr", scope: !42, file: !1, line: 16, type: !31)
!55 = !DILocation(line: 16, column: 11, scope: !42)
!56 = !DILocalVariable(name: "t_fptr", scope: !42, file: !1, line: 17, type: !31)
!57 = !DILocation(line: 17, column: 11, scope: !42)
!58 = !DILocation(line: 19, column: 9, scope: !59)
!59 = distinct !DILexicalBlock(scope: !42, file: !1, line: 19, column: 9)
!60 = !DILocation(line: 19, column: 11, scope: !59)
!61 = !DILocation(line: 19, column: 9, scope: !42)
!62 = !DILocation(line: 20, column: 17, scope: !63)
!63 = distinct !DILexicalBlock(scope: !59, file: !1, line: 19, column: 19)
!64 = !DILocation(line: 20, column: 15, scope: !63)
!65 = !DILocation(line: 21, column: 5, scope: !63)
!66 = !DILocation(line: 22, column: 14, scope: !67)
!67 = distinct !DILexicalBlock(scope: !59, file: !1, line: 22, column: 14)
!68 = !DILocation(line: 22, column: 16, scope: !67)
!69 = !DILocation(line: 22, column: 14, scope: !59)
!70 = !DILocation(line: 23, column: 17, scope: !71)
!71 = distinct !DILexicalBlock(scope: !67, file: !1, line: 22, column: 24)
!72 = !DILocation(line: 23, column: 15, scope: !71)
!73 = !DILocation(line: 24, column: 5, scope: !71)
!74 = !DILocalVariable(name: "result", scope: !42, file: !1, line: 26, type: !75)
!75 = !DIBasicType(name: "unsigned int", size: 32, encoding: DW_ATE_unsigned)
!76 = !DILocation(line: 26, column: 14, scope: !42)
!77 = !DILocation(line: 26, column: 30, scope: !42)
!78 = !DILocation(line: 26, column: 35, scope: !42)
!79 = !DILocation(line: 26, column: 40, scope: !42)
!80 = !DILocation(line: 26, column: 23, scope: !42)
!81 = !DILocation(line: 27, column: 5, scope: !42)
