; ModuleID = 'test03.bc'
source_filename = "test03.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @plus(i32 %a, i32 %b) #0 !dbg !9 {
entry:
  %a.addr = alloca i32, align 4
  %b.addr = alloca i32, align 4
  store i32 %a, i32* %a.addr, align 4
  call void @llvm.dbg.declare(metadata i32* %a.addr, metadata !13, metadata !DIExpression()), !dbg !14
  store i32 %b, i32* %b.addr, align 4
  call void @llvm.dbg.declare(metadata i32* %b.addr, metadata !15, metadata !DIExpression()), !dbg !16
  %0 = load i32, i32* %a.addr, align 4, !dbg !17
  %1 = load i32, i32* %b.addr, align 4, !dbg !18
  %add = add nsw i32 %0, %1, !dbg !19
  ret i32 %add, !dbg !20
}

; Function Attrs: nounwind readnone speculatable willreturn
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @minus(i32 %a, i32 %b) #0 !dbg !21 {
entry:
  %a.addr = alloca i32, align 4
  %b.addr = alloca i32, align 4
  store i32 %a, i32* %a.addr, align 4
  call void @llvm.dbg.declare(metadata i32* %a.addr, metadata !22, metadata !DIExpression()), !dbg !23
  store i32 %b, i32* %b.addr, align 4
  call void @llvm.dbg.declare(metadata i32* %b.addr, metadata !24, metadata !DIExpression()), !dbg !25
  %0 = load i32, i32* %a.addr, align 4, !dbg !26
  %1 = load i32, i32* %b.addr, align 4, !dbg !27
  %sub = sub nsw i32 %0, %1, !dbg !28
  ret i32 %sub, !dbg !29
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @clever(i32 %x) #0 !dbg !30 {
entry:
  %x.addr = alloca i32, align 4
  %a_fptr = alloca i32 (i32, i32)*, align 8
  %s_fptr = alloca i32 (i32, i32)*, align 8
  %t_fptr = alloca i32 (i32, i32)*, align 8
  %op1 = alloca i32, align 4
  %op2 = alloca i32, align 4
  %result = alloca i32, align 4
  store i32 %x, i32* %x.addr, align 4
  call void @llvm.dbg.declare(metadata i32* %x.addr, metadata !33, metadata !DIExpression()), !dbg !34
  call void @llvm.dbg.declare(metadata i32 (i32, i32)** %a_fptr, metadata !35, metadata !DIExpression()), !dbg !37
  store i32 (i32, i32)* @plus, i32 (i32, i32)** %a_fptr, align 8, !dbg !37
  call void @llvm.dbg.declare(metadata i32 (i32, i32)** %s_fptr, metadata !38, metadata !DIExpression()), !dbg !39
  store i32 (i32, i32)* @minus, i32 (i32, i32)** %s_fptr, align 8, !dbg !39
  call void @llvm.dbg.declare(metadata i32 (i32, i32)** %t_fptr, metadata !40, metadata !DIExpression()), !dbg !41
  store i32 (i32, i32)* null, i32 (i32, i32)** %t_fptr, align 8, !dbg !41
  call void @llvm.dbg.declare(metadata i32* %op1, metadata !42, metadata !DIExpression()), !dbg !43
  store i32 1, i32* %op1, align 4, !dbg !43
  call void @llvm.dbg.declare(metadata i32* %op2, metadata !44, metadata !DIExpression()), !dbg !45
  store i32 2, i32* %op2, align 4, !dbg !45
  %0 = load i32, i32* %x.addr, align 4, !dbg !46
  %cmp = icmp eq i32 %0, 3, !dbg !48
  br i1 %cmp, label %if.then, label %if.end, !dbg !49

if.then:                                          ; preds = %entry
  %1 = load i32 (i32, i32)*, i32 (i32, i32)** %a_fptr, align 8, !dbg !50
  store i32 (i32, i32)* %1, i32 (i32, i32)** %t_fptr, align 8, !dbg !52
  br label %if.end, !dbg !53

if.end:                                           ; preds = %if.then, %entry
  %2 = load i32, i32* %x.addr, align 4, !dbg !54
  %cmp1 = icmp eq i32 %2, 4, !dbg !56
  br i1 %cmp1, label %if.then2, label %if.end3, !dbg !57

if.then2:                                         ; preds = %if.end
  %3 = load i32 (i32, i32)*, i32 (i32, i32)** %s_fptr, align 8, !dbg !58
  store i32 (i32, i32)* %3, i32 (i32, i32)** %t_fptr, align 8, !dbg !60
  br label %if.end3, !dbg !61

if.end3:                                          ; preds = %if.then2, %if.end
  %4 = load i32 (i32, i32)*, i32 (i32, i32)** %t_fptr, align 8, !dbg !62
  %cmp4 = icmp ne i32 (i32, i32)* %4, null, !dbg !64
  br i1 %cmp4, label %if.then5, label %if.end6, !dbg !65

if.then5:                                         ; preds = %if.end3
  call void @llvm.dbg.declare(metadata i32* %result, metadata !66, metadata !DIExpression()), !dbg !69
  %5 = load i32 (i32, i32)*, i32 (i32, i32)** %t_fptr, align 8, !dbg !70
  %6 = load i32, i32* %op1, align 4, !dbg !71
  %7 = load i32, i32* %op2, align 4, !dbg !72
  %call = call i32 %5(i32 %6, i32 %7), !dbg !70
  store i32 %call, i32* %result, align 4, !dbg !69
  br label %if.end6, !dbg !73

if.end6:                                          ; preds = %if.then5, %if.end3
  ret i32 0, !dbg !74
}

attributes #0 = { noinline nounwind optnone uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind readnone speculatable willreturn }

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!5, !6, !7}
!llvm.ident = !{!8}

!0 = distinct !DICompileUnit(language: DW_LANG_C99, file: !1, producer: "clang version 10.0.0 ", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, enums: !2, retainedTypes: !3, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "test03.c", directory: "/root/assign2/test")
!2 = !{}
!3 = !{!4}
!4 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: null, size: 64)
!5 = !{i32 7, !"Dwarf Version", i32 4}
!6 = !{i32 2, !"Debug Info Version", i32 3}
!7 = !{i32 1, !"wchar_size", i32 4}
!8 = !{!"clang version 10.0.0 "}
!9 = distinct !DISubprogram(name: "plus", scope: !1, file: !1, line: 3, type: !10, scopeLine: 3, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !2)
!10 = !DISubroutineType(types: !11)
!11 = !{!12, !12, !12}
!12 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!13 = !DILocalVariable(name: "a", arg: 1, scope: !9, file: !1, line: 3, type: !12)
!14 = !DILocation(line: 3, column: 14, scope: !9)
!15 = !DILocalVariable(name: "b", arg: 2, scope: !9, file: !1, line: 3, type: !12)
!16 = !DILocation(line: 3, column: 21, scope: !9)
!17 = !DILocation(line: 4, column: 11, scope: !9)
!18 = !DILocation(line: 4, column: 13, scope: !9)
!19 = !DILocation(line: 4, column: 12, scope: !9)
!20 = !DILocation(line: 4, column: 4, scope: !9)
!21 = distinct !DISubprogram(name: "minus", scope: !1, file: !1, line: 7, type: !10, scopeLine: 7, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !2)
!22 = !DILocalVariable(name: "a", arg: 1, scope: !21, file: !1, line: 7, type: !12)
!23 = !DILocation(line: 7, column: 15, scope: !21)
!24 = !DILocalVariable(name: "b", arg: 2, scope: !21, file: !1, line: 7, type: !12)
!25 = !DILocation(line: 7, column: 22, scope: !21)
!26 = !DILocation(line: 8, column: 11, scope: !21)
!27 = !DILocation(line: 8, column: 13, scope: !21)
!28 = !DILocation(line: 8, column: 12, scope: !21)
!29 = !DILocation(line: 8, column: 4, scope: !21)
!30 = distinct !DISubprogram(name: "clever", scope: !1, file: !1, line: 11, type: !31, scopeLine: 11, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !2)
!31 = !DISubroutineType(types: !32)
!32 = !{!12, !12}
!33 = !DILocalVariable(name: "x", arg: 1, scope: !30, file: !1, line: 11, type: !12)
!34 = !DILocation(line: 11, column: 16, scope: !30)
!35 = !DILocalVariable(name: "a_fptr", scope: !30, file: !1, line: 12, type: !36)
!36 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !10, size: 64)
!37 = !DILocation(line: 12, column: 11, scope: !30)
!38 = !DILocalVariable(name: "s_fptr", scope: !30, file: !1, line: 13, type: !36)
!39 = !DILocation(line: 13, column: 11, scope: !30)
!40 = !DILocalVariable(name: "t_fptr", scope: !30, file: !1, line: 14, type: !36)
!41 = !DILocation(line: 14, column: 11, scope: !30)
!42 = !DILocalVariable(name: "op1", scope: !30, file: !1, line: 16, type: !12)
!43 = !DILocation(line: 16, column: 9, scope: !30)
!44 = !DILocalVariable(name: "op2", scope: !30, file: !1, line: 16, type: !12)
!45 = !DILocation(line: 16, column: 16, scope: !30)
!46 = !DILocation(line: 18, column: 9, scope: !47)
!47 = distinct !DILexicalBlock(scope: !30, file: !1, line: 18, column: 9)
!48 = !DILocation(line: 18, column: 11, scope: !47)
!49 = !DILocation(line: 18, column: 9, scope: !30)
!50 = !DILocation(line: 19, column: 17, scope: !51)
!51 = distinct !DILexicalBlock(scope: !47, file: !1, line: 18, column: 17)
!52 = !DILocation(line: 19, column: 15, scope: !51)
!53 = !DILocation(line: 20, column: 5, scope: !51)
!54 = !DILocation(line: 21, column: 9, scope: !55)
!55 = distinct !DILexicalBlock(scope: !30, file: !1, line: 21, column: 9)
!56 = !DILocation(line: 21, column: 11, scope: !55)
!57 = !DILocation(line: 21, column: 9, scope: !30)
!58 = !DILocation(line: 22, column: 17, scope: !59)
!59 = distinct !DILexicalBlock(scope: !55, file: !1, line: 21, column: 17)
!60 = !DILocation(line: 22, column: 15, scope: !59)
!61 = !DILocation(line: 23, column: 5, scope: !59)
!62 = !DILocation(line: 26, column: 9, scope: !63)
!63 = distinct !DILexicalBlock(scope: !30, file: !1, line: 26, column: 9)
!64 = !DILocation(line: 26, column: 16, scope: !63)
!65 = !DILocation(line: 26, column: 9, scope: !30)
!66 = !DILocalVariable(name: "result", scope: !67, file: !1, line: 27, type: !68)
!67 = distinct !DILexicalBlock(scope: !63, file: !1, line: 26, column: 25)
!68 = !DIBasicType(name: "unsigned int", size: 32, encoding: DW_ATE_unsigned)
!69 = !DILocation(line: 27, column: 17, scope: !67)
!70 = !DILocation(line: 27, column: 26, scope: !67)
!71 = !DILocation(line: 27, column: 33, scope: !67)
!72 = !DILocation(line: 27, column: 38, scope: !67)
!73 = !DILocation(line: 28, column: 5, scope: !67)
!74 = !DILocation(line: 29, column: 4, scope: !30)
