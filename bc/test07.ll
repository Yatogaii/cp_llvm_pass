; ModuleID = 'test07.bc'
source_filename = "test07.c"
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
  %af_ptr = alloca i32 (i32, i32, i32 (i32, i32)*)*, align 8
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
  call void @llvm.dbg.declare(metadata i32 (i32, i32, i32 (i32, i32)*)** %af_ptr, metadata !58, metadata !DIExpression()), !dbg !60
  store i32 (i32, i32, i32 (i32, i32)*)* @clever, i32 (i32, i32, i32 (i32, i32)*)** %af_ptr, align 8, !dbg !60
  call void @llvm.dbg.declare(metadata i32* %result, metadata !61, metadata !DIExpression()), !dbg !63
  store i32 0, i32* %result, align 4, !dbg !63
  %0 = load i8, i8* %x.addr, align 1, !dbg !64
  %conv = sext i8 %0 to i32, !dbg !64
  %cmp = icmp eq i32 %conv, 43, !dbg !66
  br i1 %cmp, label %if.then, label %if.else, !dbg !67

if.then:                                          ; preds = %entry
  %1 = load i32 (i32, i32)*, i32 (i32, i32)** %a_fptr, align 8, !dbg !68
  store i32 (i32, i32)* %1, i32 (i32, i32)** %t_fptr, align 8, !dbg !70
  br label %if.end, !dbg !71

if.else:                                          ; preds = %entry
  %2 = load i32 (i32, i32)*, i32 (i32, i32)** %s_fptr, align 8, !dbg !72
  store i32 (i32, i32)* %2, i32 (i32, i32)** %t_fptr, align 8, !dbg !74
  br label %if.end

if.end:                                           ; preds = %if.else, %if.then
  %3 = load i32, i32* %op1.addr, align 4, !dbg !75
  %cmp2 = icmp sgt i32 %3, 0, !dbg !77
  br i1 %cmp2, label %if.then4, label %if.else5, !dbg !78

if.then4:                                         ; preds = %if.end
  %4 = load i32 (i32, i32, i32 (i32, i32)*)*, i32 (i32, i32, i32 (i32, i32)*)** %af_ptr, align 8, !dbg !79
  %5 = load i32, i32* %op1.addr, align 4, !dbg !80
  %6 = load i32, i32* %op2.addr, align 4, !dbg !81
  %7 = load i32 (i32, i32)*, i32 (i32, i32)** %t_fptr, align 8, !dbg !82
  %call = call i32 %4(i32 %5, i32 %6, i32 (i32, i32)* %7), !dbg !79
  store i32 %call, i32* %result, align 4, !dbg !83
  br label %if.end7, !dbg !84

if.else5:                                         ; preds = %if.end
  %8 = load i32 (i32, i32, i32 (i32, i32)*)*, i32 (i32, i32, i32 (i32, i32)*)** %af_ptr, align 8, !dbg !85
  %9 = load i32, i32* %op1.addr, align 4, !dbg !86
  %10 = load i32, i32* %op2.addr, align 4, !dbg !87
  %11 = load i32 (i32, i32)*, i32 (i32, i32)** %a_fptr, align 8, !dbg !88
  %call6 = call i32 %8(i32 %9, i32 %10, i32 (i32, i32)* %11), !dbg !85
  store i32 %call6, i32* %result, align 4, !dbg !89
  br label %if.end7

if.end7:                                          ; preds = %if.else5, %if.then4
  %12 = load i32, i32* %result, align 4, !dbg !90
  ret i32 %12, !dbg !91
}

attributes #0 = { noinline nounwind optnone uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind readnone speculatable willreturn }

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!3, !4, !5}
!llvm.ident = !{!6}

!0 = distinct !DICompileUnit(language: DW_LANG_C99, file: !1, producer: "clang version 10.0.0 ", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, enums: !2, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "test07.c", directory: "/root/assign2/test")
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
!58 = !DILocalVariable(name: "af_ptr", scope: !42, file: !1, line: 18, type: !59)
!59 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !29, size: 64)
!60 = !DILocation(line: 18, column: 11, scope: !42)
!61 = !DILocalVariable(name: "result", scope: !42, file: !1, line: 19, type: !62)
!62 = !DIBasicType(name: "unsigned int", size: 32, encoding: DW_ATE_unsigned)
!63 = !DILocation(line: 19, column: 14, scope: !42)
!64 = !DILocation(line: 20, column: 9, scope: !65)
!65 = distinct !DILexicalBlock(scope: !42, file: !1, line: 20, column: 9)
!66 = !DILocation(line: 20, column: 11, scope: !65)
!67 = !DILocation(line: 20, column: 9, scope: !42)
!68 = !DILocation(line: 21, column: 17, scope: !69)
!69 = distinct !DILexicalBlock(scope: !65, file: !1, line: 20, column: 19)
!70 = !DILocation(line: 21, column: 15, scope: !69)
!71 = !DILocation(line: 22, column: 5, scope: !69)
!72 = !DILocation(line: 24, column: 17, scope: !73)
!73 = distinct !DILexicalBlock(scope: !65, file: !1, line: 23, column: 5)
!74 = !DILocation(line: 24, column: 15, scope: !73)
!75 = !DILocation(line: 27, column: 8, scope: !76)
!76 = distinct !DILexicalBlock(scope: !42, file: !1, line: 27, column: 8)
!77 = !DILocation(line: 27, column: 12, scope: !76)
!78 = !DILocation(line: 27, column: 8, scope: !42)
!79 = !DILocation(line: 28, column: 16, scope: !76)
!80 = !DILocation(line: 28, column: 23, scope: !76)
!81 = !DILocation(line: 28, column: 28, scope: !76)
!82 = !DILocation(line: 28, column: 33, scope: !76)
!83 = !DILocation(line: 28, column: 14, scope: !76)
!84 = !DILocation(line: 28, column: 8, scope: !76)
!85 = !DILocation(line: 30, column: 16, scope: !76)
!86 = !DILocation(line: 30, column: 23, scope: !76)
!87 = !DILocation(line: 30, column: 28, scope: !76)
!88 = !DILocation(line: 30, column: 33, scope: !76)
!89 = !DILocation(line: 30, column: 14, scope: !76)
!90 = !DILocation(line: 31, column: 12, scope: !42)
!91 = !DILocation(line: 31, column: 5, scope: !42)
