; ModuleID = 'test00.bc'
source_filename = "test00.c"
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
define dso_local i32 @clever() #0 !dbg !19 {
entry:
  %a_fptr = alloca i32 (i32, i32)*, align 8
  %op1 = alloca i32, align 4
  %op2 = alloca i32, align 4
  %result = alloca i32, align 4
  call void @llvm.dbg.declare(metadata i32 (i32, i32)** %a_fptr, metadata !22, metadata !DIExpression()), !dbg !24
  store i32 (i32, i32)* @plus, i32 (i32, i32)** %a_fptr, align 8, !dbg !24
  call void @llvm.dbg.declare(metadata i32* %op1, metadata !25, metadata !DIExpression()), !dbg !26
  store i32 1, i32* %op1, align 4, !dbg !26
  call void @llvm.dbg.declare(metadata i32* %op2, metadata !27, metadata !DIExpression()), !dbg !28
  store i32 2, i32* %op2, align 4, !dbg !28
  call void @llvm.dbg.declare(metadata i32* %result, metadata !29, metadata !DIExpression()), !dbg !31
  %0 = load i32 (i32, i32)*, i32 (i32, i32)** %a_fptr, align 8, !dbg !32
  %1 = load i32, i32* %op1, align 4, !dbg !33
  %2 = load i32, i32* %op2, align 4, !dbg !34
  %call = call i32 %0(i32 %1, i32 %2), !dbg !32
  store i32 %call, i32* %result, align 4, !dbg !31
  ret i32 0, !dbg !35
}

attributes #0 = { noinline nounwind optnone uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind readnone speculatable willreturn }

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!3, !4, !5}
!llvm.ident = !{!6}

!0 = distinct !DICompileUnit(language: DW_LANG_C99, file: !1, producer: "clang version 10.0.0 ", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, enums: !2, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "test00.c", directory: "/root/assign2/test")
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
!19 = distinct !DISubprogram(name: "clever", scope: !1, file: !1, line: 5, type: !20, scopeLine: 5, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !2)
!20 = !DISubroutineType(types: !21)
!21 = !{!10}
!22 = !DILocalVariable(name: "a_fptr", scope: !19, file: !1, line: 6, type: !23)
!23 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !8, size: 64)
!24 = !DILocation(line: 6, column: 11, scope: !19)
!25 = !DILocalVariable(name: "op1", scope: !19, file: !1, line: 8, type: !10)
!26 = !DILocation(line: 8, column: 9, scope: !19)
!27 = !DILocalVariable(name: "op2", scope: !19, file: !1, line: 8, type: !10)
!28 = !DILocation(line: 8, column: 18, scope: !19)
!29 = !DILocalVariable(name: "result", scope: !19, file: !1, line: 10, type: !30)
!30 = !DIBasicType(name: "unsigned int", size: 32, encoding: DW_ATE_unsigned)
!31 = !DILocation(line: 10, column: 14, scope: !19)
!32 = !DILocation(line: 10, column: 23, scope: !19)
!33 = !DILocation(line: 10, column: 30, scope: !19)
!34 = !DILocation(line: 10, column: 35, scope: !19)
!35 = !DILocation(line: 11, column: 5, scope: !19)
