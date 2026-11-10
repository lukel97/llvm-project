; RUN: sed 's/DIVREM/sdiv/g' %s | not llubi --verbose 2>&1 | FileCheck %s
; RUN: sed 's/DIVREM/srem/g' %s | not llubi --verbose 2>&1 | FileCheck %s

define void @main() {
  %res = call <2 x i8> @llvm.vp.DIVREM.v2i8(<2 x i8> <i8 1, i8 -128>, <2 x i8> splat (i8 -1), <2 x i1> splat (i1 true), i32 2)
  ret void
}
; CHECK: Entering function: main
; CHECK-NEXT: Stacktrace:
; CHECK-NEXT: #0 %res = call <2 x i8> @llvm.vp.{{sdiv|srem}}.v2i8(<2 x i8> <i8 1, i8 -128>, <2 x i8> splat (i8 -1), <2 x i1> splat (i1 true), i32 2) at @main <stdin>:5
; CHECK-NEXT: Immediate UB detected: Signed division overflow.
; CHECK-NEXT: error: Execution of function 'main' failed.
