; RUN: sed 's/DIVREM/sdiv/g' %s | not llubi --verbose 2>&1 | FileCheck %s
; RUN: sed 's/DIVREM/udiv/g' %s | not llubi --verbose 2>&1 | FileCheck %s
; RUN: sed 's/DIVREM/srem/g' %s | not llubi --verbose 2>&1 | FileCheck %s
; RUN: sed 's/DIVREM/urem/g' %s | not llubi --verbose 2>&1 | FileCheck %s

define void @main() {
  %res = call <2 x i32> @llvm.vp.DIVREM.v2i32(<2 x i32> splat (i32 10), <2 x i32> splat (i32 3), <2 x i1> splat (i1 true), i32 poison)
  ret void
}
; CHECK: Entering function: main
; CHECK-NEXT: Stacktrace:
; CHECK-NEXT: #0 %res = call <2 x i32> @llvm.vp.{{sdiv|udiv|srem|urem}}.v2i32(<2 x i32> splat (i32 10), <2 x i32> splat (i32 3), <2 x i1> splat (i1 true), i32 poison) at @main <stdin>:7
; CHECK-NEXT: Immediate UB detected: VP intrinsic with poison explicit vector length.
; CHECK-NEXT: error: Execution of function 'main' failed.
