; RUN: sed 's/DIVREM/sdiv/g' %s | not llubi --verbose --vscale=2 2>&1 | FileCheck %s
; RUN: sed 's/DIVREM/udiv/g' %s | not llubi --verbose --vscale=2 2>&1 | FileCheck %s
; RUN: sed 's/DIVREM/srem/g' %s | not llubi --verbose --vscale=2 2>&1 | FileCheck %s
; RUN: sed 's/DIVREM/urem/g' %s | not llubi --verbose --vscale=2 2>&1 | FileCheck %s

define void @main() {
  %res = call <vscale x 2 x i32> @llvm.vp.DIVREM.nxv2i32(<vscale x 2 x i32> splat (i32 10), <vscale x 2 x i32> splat (i32 3), <vscale x 2 x i1> splat (i1 true), i32 5)
  ret void
}
; CHECK: Entering function: main
; CHECK-NEXT: Stacktrace:
; CHECK-NEXT: #0 %res = call <vscale x 2 x i32> @llvm.vp.{{sdiv|udiv|srem|urem}}.nxv2i32(<vscale x 2 x i32> splat (i32 10), <vscale x 2 x i32> splat (i32 3), <vscale x 2 x i1> splat (i1 true), i32 5) at @main <stdin>:7
; CHECK-NEXT: Immediate UB detected: VP intrinsic with explicit vector length 5 greater than the number of elements 4.
; CHECK-NEXT: error: Execution of function 'main' failed.
