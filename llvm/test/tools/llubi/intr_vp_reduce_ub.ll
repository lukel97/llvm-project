; RUN: sed 's/OP/evl_too_large/g' %s | not llubi --verbose --vscale=2 2>&1 | FileCheck %s --check-prefix=LARGE
; RUN: sed 's/OP/evl_too_large_scalable/g' %s | not llubi --verbose --vscale=2 2>&1 | FileCheck %s --check-prefix=LARGE-SCALABLE
; RUN: sed 's/OP/evl_poison/g' %s | not llubi --verbose --vscale=2 2>&1 | FileCheck %s --check-prefix=POISON

define void @main() {
  call void @OP()
  ret void
}

define void @evl_too_large() {
  %r = call i32 @llvm.vp.reduce.add.v4i32(i32 0, <4 x i32> zeroinitializer, <4 x i1> splat (i1 true), i32 5)
  ret void
}

define void @evl_too_large_scalable() {
  %r = call i32 @llvm.vp.reduce.add.nxv2i32(i32 0, <vscale x 2 x i32> zeroinitializer, <vscale x 2 x i1> splat (i1 true), i32 5)
  ret void
}

define void @evl_poison() {
  %r = call i32 @llvm.vp.reduce.add.v4i32(i32 0, <4 x i32> zeroinitializer, <4 x i1> splat (i1 true), i32 poison)
  ret void
}

; LARGE: Entering function: evl_too_large
; LARGE: Immediate UB detected: VP intrinsic with explicit vector length 5 greater than the number of vector elements 4.
; LARGE: error: Execution of function 'main' failed.

; LARGE-SCALABLE: Entering function: evl_too_large_scalable
; LARGE-SCALABLE: Immediate UB detected: VP intrinsic with explicit vector length 5 greater than the number of vector elements 4.
; LARGE-SCALABLE: error: Execution of function 'main' failed.

; POISON: Entering function: evl_poison
; POISON: Immediate UB detected: VP intrinsic with poison explicit vector length.
; POISON: error: Execution of function 'main' failed.
