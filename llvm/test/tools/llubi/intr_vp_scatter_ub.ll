; RUN: not llubi --verbose --entry-function=out_of_bounds < %s 2>&1 | FileCheck %s --check-prefix=OOB
; RUN: not llubi --verbose --entry-function=constant_mem < %s 2>&1 | FileCheck %s --check-prefix=CONSTANT-MEM
; RUN: not llubi --verbose --entry-function=poison_mask_lane < %s 2>&1 | FileCheck %s --check-prefix=POISON-MASK-LANE
; RUN: not llubi --verbose --entry-function=poison_pointer < %s 2>&1 | FileCheck %s --check-prefix=POISON-PTR
; RUN: not llubi --verbose --entry-function=poison_evl < %s 2>&1 | FileCheck %s --check-prefix=POISON-EVL
; RUN: not llubi --verbose --entry-function=evl_too_large < %s 2>&1 | FileCheck %s --check-prefix=EVL-TOO-LARGE
; RUN: not llubi --verbose --vscale=2 --entry-function=evl_too_large_scalable < %s 2>&1 | FileCheck %s --check-prefix=EVL-TOO-LARGE-SCALABLE
; RUN: not llubi --verbose --entry-function=misaligned < %s 2>&1 | FileCheck %s --check-prefix=MISALIGNED
; RUN: not llubi --verbose --entry-function=misaligned_abi < %s 2>&1 | FileCheck %s --check-prefix=MISALIGNED-ABI

@a = global [4 x i32] [i32 0, i32 1, i32 2, i32 3]
@c = constant [4 x i32] [i32 0, i32 1, i32 2, i32 3]

define void @out_of_bounds() {
  %ptrs = getelementptr i32, ptr @a, <4 x i64> <i64 0, i64 1, i64 4, i64 5>
  call void @llvm.vp.scatter.v4i32.v4p0(<4 x i32> zeroinitializer, <4 x ptr> align 4 %ptrs, <4 x i1> <i1 true, i1 false, i1 true, i1 true>, i32 3)
  ret void
}

define void @constant_mem() {
  call void @llvm.vp.scatter.v4i32.v4p0(<4 x i32> zeroinitializer, <4 x ptr> align 4 <ptr @a, ptr @c, ptr @a, ptr @a>, <4 x i1> <i1 false, i1 true, i1 false, i1 false>, i32 4)
  ret void
}

define void @poison_mask_lane() {
  call void @llvm.vp.scatter.v4i32.v4p0(<4 x i32> zeroinitializer, <4 x ptr> align 4 <ptr @a, ptr @a, ptr @a, ptr @a>, <4 x i1> <i1 true, i1 poison, i1 false, i1 false>, i32 2)
  ret void
}

define void @poison_pointer() {
  call void @llvm.vp.scatter.v4i32.v4p0(<4 x i32> zeroinitializer, <4 x ptr> align 4 <ptr @a, ptr poison, ptr @a, ptr @a>, <4 x i1> <i1 false, i1 true, i1 false, i1 false>, i32 4)
  ret void
}

define void @poison_evl() {
  call void @llvm.vp.scatter.v4i32.v4p0(<4 x i32> zeroinitializer, <4 x ptr> align 4 <ptr @a, ptr @a, ptr @a, ptr @a>, <4 x i1> splat (i1 true), i32 poison)
  ret void
}

define void @evl_too_large() {
  call void @llvm.vp.scatter.v4i32.v4p0(<4 x i32> zeroinitializer, <4 x ptr> align 4 <ptr @a, ptr @a, ptr @a, ptr @a>, <4 x i1> splat (i1 true), i32 5)
  ret void
}

define void @evl_too_large_scalable() {
  call void @llvm.vp.scatter.nxv2i32.nxv2p0(<vscale x 2 x i32> zeroinitializer, <vscale x 2 x ptr> align 4 splat (ptr @a), <vscale x 2 x i1> splat (i1 true), i32 5)
  ret void
}

; The align attribute turns the misaligned pointer into poison.
define void @misaligned() {
  %ptrs = getelementptr i8, ptr @a, <2 x i64> <i64 0, i64 2>
  call void @llvm.vp.scatter.v2i32.v2p0(<2 x i32> zeroinitializer, <2 x ptr> align 4 %ptrs, <2 x i1> <i1 false, i1 true>, i32 2)
  ret void
}

; Without the align attribute, the ABI alignment of the element type is used.
define void @misaligned_abi() {
  %ptrs = getelementptr i8, ptr @a, <2 x i64> <i64 0, i64 2>
  call void @llvm.vp.scatter.v2i32.v2p0(<2 x i32> zeroinitializer, <2 x ptr> %ptrs, <2 x i1> <i1 false, i1 true>, i32 2)
  ret void
}
