; RUN: not llubi --verbose --entry-function=out_of_bounds < %s 2>&1 | FileCheck %s --check-prefix=OOB
; RUN: not llubi --verbose --entry-function=constant_mem < %s 2>&1 | FileCheck %s --check-prefix=CONSTANT-MEM
; RUN: not llubi --verbose --entry-function=evl_too_large < %s 2>&1 | FileCheck %s --check-prefix=EVL-TOO-LARGE
; RUN: not llubi --verbose --vscale=2 --entry-function=evl_too_large_scalable < %s 2>&1 | FileCheck %s --check-prefix=EVL-TOO-LARGE-SCALABLE
; RUN: not llubi --verbose --entry-function=poison_evl < %s 2>&1 | FileCheck %s --check-prefix=POISON-EVL
; RUN: not llubi --verbose --entry-function=poison_mask_lane < %s 2>&1 | FileCheck %s --check-prefix=POISON-MASK-LANE
; RUN: not llubi --verbose --entry-function=poison_pointer < %s 2>&1 | FileCheck %s --check-prefix=POISON-PTR
; RUN: not llubi --verbose --entry-function=misaligned < %s 2>&1 | FileCheck %s --check-prefix=MISALIGNED
; RUN: not llubi --verbose --entry-function=misaligned_default < %s 2>&1 | FileCheck %s --check-prefix=MISALIGNED-DEFAULT
; RUN: not llubi --verbose --entry-function=unsupported_elt_ty < %s 2>&1 | FileCheck %s --check-prefix=ELT-TY

@a = global [4 x i32] [i32 0, i32 1, i32 2, i32 3]
@d = global [8 x i32] zeroinitializer, align 16
@c = constant [4 x i32] [i32 0, i32 1, i32 2, i32 3]

define void @out_of_bounds() {
; OOB: Entering function: out_of_bounds
; OOB-NEXT:   %tail = getelementptr i32, ptr @a, i64 2 => ptr 0x10 [@a + 8]
; OOB-NEXT: Stacktrace:
; OOB-NEXT: #0   call void @llvm.vp.store.v4i32.p0(<4 x i32> zeroinitializer, ptr align 4 %tail, <4 x i1> splat (i1 true), i32 3) at @out_of_bounds <stdin>:{{[0-9]+}}
; OOB-NEXT: Immediate UB detected: Memory access is out of bounds. Accessed size: 4, Address: 0x18, Object base: 0x8, Object size: 16.
; OOB-NEXT: error: Execution of function 'out_of_bounds' failed.
  %tail = getelementptr i32, ptr @a, i64 2
  call void @llvm.vp.store.v4i32.p0(<4 x i32> zeroinitializer, ptr align 4 %tail, <4 x i1> splat (i1 true), i32 3)
  ret void
}

define void @constant_mem() {
; CONSTANT-MEM: Entering function: constant_mem
; CONSTANT-MEM-NEXT: Stacktrace:
; CONSTANT-MEM-NEXT: #0   call void @llvm.vp.store.v4i32.p0(<4 x i32> zeroinitializer, ptr align 4 @c, <4 x i1> splat (i1 true), i32 1) at @constant_mem <stdin>:{{[0-9]+}}
; CONSTANT-MEM-NEXT: Immediate UB detected: Try to write to a constant memory object: ptr 0x44 [@c].
; CONSTANT-MEM-NEXT: error: Execution of function 'constant_mem' failed.
  call void @llvm.vp.store.v4i32.p0(<4 x i32> zeroinitializer, ptr align 4 @c, <4 x i1> splat (i1 true), i32 1)
  ret void
}

define void @evl_too_large() {
; EVL-TOO-LARGE: Entering function: evl_too_large
; EVL-TOO-LARGE-NEXT: Stacktrace:
; EVL-TOO-LARGE-NEXT: #0   call void @llvm.vp.store.v4i32.p0(<4 x i32> zeroinitializer, ptr align 4 @a, <4 x i1> splat (i1 true), i32 5) at @evl_too_large <stdin>:{{[0-9]+}}
; EVL-TOO-LARGE-NEXT: Immediate UB detected: llvm.vp.store with explicit vector length 5 greater than the number of vector elements 4.
; EVL-TOO-LARGE-NEXT: error: Execution of function 'evl_too_large' failed.
  call void @llvm.vp.store.v4i32.p0(<4 x i32> zeroinitializer, ptr align 4 @a, <4 x i1> splat (i1 true), i32 5)
  ret void
}

define void @evl_too_large_scalable() {
; EVL-TOO-LARGE-SCALABLE: Entering function: evl_too_large_scalable
; EVL-TOO-LARGE-SCALABLE-NEXT: Stacktrace:
; EVL-TOO-LARGE-SCALABLE-NEXT: #0   call void @llvm.vp.store.nxv2i32.p0(<vscale x 2 x i32> zeroinitializer, ptr align 4 @a, <vscale x 2 x i1> splat (i1 true), i32 5) at @evl_too_large_scalable <stdin>:{{[0-9]+}}
; EVL-TOO-LARGE-SCALABLE-NEXT: Immediate UB detected: llvm.vp.store with explicit vector length 5 greater than the number of vector elements 4.
; EVL-TOO-LARGE-SCALABLE-NEXT: error: Execution of function 'evl_too_large_scalable' failed.
  call void @llvm.vp.store.nxv2i32.p0(<vscale x 2 x i32> zeroinitializer, ptr align 4 @a, <vscale x 2 x i1> splat (i1 true), i32 5)
  ret void
}

define void @poison_evl() {
; POISON-EVL: Entering function: poison_evl
; POISON-EVL-NEXT: Stacktrace:
; POISON-EVL-NEXT: #0   call void @llvm.vp.store.v4i32.p0(<4 x i32> zeroinitializer, ptr align 4 @a, <4 x i1> splat (i1 true), i32 poison) at @poison_evl <stdin>:{{[0-9]+}}
; POISON-EVL-NEXT: Immediate UB detected: llvm.vp.store with poison explicit vector length.
; POISON-EVL-NEXT: error: Execution of function 'poison_evl' failed.
  call void @llvm.vp.store.v4i32.p0(<4 x i32> zeroinitializer, ptr align 4 @a, <4 x i1> splat (i1 true), i32 poison)
  ret void
}

define void @poison_mask_lane() {
; POISON-MASK-LANE: Entering function: poison_mask_lane
; POISON-MASK-LANE-NEXT: Stacktrace:
; POISON-MASK-LANE-NEXT: #0   call void @llvm.vp.store.v4i32.p0(<4 x i32> zeroinitializer, ptr align 4 @a, <4 x i1> <i1 true, i1 poison, i1 false, i1 false>, i32 2) at @poison_mask_lane <stdin>:{{[0-9]+}}
; POISON-MASK-LANE-NEXT: Immediate UB detected: Poison mask lane in llvm.vp.store.
; POISON-MASK-LANE-NEXT: error: Execution of function 'poison_mask_lane' failed.
  call void @llvm.vp.store.v4i32.p0(<4 x i32> zeroinitializer, ptr align 4 @a, <4 x i1> <i1 true, i1 poison, i1 false, i1 false>, i32 2)
  ret void
}

define void @poison_pointer() {
; POISON-PTR: Entering function: poison_pointer
; POISON-PTR-NEXT: Stacktrace:
; POISON-PTR-NEXT: #0   call void @llvm.vp.store.v4i32.p0(<4 x i32> zeroinitializer, ptr align 4 poison, <4 x i1> splat (i1 true), i32 1) at @poison_pointer <stdin>:{{[0-9]+}}
; POISON-PTR-NEXT: Immediate UB detected: Invalid memory access with a poison pointer.
; POISON-PTR-NEXT: error: Execution of function 'poison_pointer' failed.
  call void @llvm.vp.store.v4i32.p0(<4 x i32> zeroinitializer, ptr align 4 poison, <4 x i1> splat (i1 true), i32 1)
  ret void
}

; The align attribute turns the misaligned pointer into poison.
define void @misaligned() {
; MISALIGNED: Entering function: misaligned
; MISALIGNED-NEXT:   %p = getelementptr i32, ptr @a, i64 1 => ptr 0xC [@a + 4]
; MISALIGNED-NEXT: Stacktrace:
; MISALIGNED-NEXT: #0   call void @llvm.vp.store.v2i32.p0(<2 x i32> zeroinitializer, ptr align 8 %p, <2 x i1> splat (i1 true), i32 1) at @misaligned <stdin>:{{[0-9]+}}
; MISALIGNED-NEXT: Immediate UB detected: Invalid memory access with a poison pointer.
; MISALIGNED-NEXT: error: Execution of function 'misaligned' failed.
  %p = getelementptr i32, ptr @a, i64 1
  call void @llvm.vp.store.v2i32.p0(<2 x i32> zeroinitializer, ptr align 8 %p, <2 x i1> splat (i1 true), i32 1)
  ret void
}

; Without an align attribute, the ABI alignment of <4 x i32> is required.
define void @misaligned_default() {
; MISALIGNED-DEFAULT: Entering function: misaligned_default
; MISALIGNED-DEFAULT-NEXT:   %p = getelementptr i32, ptr @d, i64 1 => ptr 0x24 [@d + 4]
; MISALIGNED-DEFAULT-NEXT: Stacktrace:
; MISALIGNED-DEFAULT-NEXT: #0   call void @llvm.vp.store.v4i32.p0(<4 x i32> zeroinitializer, ptr %p, <4 x i1> splat (i1 true), i32 1) at @misaligned_default <stdin>:{{[0-9]+}}
; MISALIGNED-DEFAULT-NEXT: Immediate UB detected: Invalid memory access with a poison pointer.
; MISALIGNED-DEFAULT-NEXT: error: Execution of function 'misaligned_default' failed.
  %p = getelementptr i32, ptr @d, i64 1
  call void @llvm.vp.store.v4i32.p0(<4 x i32> zeroinitializer, ptr %p, <4 x i1> splat (i1 true), i32 1)
  ret void
}

define void @unsupported_elt_ty() {
; ELT-TY: Entering function: unsupported_elt_ty
; ELT-TY-NEXT: Stacktrace:
; ELT-TY-NEXT: #0   call void @llvm.vp.store.v8i1.p0(<8 x i1> zeroinitializer, ptr align 1 @a, <8 x i1> splat (i1 true), i32 8) at @unsupported_elt_ty <stdin>:{{[0-9]+}}
; ELT-TY-NEXT: Error: llvm.vp.store with element type i1 is not supported.
; ELT-TY-NEXT: error: Execution of function 'unsupported_elt_ty' failed.
  call void @llvm.vp.store.v8i1.p0(<8 x i1> zeroinitializer, ptr align 1 @a, <8 x i1> splat (i1 true), i32 8)
  ret void
}
