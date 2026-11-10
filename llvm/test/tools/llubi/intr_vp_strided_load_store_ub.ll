; RUN: not llubi --verbose --entry-function=evl_too_large < %s 2>&1 | FileCheck %s --check-prefix=EVL-TOO-LARGE
; RUN: not llubi --verbose --entry-function=poison_evl < %s 2>&1 | FileCheck %s --check-prefix=POISON-EVL
; RUN: not llubi --verbose --entry-function=poison_mask < %s 2>&1 | FileCheck %s --check-prefix=POISON-MASK
; RUN: not llubi --verbose --entry-function=poison_pointer < %s 2>&1 | FileCheck %s --check-prefix=POISON-PTR
; RUN: not llubi --verbose --entry-function=poison_stride < %s 2>&1 | FileCheck %s --check-prefix=POISON-STRIDE
; RUN: not llubi --verbose --entry-function=load_out_of_bounds < %s 2>&1 | FileCheck %s --check-prefix=LOAD-OOB
; RUN: not llubi --verbose --entry-function=store_out_of_bounds < %s 2>&1 | FileCheck %s --check-prefix=STORE-OOB
; RUN: not llubi --verbose --entry-function=misaligned < %s 2>&1 | FileCheck %s --check-prefix=MISALIGNED
; RUN: not llubi --verbose --entry-function=store_constant < %s 2>&1 | FileCheck %s --check-prefix=STORE-CONST

@a = global [4 x i32] [i32 0, i32 1, i32 2, i32 3]
@const = constant [4 x i32] [i32 0, i32 1, i32 2, i32 3]

define void @evl_too_large() {
; EVL-TOO-LARGE: Entering function: evl_too_large
; EVL-TOO-LARGE-NEXT: Stacktrace:
; EVL-TOO-LARGE-NEXT: #0   %r = call <4 x i32> @llvm.experimental.vp.strided.load.v4i32.p0.i64(ptr @a, i64 4, <4 x i1> splat (i1 true), i32 5) at @evl_too_large <stdin>:{{[0-9]+}}
; EVL-TOO-LARGE-NEXT: Immediate UB detected: llvm.experimental.vp.strided.load explicit vector length 5 exceeds the number of vector elements 4.
; EVL-TOO-LARGE-NEXT: error: Execution of function 'evl_too_large' failed.
  %r = call <4 x i32> @llvm.experimental.vp.strided.load.v4i32.p0.i64(ptr @a, i64 4, <4 x i1> splat (i1 true), i32 5)
  ret void
}

define void @poison_evl() {
; POISON-EVL: Entering function: poison_evl
; POISON-EVL-NEXT: Stacktrace:
; POISON-EVL-NEXT: #0   call void @llvm.experimental.vp.strided.store.v4i32.p0.i64(<4 x i32> zeroinitializer, ptr @a, i64 4, <4 x i1> splat (i1 true), i32 poison) at @poison_evl <stdin>:{{[0-9]+}}
; POISON-EVL-NEXT: Immediate UB detected: llvm.experimental.vp.strided.store with poison explicit vector length.
; POISON-EVL-NEXT: error: Execution of function 'poison_evl' failed.
  call void @llvm.experimental.vp.strided.store.v4i32.p0.i64(<4 x i32> zeroinitializer, ptr @a, i64 4, <4 x i1> splat (i1 true), i32 poison)
  ret void
}

define void @poison_mask() {
; POISON-MASK: Entering function: poison_mask
; POISON-MASK-NEXT: Stacktrace:
; POISON-MASK-NEXT: #0   %r = call <4 x i32> @llvm.experimental.vp.strided.load.v4i32.p0.i64(ptr @a, i64 4, <4 x i1> <i1 true, i1 poison, i1 true, i1 true>, i32 2) at @poison_mask <stdin>:{{[0-9]+}}
; POISON-MASK-NEXT: Immediate UB detected: Poison mask lane in llvm.experimental.vp.strided.load.
; POISON-MASK-NEXT: error: Execution of function 'poison_mask' failed.
  %r = call <4 x i32> @llvm.experimental.vp.strided.load.v4i32.p0.i64(ptr @a, i64 4, <4 x i1> <i1 true, i1 poison, i1 true, i1 true>, i32 2)
  ret void
}

define void @poison_pointer() {
; POISON-PTR: Entering function: poison_pointer
; POISON-PTR-NEXT: Stacktrace:
; POISON-PTR-NEXT: #0   %r = call <4 x i32> @llvm.experimental.vp.strided.load.v4i32.p0.i64(ptr poison, i64 4, <4 x i1> <i1 false, i1 false, i1 true, i1 false>, i32 4) at @poison_pointer <stdin>:{{[0-9]+}}
; POISON-PTR-NEXT: Immediate UB detected: llvm.experimental.vp.strided.load with poison pointer.
; POISON-PTR-NEXT: error: Execution of function 'poison_pointer' failed.
  %r = call <4 x i32> @llvm.experimental.vp.strided.load.v4i32.p0.i64(ptr poison, i64 4, <4 x i1> <i1 false, i1 false, i1 true, i1 false>, i32 4)
  ret void
}

define void @poison_stride() {
; POISON-STRIDE: Entering function: poison_stride
; POISON-STRIDE-NEXT: Stacktrace:
; POISON-STRIDE-NEXT: #0   call void @llvm.experimental.vp.strided.store.v4i32.p0.i64(<4 x i32> zeroinitializer, ptr @a, i64 poison, <4 x i1> <i1 true, i1 true, i1 false, i1 false>, i32 4) at @poison_stride <stdin>:{{[0-9]+}}
; POISON-STRIDE-NEXT: Immediate UB detected: llvm.experimental.vp.strided.store with poison stride.
; POISON-STRIDE-NEXT: error: Execution of function 'poison_stride' failed.
  call void @llvm.experimental.vp.strided.store.v4i32.p0.i64(<4 x i32> zeroinitializer, ptr @a, i64 poison, <4 x i1> <i1 true, i1 true, i1 false, i1 false>, i32 4)
  ret void
}

define void @load_out_of_bounds() {
; LOAD-OOB: Entering function: load_out_of_bounds
; LOAD-OOB-NEXT: Stacktrace:
; LOAD-OOB-NEXT: #0   %r = call <4 x i32> @llvm.experimental.vp.strided.load.v4i32.p0.i64(ptr @a, i64 8, <4 x i1> splat (i1 true), i32 3) at @load_out_of_bounds <stdin>:{{[0-9]+}}
; LOAD-OOB-NEXT: Immediate UB detected: Memory access is out of bounds. Accessed size: 4, Address: 0x18, Object base: 0x8, Object size: 16.
; LOAD-OOB-NEXT: error: Execution of function 'load_out_of_bounds' failed.
  %r = call <4 x i32> @llvm.experimental.vp.strided.load.v4i32.p0.i64(ptr @a, i64 8, <4 x i1> splat (i1 true), i32 3)
  ret void
}

define void @store_out_of_bounds() {
; STORE-OOB: Entering function: store_out_of_bounds
; STORE-OOB-NEXT: Stacktrace:
; STORE-OOB-NEXT: #0   call void @llvm.experimental.vp.strided.store.v4i32.p0.i64(<4 x i32> zeroinitializer, ptr @a, i64 -4, <4 x i1> <i1 true, i1 false, i1 true, i1 false>, i32 3) at @store_out_of_bounds <stdin>:{{[0-9]+}}
; STORE-OOB-NEXT: Immediate UB detected: Memory access is out of bounds. Accessed size: 4, Address: 0x0, Object base: 0x8, Object size: 16.
; STORE-OOB-NEXT: error: Execution of function 'store_out_of_bounds' failed.
  call void @llvm.experimental.vp.strided.store.v4i32.p0.i64(<4 x i32> zeroinitializer, ptr @a, i64 -4, <4 x i1> <i1 true, i1 false, i1 true, i1 false>, i32 3)
  ret void
}

define void @misaligned() {
; MISALIGNED: Entering function: misaligned
; MISALIGNED-NEXT: Stacktrace:
; MISALIGNED-NEXT: #0   %r = call <2 x i32> @llvm.experimental.vp.strided.load.v2i32.p0.i64(ptr @a, i64 2, <2 x i1> splat (i1 true), i32 2) at @misaligned <stdin>:{{[0-9]+}}
; MISALIGNED-NEXT: Immediate UB detected: Misaligned memory access. Address: 0xA, Required alignment: 4.
; MISALIGNED-NEXT: error: Execution of function 'misaligned' failed.
  %r = call <2 x i32> @llvm.experimental.vp.strided.load.v2i32.p0.i64(ptr @a, i64 2, <2 x i1> splat (i1 true), i32 2)
  ret void
}

define void @store_constant() {
; STORE-CONST: Entering function: store_constant
; STORE-CONST-NEXT: Stacktrace:
; STORE-CONST-NEXT: #0   call void @llvm.experimental.vp.strided.store.v4i32.p0.i64(<4 x i32> zeroinitializer, ptr @const, i64 4, <4 x i1> splat (i1 true), i32 4) at @store_constant <stdin>:{{[0-9]+}}
; STORE-CONST-NEXT: Immediate UB detected: Try to write to a constant memory object: ptr 0x1C [@const].
; STORE-CONST-NEXT: error: Execution of function 'store_constant' failed.
  call void @llvm.experimental.vp.strided.store.v4i32.p0.i64(<4 x i32> zeroinitializer, ptr @const, i64 4, <4 x i1> splat (i1 true), i32 4)
  ret void
}
