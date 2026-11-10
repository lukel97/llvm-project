; RUN: not llubi --verbose --entry-function=out_of_bounds < %s 2>&1 | FileCheck %s --check-prefix=OOB
; RUN: not llubi --verbose --entry-function=poison_mask_lane < %s 2>&1 | FileCheck %s --check-prefix=POISON-MASK-LANE
; RUN: not llubi --verbose --entry-function=poison_pointer < %s 2>&1 | FileCheck %s --check-prefix=POISON-PTR
; RUN: not llubi --verbose --entry-function=poison_evl < %s 2>&1 | FileCheck %s --check-prefix=POISON-EVL
; RUN: not llubi --verbose --entry-function=evl_too_large < %s 2>&1 | FileCheck %s --check-prefix=EVL-TOO-LARGE
; RUN: not llubi --verbose --vscale=2 --entry-function=evl_too_large_scalable < %s 2>&1 | FileCheck %s --check-prefix=EVL-TOO-LARGE-SCALABLE
; RUN: not llubi --verbose --entry-function=misaligned < %s 2>&1 | FileCheck %s --check-prefix=MISALIGNED
; RUN: not llubi --verbose --entry-function=misaligned_abi < %s 2>&1 | FileCheck %s --check-prefix=MISALIGNED-ABI

@a = global [4 x i32] [i32 0, i32 1, i32 2, i32 3]

define void @out_of_bounds() {
; OOB: Entering function: out_of_bounds
; OOB-NEXT:   %ptrs = getelementptr i32, ptr @a, <4 x i64> <i64 0, i64 1, i64 4, i64 5> => { ptr 0x8 [@a], ptr 0xC [@a + 4], ptr 0x18 [@a + 16], ptr 0x1C [@a + 20] }
; OOB-NEXT: Stacktrace:
; OOB-NEXT: #0   %r = call <4 x i32> @llvm.vp.gather.v4i32.v4p0(<4 x ptr> align 4 %ptrs, <4 x i1> <i1 true, i1 false, i1 true, i1 true>, i32 3) at @out_of_bounds <stdin>:{{[0-9]+}}
; OOB-NEXT: Immediate UB detected: Memory access is out of bounds. Accessed size: 4, Address: 0x18, Object base: 0x8, Object size: 16.
; OOB-NEXT: error: Execution of function 'out_of_bounds' failed.
  %ptrs = getelementptr i32, ptr @a, <4 x i64> <i64 0, i64 1, i64 4, i64 5>
  %r = call <4 x i32> @llvm.vp.gather.v4i32.v4p0(<4 x ptr> align 4 %ptrs, <4 x i1> <i1 true, i1 false, i1 true, i1 true>, i32 3)
  ret void
}

define void @poison_mask_lane() {
; POISON-MASK-LANE: Entering function: poison_mask_lane
; POISON-MASK-LANE-NEXT: Stacktrace:
; POISON-MASK-LANE-NEXT: #0   %r = call <4 x i32> @llvm.vp.gather.v4i32.v4p0(<4 x ptr> align 4 <ptr @a, ptr @a, ptr @a, ptr @a>, <4 x i1> <i1 true, i1 poison, i1 false, i1 false>, i32 2) at @poison_mask_lane <stdin>:{{[0-9]+}}
; POISON-MASK-LANE-NEXT: Immediate UB detected: Poison mask lane in llvm.vp.gather.
; POISON-MASK-LANE-NEXT: error: Execution of function 'poison_mask_lane' failed.
  %r = call <4 x i32> @llvm.vp.gather.v4i32.v4p0(<4 x ptr> align 4 <ptr @a, ptr @a, ptr @a, ptr @a>, <4 x i1> <i1 true, i1 poison, i1 false, i1 false>, i32 2)
  ret void
}

define void @poison_pointer() {
; POISON-PTR: Entering function: poison_pointer
; POISON-PTR-NEXT: Stacktrace:
; POISON-PTR-NEXT: #0   %r = call <4 x i32> @llvm.vp.gather.v4i32.v4p0(<4 x ptr> align 4 <ptr @a, ptr poison, ptr @a, ptr @a>, <4 x i1> <i1 false, i1 true, i1 false, i1 false>, i32 4) at @poison_pointer <stdin>:{{[0-9]+}}
; POISON-PTR-NEXT: Immediate UB detected: Invalid memory access with a poison pointer.
; POISON-PTR-NEXT: error: Execution of function 'poison_pointer' failed.
  %r = call <4 x i32> @llvm.vp.gather.v4i32.v4p0(<4 x ptr> align 4 <ptr @a, ptr poison, ptr @a, ptr @a>, <4 x i1> <i1 false, i1 true, i1 false, i1 false>, i32 4)
  ret void
}

define void @poison_evl() {
; POISON-EVL: Entering function: poison_evl
; POISON-EVL-NEXT: Stacktrace:
; POISON-EVL-NEXT: #0   %r = call <4 x i32> @llvm.vp.gather.v4i32.v4p0(<4 x ptr> align 4 <ptr @a, ptr @a, ptr @a, ptr @a>, <4 x i1> splat (i1 true), i32 poison) at @poison_evl <stdin>:{{[0-9]+}}
; POISON-EVL-NEXT: Immediate UB detected: Poison EVL in llvm.vp.gather.
; POISON-EVL-NEXT: error: Execution of function 'poison_evl' failed.
  %r = call <4 x i32> @llvm.vp.gather.v4i32.v4p0(<4 x ptr> align 4 <ptr @a, ptr @a, ptr @a, ptr @a>, <4 x i1> splat (i1 true), i32 poison)
  ret void
}

define void @evl_too_large() {
; EVL-TOO-LARGE: Entering function: evl_too_large
; EVL-TOO-LARGE-NEXT: Stacktrace:
; EVL-TOO-LARGE-NEXT: #0   %r = call <4 x i32> @llvm.vp.gather.v4i32.v4p0(<4 x ptr> align 4 <ptr @a, ptr @a, ptr @a, ptr @a>, <4 x i1> splat (i1 true), i32 5) at @evl_too_large <stdin>:{{[0-9]+}}
; EVL-TOO-LARGE-NEXT: Immediate UB detected: llvm.vp.gather with EVL 5 exceeding the number of lanes 4.
; EVL-TOO-LARGE-NEXT: error: Execution of function 'evl_too_large' failed.
  %r = call <4 x i32> @llvm.vp.gather.v4i32.v4p0(<4 x ptr> align 4 <ptr @a, ptr @a, ptr @a, ptr @a>, <4 x i1> splat (i1 true), i32 5)
  ret void
}

define void @evl_too_large_scalable() {
; EVL-TOO-LARGE-SCALABLE: Entering function: evl_too_large_scalable
; EVL-TOO-LARGE-SCALABLE-NEXT:   %ptrs = getelementptr i32, ptr @a, <vscale x 2 x i64> zeroinitializer => { ptr 0x8 [@a], ptr 0x8 [@a], ptr 0x8 [@a], ptr 0x8 [@a] }
; EVL-TOO-LARGE-SCALABLE-NEXT: Stacktrace:
; EVL-TOO-LARGE-SCALABLE-NEXT: #0   %r = call <vscale x 2 x i32> @llvm.vp.gather.nxv2i32.nxv2p0(<vscale x 2 x ptr> align 4 %ptrs, <vscale x 2 x i1> splat (i1 true), i32 5) at @evl_too_large_scalable <stdin>:{{[0-9]+}}
; EVL-TOO-LARGE-SCALABLE-NEXT: Immediate UB detected: llvm.vp.gather with EVL 5 exceeding the number of lanes 4.
; EVL-TOO-LARGE-SCALABLE-NEXT: error: Execution of function 'evl_too_large_scalable' failed.
  %ptrs = getelementptr i32, ptr @a, <vscale x 2 x i64> zeroinitializer
  %r = call <vscale x 2 x i32> @llvm.vp.gather.nxv2i32.nxv2p0(<vscale x 2 x ptr> align 4 %ptrs, <vscale x 2 x i1> splat (i1 true), i32 5)
  ret void
}

; The align attribute turns the misaligned pointer into poison.
define void @misaligned() {
; MISALIGNED: Entering function: misaligned
; MISALIGNED-NEXT:   %ptrs = getelementptr i8, ptr @a, <2 x i64> <i64 0, i64 2> => { ptr 0x8 [@a], ptr 0xA [@a + 2] }
; MISALIGNED-NEXT: Stacktrace:
; MISALIGNED-NEXT: #0   %r = call <2 x i32> @llvm.vp.gather.v2i32.v2p0(<2 x ptr> align 4 %ptrs, <2 x i1> <i1 false, i1 true>, i32 2) at @misaligned <stdin>:{{[0-9]+}}
; MISALIGNED-NEXT: Immediate UB detected: Invalid memory access with a poison pointer.
; MISALIGNED-NEXT: error: Execution of function 'misaligned' failed.
  %ptrs = getelementptr i8, ptr @a, <2 x i64> <i64 0, i64 2>
  %r = call <2 x i32> @llvm.vp.gather.v2i32.v2p0(<2 x ptr> align 4 %ptrs, <2 x i1> <i1 false, i1 true>, i32 2)
  ret void
}

; Without the align attribute, the ABI alignment of the element type is used.
define void @misaligned_abi() {
; MISALIGNED-ABI: Entering function: misaligned_abi
; MISALIGNED-ABI-NEXT:   %ptrs = getelementptr i8, ptr @a, <2 x i64> <i64 0, i64 2> => { ptr 0x8 [@a], ptr 0xA [@a + 2] }
; MISALIGNED-ABI-NEXT: Stacktrace:
; MISALIGNED-ABI-NEXT: #0   %r = call <2 x i32> @llvm.vp.gather.v2i32.v2p0(<2 x ptr> %ptrs, <2 x i1> <i1 false, i1 true>, i32 2) at @misaligned_abi <stdin>:{{[0-9]+}}
; MISALIGNED-ABI-NEXT: Immediate UB detected: Misaligned memory access. Address: 0xA, Required alignment: 4.
; MISALIGNED-ABI-NEXT: error: Execution of function 'misaligned_abi' failed.
  %ptrs = getelementptr i8, ptr @a, <2 x i64> <i64 0, i64 2>
  %r = call <2 x i32> @llvm.vp.gather.v2i32.v2p0(<2 x ptr> %ptrs, <2 x i1> <i1 false, i1 true>, i32 2)
  ret void
}
