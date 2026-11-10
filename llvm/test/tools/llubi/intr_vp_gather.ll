; RUN: llubi --verbose --vscale=2 < %s 2>&1 | FileCheck %s

@a = global [4 x i32] [i32 0, i32 1, i32 2, i32 3]
@b = global [2 x ptr] [ptr null, ptr @a]
@d = global [2 x i8] [i8 2, i8 1]

define void @main() {
  %ptrs = getelementptr i32, ptr @a, <4 x i64> <i64 3, i64 0, i64 2, i64 1>
  %gather = call <4 x i32> @llvm.vp.gather.v4i32.v4p0(<4 x ptr> align 4 %ptrs, <4 x i1> <i1 true, i1 false, i1 true, i1 false>, i32 4)
  %gather_all = call <4 x i32> @llvm.vp.gather.v4i32.v4p0(<4 x ptr> align 4 %ptrs, <4 x i1> splat (i1 true), i32 4)

  ; Lanes at or above the EVL are disabled.
  %gather_evl = call <4 x i32> @llvm.vp.gather.v4i32.v4p0(<4 x ptr> align 4 %ptrs, <4 x i1> <i1 true, i1 false, i1 true, i1 true>, i32 3)

  ; The alignment defaults to the ABI alignment of the element type.
  %gather_noalign = call <4 x i32> @llvm.vp.gather.v4i32.v4p0(<4 x ptr> %ptrs, <4 x i1> splat (i1 true), i32 4)

  ; Only enabled lanes are accessed, so the pointers of disabled lanes may be
  ; poison, null, out of bounds or misaligned, and the mask lanes at or above
  ; the EVL may be poison.
  %gather_none = call <4 x i32> @llvm.vp.gather.v4i32.v4p0(<4 x ptr> <ptr poison, ptr null, ptr getelementptr (i8, ptr @a, i64 16), ptr getelementptr (i8, ptr @a, i64 2)>, <4 x i1> zeroinitializer, i32 4)
  %gather_evl_zero = call <4 x i32> @llvm.vp.gather.v4i32.v4p0(<4 x ptr> <ptr poison, ptr null, ptr getelementptr (i8, ptr @a, i64 16), ptr getelementptr (i8, ptr @a, i64 2)>, <4 x i1> poison, i32 0)
  %gather_evl_tail = call <4 x i32> @llvm.vp.gather.v4i32.v4p0(<4 x ptr> <ptr @a, ptr null, ptr poison, ptr getelementptr (i8, ptr @a, i64 2)>, <4 x i1> <i1 true, i1 false, i1 poison, i1 true>, i32 2)

  ; Each lane is a scalar access, also for elements that are not a whole number
  ; of bytes.
  %ptrs_d = getelementptr i8, ptr @d, <2 x i64> <i64 1, i64 0>
  %gather_i4 = call <2 x i4> @llvm.vp.gather.v2i4.v2p0(<2 x ptr> %ptrs_d, <2 x i1> <i1 true, i1 true>, i32 1)

  %ptrs_b = getelementptr ptr, ptr @b, <2 x i64> <i64 1, i64 0>
  %gather_ptr = call <2 x ptr> @llvm.vp.gather.v2p0.v2p0(<2 x ptr> align 8 %ptrs_b, <2 x i1> <i1 true, i1 false>, i32 2)
  %ptr = extractelement <2 x ptr> %gather_ptr, i64 0
  %deref = load i32, ptr %ptr

  %step = call <vscale x 2 x i64> @llvm.stepvector.nxv2i64()
  %ptrs_scalable = getelementptr i32, ptr @a, <vscale x 2 x i64> %step
  %gather_scalable = call <vscale x 2 x i32> @llvm.vp.gather.nxv2i32.nxv2p0(<vscale x 2 x ptr> align 4 %ptrs_scalable, <vscale x 2 x i1> splat (i1 true), i32 3)
  %gather_scalable_full = call <vscale x 2 x i32> @llvm.vp.gather.nxv2i32.nxv2p0(<vscale x 2 x ptr> align 4 %ptrs_scalable, <vscale x 2 x i1> splat (i1 true), i32 4)
  ret void
}
