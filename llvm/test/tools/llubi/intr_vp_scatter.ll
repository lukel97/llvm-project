; RUN: llubi --verbose --vscale=2 < %s 2>&1 | FileCheck %s

@a = global [4 x i32] [i32 0, i32 1, i32 2, i32 3]
@b = global [2 x ptr] zeroinitializer
@c = constant [4 x i32] [i32 0, i32 1, i32 2, i32 3]
@d = global [2 x i8] zeroinitializer

define void @main() {
  %ptrs = getelementptr i32, ptr @a, <4 x i64> <i64 3, i64 0, i64 2, i64 1>
  call void @llvm.vp.scatter.v4i32.v4p0(<4 x i32> <i32 10, i32 poison, i32 12, i32 poison>, <4 x ptr> align 4 %ptrs, <4 x i1> <i1 true, i1 false, i1 true, i1 false>, i32 4)
  %after_scatter = load <4 x i32>, ptr @a, align 4

  ; Lanes at or above the EVL are disabled.
  call void @llvm.vp.scatter.v4i32.v4p0(<4 x i32> <i32 30, i32 31, i32 32, i32 33>, <4 x ptr> align 4 %ptrs, <4 x i1> <i1 true, i1 false, i1 true, i1 true>, i32 3)
  %after_evl = load <4 x i32>, ptr @a, align 4

  ; The alignment defaults to the ABI alignment of the element type.
  call void @llvm.vp.scatter.v4i32.v4p0(<4 x i32> <i32 40, i32 41, i32 42, i32 43>, <4 x ptr> %ptrs, <4 x i1> splat (i1 true), i32 4)
  %after_noalign = load <4 x i32>, ptr @a, align 4

  ; Only enabled lanes are accessed, so the pointers of disabled lanes may be
  ; poison, null, out of bounds, misaligned or point to constant memory, and
  ; the mask lanes at or above the EVL may be poison.
  call void @llvm.vp.scatter.v4i32.v4p0(<4 x i32> poison, <4 x ptr> <ptr poison, ptr null, ptr getelementptr (i8, ptr @a, i64 18), ptr @c>, <4 x i1> zeroinitializer, i32 4)
  call void @llvm.vp.scatter.v4i32.v4p0(<4 x i32> poison, <4 x ptr> <ptr poison, ptr null, ptr getelementptr (i8, ptr @a, i64 18), ptr @c>, <4 x i1> poison, i32 0)
  call void @llvm.vp.scatter.v4i32.v4p0(<4 x i32> <i32 50, i32 51, i32 poison, i32 poison>, <4 x ptr> <ptr @a, ptr null, ptr poison, ptr @c>, <4 x i1> <i1 true, i1 false, i1 poison, i1 true>, i32 2)
  %after_none = load <4 x i32>, ptr @a, align 4

  ; Lanes are stored in order, so later lanes win for overlapping addresses.
  %ptrs_dup = getelementptr i32, ptr @a, <4 x i64> <i64 0, i64 1, i64 0, i64 1>
  call void @llvm.vp.scatter.v4i32.v4p0(<4 x i32> <i32 20, i32 21, i32 22, i32 23>, <4 x ptr> align 4 %ptrs_dup, <4 x i1> splat (i1 true), i32 3)
  %after_dup = load <4 x i32>, ptr @a, align 4

  ; Each lane is a scalar access, also for elements that are not a whole number
  ; of bytes.
  %ptrs_d = getelementptr i8, ptr @d, <2 x i64> <i64 1, i64 0>
  call void @llvm.vp.scatter.v2i4.v2p0(<2 x i4> <i4 1, i4 2>, <2 x ptr> %ptrs_d, <2 x i1> splat (i1 true), i32 1)
  %d0 = load i4, ptr @d
  %d1 = load i4, ptr getelementptr (i8, ptr @d, i64 1)

  %ptrs_b = getelementptr ptr, ptr @b, <2 x i64> <i64 1, i64 0>
  call void @llvm.vp.scatter.v2p0.v2p0(<2 x ptr> <ptr @a, ptr poison>, <2 x ptr> align 8 %ptrs_b, <2 x i1> <i1 true, i1 false>, i32 2)
  %ptr = load ptr, ptr getelementptr (i8, ptr @b, i64 8)
  %deref = load i32, ptr %ptr

  %step = call <vscale x 2 x i64> @llvm.stepvector.nxv2i64()
  %ptrs_scalable = getelementptr i32, ptr @a, <vscale x 2 x i64> %step
  call void @llvm.vp.scatter.nxv2i32.nxv2p0(<vscale x 2 x i32> splat (i32 60), <vscale x 2 x ptr> align 4 %ptrs_scalable, <vscale x 2 x i1> splat (i1 true), i32 3)
  %after_scalable = load <4 x i32>, ptr @a, align 4
  call void @llvm.vp.scatter.nxv2i32.nxv2p0(<vscale x 2 x i32> splat (i32 70), <vscale x 2 x ptr> align 4 %ptrs_scalable, <vscale x 2 x i1> splat (i1 true), i32 4)
  %after_scalable_full = load <4 x i32>, ptr @a, align 4
  ret void
}
