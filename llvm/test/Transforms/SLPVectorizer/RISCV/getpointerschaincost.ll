; RUN: opt -mtriple=riscv64 -mattr=+v -riscv-v-slp-max-vf=0 -passes=slp-vectorizer -disable-output -debug-only=SLP < %s 2>&1 | FileCheck %s

; Because all of these addresses are foldable, the scalar cost should be 0 when
; computing the pointers chain cost.
define void @f(ptr %dest, i64 %i) {
; CHECK-LABEL: SLP: Analyzing blocks in f.
; CHECK: SLP: Calculated GEPs cost for Tree:
; CHECK: SLP: Costs:
; CHECK: SLP:     ReuseShuffleCost = 0
; CHECK-NEXT: SLP:     VectorCost = 0
; CHECK-NEXT: SLP:     ScalarCost = 0
; CHECK-NEXT: SLP:     ReuseShuffleCost + VecCost - ScalarCost = 0
entry:
  %p1 = getelementptr i32, ptr %dest, i32 0
  store i32 1, ptr %p1
  %p2 = getelementptr i32, ptr %dest, i32 1
  store i32 1, ptr %p2
  %p3 = getelementptr i32, ptr %dest, i32 2
  store i32 1, ptr %p3
  %p4 = getelementptr i32, ptr %dest, i32 3
  store i32 1, ptr %p4
  ret void
}

; When computing the scalar pointers chain cost here, there is a cost of 1 for
; the base pointer, and the rest can be folded in, so the scalar cost should be
; 1.
define void @g(ptr %dest, i64 %i) {
; CHECK-LABEL: SLP: Analyzing blocks in g.
; CHECK: SLP: Calculated GEPs cost for Tree:
; CHECK: SLP: Costs:
; CHECK-NEXT: SLP:     ReuseShuffleCost = 0
; CHECK-NEXT: SLP:     VectorCost = 1
; CHECK-NEXT: SLP:     ScalarCost = 1
entry:
  %p1 = getelementptr i32, ptr %dest, i32 2048
  store i32 1, ptr %p1
  %p2 = getelementptr i32, ptr %dest, i32 2049
  store i32 1, ptr %p2
  %p3 = getelementptr i32, ptr %dest, i32 2050
  store i32 1, ptr %p3
  %p4 = getelementptr i32, ptr %dest, i32 2051
  store i32 1, ptr %p4
  ret void
}

; When computing the scalar pointers chain cost here, there is a cost of 1 for
; the base pointer, and the rest can be folded in, so the scalar cost should be
; 1.
define void @h(ptr %dest, i32 %i) {
; CHECK-LABEL: SLP: Analyzing blocks in h.
; CHECK: SLP: Calculated GEPs cost for Tree:
; CHECK: SLP: Costs:
; CHECK-NEXT: SLP:     ReuseShuffleCost = 0
; CHECK-NEXT: SLP:     VectorCost = 1
; CHECK-NEXT: SLP:     ScalarCost = 1
entry:
  %p1 = getelementptr [4 x i32], ptr %dest, i32 %i, i32 0
  store i32 1, ptr %p1
  %p2 = getelementptr [4 x i32], ptr %dest, i32 %i, i32 1
  store i32 1, ptr %p2
  %p3 = getelementptr [4 x i32], ptr %dest, i32 %i, i32 2
  store i32 1, ptr %p3
  %p4 = getelementptr [4 x i32], ptr %dest, i32 %i, i32 3
  store i32 1, ptr %p4
  ret void
}
