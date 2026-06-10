; RUN: opt -p loop-vectorize -mtriple=riscv64 -mattr=+v %s -S -o /dev/null
;
; Reproducer for crash in VPInstructionWithType::computeCost hitting
; "Unhandled opcode" for StepVector. The StepVector is created by EVL tail
; folding for a loop with pointer inductions.

define void @pointer_induction_stepvector_cost(ptr %end) {
entry:
  br label %loop

loop:
  %dst = phi ptr [ %dst.next, %loop ], [ null, %entry ]
  %src = phi ptr [ %src.next, %loop ], [ null, %entry ]
  store i32 0, ptr %dst, align 2
  %src.next = getelementptr i8, ptr %src, i64 4
  %dst.next = getelementptr i8, ptr %dst, i64 4
  %cmp = icmp eq ptr %src, %end
  br i1 %cmp, label %exit, label %loop

exit:
  ret void
}
