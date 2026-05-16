	.attribute	4, 16
	.attribute	5, "rv64i2p1_f2p2_d2p2_v1p0_zicsr2p0_zve32f1p0_zve32x1p0_zve64d1p0_zve64f1p0_zve64x1p0_zvl128b1p0_zvl32b1p0_zvl64b1p0"
	.file	"masked-udiv.ll"
	.text
	.globl	udiv_nxv8i8                     # -- Begin function udiv_nxv8i8
	.p2align	2
	.type	udiv_nxv8i8,@function
	.variant_cc	udiv_nxv8i8
udiv_nxv8i8:                            # @udiv_nxv8i8
	.cfi_startproc
# %bb.0:
	vsetvli	a0, zero, e8, m1, ta, ma
	vdivu.vv	v8, v8, v9, v0.t
	ret
.Lfunc_end0:
	.size	udiv_nxv8i8, .Lfunc_end0-udiv_nxv8i8
	.cfi_endproc
                                        # -- End function
	.globl	udiv_nxv4i16                    # -- Begin function udiv_nxv4i16
	.p2align	2
	.type	udiv_nxv4i16,@function
	.variant_cc	udiv_nxv4i16
udiv_nxv4i16:                           # @udiv_nxv4i16
	.cfi_startproc
# %bb.0:
	vsetvli	a0, zero, e16, m1, ta, ma
	vdivu.vv	v8, v8, v9, v0.t
	ret
.Lfunc_end1:
	.size	udiv_nxv4i16, .Lfunc_end1-udiv_nxv4i16
	.cfi_endproc
                                        # -- End function
	.globl	udiv_nxv4i32                    # -- Begin function udiv_nxv4i32
	.p2align	2
	.type	udiv_nxv4i32,@function
	.variant_cc	udiv_nxv4i32
udiv_nxv4i32:                           # @udiv_nxv4i32
	.cfi_startproc
# %bb.0:
	vsetvli	a0, zero, e32, m2, ta, ma
	vdivu.vv	v8, v8, v10, v0.t
	ret
.Lfunc_end2:
	.size	udiv_nxv4i32, .Lfunc_end2-udiv_nxv4i32
	.cfi_endproc
                                        # -- End function
	.globl	udiv_nxv8i32                    # -- Begin function udiv_nxv8i32
	.p2align	2
	.type	udiv_nxv8i32,@function
	.variant_cc	udiv_nxv8i32
udiv_nxv8i32:                           # @udiv_nxv8i32
	.cfi_startproc
# %bb.0:
	vsetvli	a0, zero, e32, m4, ta, ma
	vdivu.vv	v8, v8, v12, v0.t
	ret
.Lfunc_end3:
	.size	udiv_nxv8i32, .Lfunc_end3-udiv_nxv8i32
	.cfi_endproc
                                        # -- End function
	.globl	udiv_nxv8i32_splat_rhs          # -- Begin function udiv_nxv8i32_splat_rhs
	.p2align	2
	.type	udiv_nxv8i32_splat_rhs,@function
	.variant_cc	udiv_nxv8i32_splat_rhs
udiv_nxv8i32_splat_rhs:                 # @udiv_nxv8i32_splat_rhs
	.cfi_startproc
# %bb.0:
	vsetvli	a1, zero, e32, m4, ta, ma
	vdivu.vx	v8, v8, a0, v0.t
	ret
.Lfunc_end4:
	.size	udiv_nxv8i32_splat_rhs, .Lfunc_end4-udiv_nxv8i32_splat_rhs
	.cfi_endproc
                                        # -- End function
	.globl	udiv_nxv8i16                    # -- Begin function udiv_nxv8i16
	.p2align	2
	.type	udiv_nxv8i16,@function
	.variant_cc	udiv_nxv8i16
udiv_nxv8i16:                           # @udiv_nxv8i16
	.cfi_startproc
# %bb.0:
	vsetvli	a0, zero, e16, m2, ta, ma
	vdivu.vv	v8, v8, v10, v0.t
	ret
.Lfunc_end5:
	.size	udiv_nxv8i16, .Lfunc_end5-udiv_nxv8i16
	.cfi_endproc
                                        # -- End function
	.globl	udiv_nxv2i64                    # -- Begin function udiv_nxv2i64
	.p2align	2
	.type	udiv_nxv2i64,@function
	.variant_cc	udiv_nxv2i64
udiv_nxv2i64:                           # @udiv_nxv2i64
	.cfi_startproc
# %bb.0:
	vsetvli	a0, zero, e64, m2, ta, ma
	vdivu.vv	v8, v8, v10, v0.t
	ret
.Lfunc_end6:
	.size	udiv_nxv2i64, .Lfunc_end6-udiv_nxv2i64
	.cfi_endproc
                                        # -- End function
	.globl	udiv_v4i32                      # -- Begin function udiv_v4i32
	.p2align	2
	.type	udiv_v4i32,@function
	.variant_cc	udiv_v4i32
udiv_v4i32:                             # @udiv_v4i32
	.cfi_startproc
# %bb.0:
	vsetivli	zero, 4, e32, m1, ta, ma
	vdivu.vv	v8, v8, v9, v0.t
	ret
.Lfunc_end7:
	.size	udiv_v4i32, .Lfunc_end7-udiv_v4i32
	.cfi_endproc
                                        # -- End function
	.globl	udiv_v2i64                      # -- Begin function udiv_v2i64
	.p2align	2
	.type	udiv_v2i64,@function
	.variant_cc	udiv_v2i64
udiv_v2i64:                             # @udiv_v2i64
	.cfi_startproc
# %bb.0:
	vsetivli	zero, 2, e64, m1, ta, ma
	vdivu.vv	v8, v8, v9, v0.t
	ret
.Lfunc_end8:
	.size	udiv_v2i64, .Lfunc_end8-udiv_v2i64
	.cfi_endproc
                                        # -- End function
	.globl	udiv_v4i64                      # -- Begin function udiv_v4i64
	.p2align	2
	.type	udiv_v4i64,@function
	.variant_cc	udiv_v4i64
udiv_v4i64:                             # @udiv_v4i64
	.cfi_startproc
# %bb.0:
	vsetivli	zero, 4, e64, m2, ta, ma
	vdivu.vv	v8, v8, v10, v0.t
	ret
.Lfunc_end9:
	.size	udiv_v4i64, .Lfunc_end9-udiv_v4i64
	.cfi_endproc
                                        # -- End function
	.globl	udiv_v2i32                      # -- Begin function udiv_v2i32
	.p2align	2
	.type	udiv_v2i32,@function
	.variant_cc	udiv_v2i32
udiv_v2i32:                             # @udiv_v2i32
	.cfi_startproc
# %bb.0:
	vsetivli	zero, 2, e32, mf2, ta, ma
	vdivu.vv	v8, v8, v9, v0.t
	ret
.Lfunc_end10:
	.size	udiv_v2i32, .Lfunc_end10-udiv_v2i32
	.cfi_endproc
                                        # -- End function
	.globl	udiv_v4i16                      # -- Begin function udiv_v4i16
	.p2align	2
	.type	udiv_v4i16,@function
	.variant_cc	udiv_v4i16
udiv_v4i16:                             # @udiv_v4i16
	.cfi_startproc
# %bb.0:
	vsetivli	zero, 4, e16, mf2, ta, ma
	vdivu.vv	v8, v8, v9, v0.t
	ret
.Lfunc_end11:
	.size	udiv_v4i16, .Lfunc_end11-udiv_v4i16
	.cfi_endproc
                                        # -- End function
	.globl	udiv_v8i16                      # -- Begin function udiv_v8i16
	.p2align	2
	.type	udiv_v8i16,@function
	.variant_cc	udiv_v8i16
udiv_v8i16:                             # @udiv_v8i16
	.cfi_startproc
# %bb.0:
	vsetivli	zero, 8, e16, m1, ta, ma
	vdivu.vv	v8, v8, v9, v0.t
	ret
.Lfunc_end12:
	.size	udiv_v8i16, .Lfunc_end12-udiv_v8i16
	.cfi_endproc
                                        # -- End function
	.globl	udiv_v1i64                      # -- Begin function udiv_v1i64
	.p2align	2
	.type	udiv_v1i64,@function
	.variant_cc	udiv_v1i64
udiv_v1i64:                             # @udiv_v1i64
	.cfi_startproc
# %bb.0:
	vsetivli	zero, 1, e64, m1, ta, ma
	vdivu.vv	v8, v8, v9, v0.t
	ret
.Lfunc_end13:
	.size	udiv_v1i64, .Lfunc_end13-udiv_v1i64
	.cfi_endproc
                                        # -- End function
	.globl	udiv_v2i128                     # -- Begin function udiv_v2i128
	.p2align	2
	.type	udiv_v2i128,@function
	.variant_cc	udiv_v2i128
udiv_v2i128:                            # @udiv_v2i128
# %bb.0:
	addi	sp, sp, -80
	sd	ra, 72(sp)                      # 8-byte Folded Spill
	sd	s0, 64(sp)                      # 8-byte Folded Spill
	sd	s1, 56(sp)                      # 8-byte Folded Spill
	sd	s2, 48(sp)                      # 8-byte Folded Spill
	sd	s3, 40(sp)                      # 8-byte Folded Spill
	sd	s4, 32(sp)                      # 8-byte Folded Spill
	sd	s5, 24(sp)                      # 8-byte Folded Spill
	sd	s6, 16(sp)                      # 8-byte Folded Spill
	csrr	a3, vlenb
	sub	sp, sp, a3
	mv	a4, a1
	addi	a3, sp, 16
	vs1r.v	v0, (a3)                        # vscale x 8-byte Folded Spill
	ld	a6, 16(a1)
	ld	a1, 24(a1)
	ld	a3, 24(a2)
	vsetivli	zero, 2, e8, mf8, ta, ma
	vmv.v.i	v8, 0
	vmerge.vim	v8, v8, 1, v0
	vslidedown.vi	v8, v8, 1
	vmv.x.s	a5, v8
	andi	a7, a5, 1
	mv	s0, a0
	bnez	a7, .LBB14_2
# %bb.1:
	li	a5, 1
	j	.LBB14_3
.LBB14_2:
	ld	a5, 16(a2)
.LBB14_3:
	ld	s1, 0(a4)
	ld	s2, 8(a4)
	ld	s3, 0(a2)
	ld	s6, 8(a2)
	neg	a0, a7
	and	a3, a0, a3
	mv	a0, a6
	mv	a2, a5
	call	__udivti3
	mv	s4, a0
	addi	a0, sp, 16
	vl1r.v	v8, (a0)                        # vscale x 8-byte Folded Reload
	vsetivli	zero, 2, e8, mf8, ta, ma
	vfirst.m	a0, v8
	mv	s5, a1
	beqz	a0, .LBB14_5
# %bb.4:
	li	s3, 1
.LBB14_5:
	snez	a0, a0
	addi	a0, a0, -1
	and	a3, a0, s6
	mv	a0, s1
	mv	a1, s2
	mv	a2, s3
	call	__udivti3
	sd	a0, 0(s0)
	sd	a1, 8(s0)
	sd	s4, 16(s0)
	sd	s5, 24(s0)
	csrr	a0, vlenb
	add	sp, sp, a0
	ld	ra, 72(sp)                      # 8-byte Folded Reload
	ld	s0, 64(sp)                      # 8-byte Folded Reload
	ld	s1, 56(sp)                      # 8-byte Folded Reload
	ld	s2, 48(sp)                      # 8-byte Folded Reload
	ld	s3, 40(sp)                      # 8-byte Folded Reload
	ld	s4, 32(sp)                      # 8-byte Folded Reload
	ld	s5, 24(sp)                      # 8-byte Folded Reload
	ld	s6, 16(sp)                      # 8-byte Folded Reload
	addi	sp, sp, 80
	ret
.Lfunc_end14:
	.size	udiv_v2i128, .Lfunc_end14-udiv_v2i128
                                        # -- End function
	.globl	udiv_v3i10                      # -- Begin function udiv_v3i10
	.p2align	2
	.type	udiv_v3i10,@function
	.variant_cc	udiv_v3i10
udiv_v3i10:                             # @udiv_v3i10
	.cfi_startproc
# %bb.0:
	ld	a3, 0(a2)
	vsetivli	zero, 4, e16, mf2, ta, ma
	vmv.v.i	v8, 7
	ld	a4, 8(a2)
	ld	a2, 16(a2)
	vmv.v.x	v9, a3
	ld	a3, 0(a1)
	ld	a5, 8(a1)
	ld	a1, 16(a1)
	vmand.mm	v0, v0, v8
	vmv.v.x	v8, a3
	vslide1down.vx	v9, v9, a4
	li	a3, 1023
	vslide1down.vx	v8, v8, a5
	vslide1down.vx	v9, v9, a2
	vslide1down.vx	v8, v8, a1
	vslidedown.vi	v9, v9, 1
	vslidedown.vi	v8, v8, 1
	vand.vx	v9, v9, a3
	vand.vx	v8, v8, a3
	vdivu.vv	v8, v8, v9, v0.t
	vmv.x.s	a1, v8
	vslidedown.vi	v9, v8, 1
	vslidedown.vi	v8, v8, 2
	andi	a1, a1, 1023
	vmv.x.s	a2, v9
	vmv.x.s	a3, v8
	andi	a2, a2, 1023
	slli	a3, a3, 20
	slli	a2, a2, 10
	or	a1, a1, a3
	or	a1, a1, a2
	slli	a1, a1, 34
	srli	a1, a1, 34
	sw	a1, 0(a0)
	ret
.Lfunc_end15:
	.size	udiv_v3i10, .Lfunc_end15-udiv_v3i10
	.cfi_endproc
                                        # -- End function
	.globl	udiv_trunc_select               # -- Begin function udiv_trunc_select
	.p2align	2
	.type	udiv_trunc_select,@function
	.variant_cc	udiv_trunc_select
udiv_trunc_select:                      # @udiv_trunc_select
	.cfi_startproc
# %bb.0:
	vsetivli	zero, 8, e16, m1, ta, ma
	vmclr.m	v0
	vmv.v.i	v8, 0
	vdivu.vx	v8, v8, zero, v0.t
	andi	a0, a0, 1
	vsetvli	zero, zero, e8, mf2, ta, ma
	vmv.v.x	v9, a0
	vmsne.vi	v0, v9, 0
	vnsrl.wi	v8, v8, 0
	vmerge.vim	v8, v8, 0, v0
	ret
.Lfunc_end16:
	.size	udiv_trunc_select, .Lfunc_end16-udiv_trunc_select
	.cfi_endproc
                                        # -- End function
	.section	".note.GNU-stack","",@progbits
