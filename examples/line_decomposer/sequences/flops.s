	.file	"flops.c"
	.option nopic
	.attribute arch, "rv32i2p1_m2p0_a2p1_c2p0_zicsr2p0_zfinx1p0_zdinx1p0_zba1p0_zbb1p0_zbs1p0"
	.attribute unaligned_access, 0
	.attribute stack_align, 16
	.text
	.section	.rodata.str1.4,"aMS",@progbits,1
	.align	2
.LC0:
	.string	"   FLOPS C Program (Double Precision), V2.0 18 Dec 1992\n"
	.align	2
.LC9:
	.string	"   Module     Error        RunTime      MFLOPS"
	.align	2
.LC10:
	.string	"                            (usec)"
	.align	2
.LC14:
	.string	"     1   %13.4le  %10.4lf  %10.4lf\n"
	.align	2
.LC17:
	.string	"     2   %13.4le  %10.4lf  %10.4lf\n"
	.align	2
.LC20:
	.string	"     3   %13.4le  %10.4lf  %10.4lf\n"
	.align	2
.LC21:
	.string	"     4   %13.4le  %10.4lf  %10.4lf\n"
	.align	2
.LC24:
	.string	"     5   %13.4le  %10.4lf  %10.4lf\n"
	.align	2
.LC26:
	.string	"     6   %13.4le  %10.4lf  %10.4lf\n"
	.align	2
.LC31:
	.string	"     7   %13.4le  %10.4lf  %10.4lf\n"
	.align	2
.LC34:
	.string	"     8   %13.4le  %10.4lf  %10.4lf\n"
	.align	2
.LC39:
	.string	"   Iterations      = %10ld\n"
	.align	2
.LC40:
	.string	"   NullTime (usec) = %10.4lf\n"
	.align	2
.LC41:
	.string	"   MFLOPS(1)       = %10.4lf\n"
	.align	2
.LC42:
	.string	"   MFLOPS(2)       = %10.4lf\n"
	.align	2
.LC43:
	.string	"   MFLOPS(3)       = %10.4lf\n"
	.align	2
.LC44:
	.string	"   MFLOPS(4)       = %10.4lf\n\n"
	.text
	.align	1
	.globl	main
	.type	main, @function
main:
	addi	sp,sp,-128
	sw	ra,124(sp)
	sw	s0,120(sp)
	sw	s1,116(sp)
	sw	s2,112(sp)
	sw	s3,108(sp)
	sw	s4,104(sp)
	sw	s5,100(sp)
	sw	s6,96(sp)
	sw	s7,92(sp)
	sw	s8,88(sp)
	sw	s9,84(sp)
	sw	s10,80(sp)
	sw	s11,76(sp)
	li	a0,10
	call	putchar
	lui	a0,%hi(.LC0)
	addi	a0,a0,%lo(.LC0)
	call	puts
	lui	s0,%hi(.LANCHOR0)
	addi	s0,s0,%lo(.LANCHOR0)
	lui	a5,%hi(.LC1)
	lw	a4,%lo(.LC1)(a5)
	lw	a5,%lo(.LC1+4)(a5)
	sw	a4,8(s0)
	sw	a5,12(s0)
	lui	a4,%hi(.LC2)
	lui	a5,%hi(TLimit)
	lw	a2,%lo(.LC2)(a4)
	lw	a3,%lo(.LC2+4)(a4)
	sw	a2,%lo(TLimit)(a5)
	sw	a3,%lo(TLimit+4)(a5)
	lui	a4,%hi(.LC3)
	lui	a5,%hi(piref)
	lw	a2,%lo(.LC3)(a4)
	lw	a3,%lo(.LC3+4)(a4)
	sw	a2,%lo(piref)(a5)
	sw	a3,%lo(piref+4)(a5)
	lui	a5,%hi(.LC4)
	lw	a4,%lo(.LC4)(a5)
	lw	a5,%lo(.LC4+4)(a5)
	lui	a3,%hi(one)
	sw	a4,%lo(one)(a3)
	sw	a5,%lo(one+4)(a3)
	lui	a2,%hi(.LC5)
	lui	a3,%hi(two)
	lw	a0,%lo(.LC5)(a2)
	lw	a1,%lo(.LC5+4)(a2)
	sw	a0,%lo(two)(a3)
	sw	a1,%lo(two+4)(a3)
	lui	a2,%hi(.LC6)
	lui	a3,%hi(three)
	lw	a0,%lo(.LC6)(a2)
	lw	a1,%lo(.LC6+4)(a2)
	sw	a0,%lo(three)(a3)
	sw	a1,%lo(three+4)(a3)
	lui	a2,%hi(.LC7)
	lui	a3,%hi(four)
	lw	a0,%lo(.LC7)(a2)
	lw	a1,%lo(.LC7+4)(a2)
	sw	a0,%lo(four)(a3)
	sw	a1,%lo(four+4)(a3)
	lui	a2,%hi(.LC8)
	lui	a3,%hi(five)
	lw	a0,%lo(.LC8)(a2)
	lw	a1,%lo(.LC8+4)(a2)
	sw	a0,%lo(five)(a3)
	sw	a1,%lo(five+4)(a3)
	lui	a3,%hi(scale)
	sw	a4,%lo(scale)(a3)
	sw	a5,%lo(scale+4)(a3)
	lui	a0,%hi(.LC9)
	addi	a0,a0,%lo(.LC9)
	call	puts
	lui	a0,%hi(.LC10)
	addi	a0,a0,%lo(.LC10)
	call	puts
	addi	s0,s0,288
	mv	a0,s0
	call	dtime
	mv	a0,s0
	call	dtime
	lui	a5,%hi(sa)
	sw	zero,%lo(sa)(a5)
	sw	zero,%lo(sa+4)(a5)
	li	a5,16384
	addi	s3,a5,-759
	lui	a5,%hi(.LANCHOR0)
	addi	a5,a5,%lo(.LANCHOR0)
	sw	s0,8(sp)
	sw	s3,0(sp)
	sw	a5,24(sp)
	j	.L2
.L54:
	lw	s3,0(sp)
	fcvt.d.w	a2,s3
	lui	a5,%hi(.LC11)
	lw	a4,%lo(.LC11)(a5)
	lw	a5,%lo(.LC11+4)(a5)
	fdiv.d	a4,a4,a2
	lui	a3,%hi(scale)
	sw	a4,%lo(scale)(a3)
	sw	a5,%lo(scale+4)(a3)
	lui	a0,%hi(.LANCHOR0)
	addi	a0,a0,%lo(.LANCHOR0)
	sw	a4,8(a0)
	sw	a5,12(a0)
	addi	a0,a0,288
	call	dtime
	li	a5,1
	bgt	s3,a5,.L32
	j	.L7
.L34:
	li	s6,0
	li	s7,0
	j	.L16
.L35:
	li	s0,0
	li	s1,0
	j	.L18
.L36:
	li	s4,0
	li	s5,0
	j	.L20
.L37:
	li	s4,0
	li	s5,0
	j	.L22
.L38:
	li	s0,0
	li	s1,0
	j	.L24
.L39:
	li	s6,0
	li	s7,0
	j	.L26
.L29:
	lui	a0,%hi(.LANCHOR0)
	addi	a0,a0,%lo(.LANCHOR0)
	sw	zero,40(a0)
	sw	zero,44(a0)
	fcvt.d.w	s2,s2
	lui	a5,%hi(sc)
	sw	s2,%lo(sc)(a5)
	sw	s3,%lo(sc+4)(a5)
	addi	a0,a0,288
	call	dtime
	li	s2,0
	li	s3,0
	mv	s8,s2
	mv	s9,s3
	mv	s6,s2
	mv	s7,s3
	j	.L28
.L11:
	lui	s0,%hi(.LANCHOR0)
	addi	s0,s0,%lo(.LANCHOR0)
	addi	a0,s0,288
	call	dtime
	lw	a4,8(s0)
	lw	a5,12(s0)
	lw	a2,296(s0)
	lw	a3,300(s0)
	fmul.d	a4,a4,a2
	li	a2,0
	li	a3,0
	flt.d	a2,a4,a2
	bne	a2,zero,.L29
	lui	a0,%hi(.LANCHOR0)
	addi	a0,a0,%lo(.LANCHOR0)
	sw	a4,40(a0)
	sw	a5,44(a0)
	fcvt.d.w	s2,s2
	lui	a5,%hi(sc)
	sw	s2,%lo(sc)(a5)
	sw	s3,%lo(sc+4)(a5)
	addi	a0,a0,288
	call	dtime
	li	s2,0
	li	s3,0
	mv	s8,s2
	mv	s9,s3
	mv	s6,s2
	mv	s7,s3
	j	.L28
.L13:
	lui	a0,%hi(.LANCHOR0)
	addi	a0,a0,%lo(.LANCHOR0)
	sw	zero,40(a0)
	sw	zero,44(a0)
	fcvt.d.w	a4,s2
	lui	a3,%hi(sc)
	sw	a4,%lo(sc)(a3)
	sw	a5,%lo(sc+4)(a3)
	lui	a5,%hi(sa)
	lw	s4,%lo(sa)(a5)
	lw	s5,%lo(sa+4)(a5)
	addi	a0,a0,288
	call	dtime
	j	.L31
.L3:
	lui	s0,%hi(.LANCHOR0)
	addi	s0,s0,%lo(.LANCHOR0)
	addi	a0,s0,288
	call	dtime
	lw	a2,296(s0)
	lw	a3,300(s0)
	lui	a5,%hi(sa)
	sw	a2,%lo(sa)(a5)
	sw	a3,%lo(sa+4)(a5)
	li	s8,0
	li	s9,0
.L33:
	lw	a5,16(sp)
	sw	a5,0(sp)
.L2:
	lui	a5,%hi(sa)
	lw	a4,%lo(sa)(a5)
	lw	a5,%lo(sa+4)(a5)
	lui	a3,%hi(TLimit)
	lw	a2,%lo(TLimit)(a3)
	lw	a3,%lo(TLimit+4)(a3)
	flt.d	a4,a4,a2
	beq	a4,zero,.L54
	lw	s8,0(sp)
	slli	s2,s8,1
	sw	s2,16(sp)
	lui	a5,%hi(one)
	lw	s0,%lo(one)(a5)
	lw	s1,%lo(one+4)(a5)
	fcvt.d.w	s10,s2
	fdiv.d	s10,s0,s10
	lw	a0,8(sp)
	call	dtime
	li	a5,1
	ble	s2,a5,.L3
	lui	a5,%hi(D3)
	lw	t5,%lo(D3)(a5)
	lw	t6,%lo(D3+4)(a5)
	lui	a5,%hi(D2)
	lw	s4,%lo(D2)(a5)
	lw	s5,%lo(D2+4)(a5)
	lui	a5,%hi(D1)
	lw	a6,%lo(D1)(a5)
	lw	a7,%lo(D1+4)(a5)
	lui	a5,%hi(E3)
	lw	s6,%lo(E3)(a5)
	lw	s7,%lo(E3+4)(a5)
	lui	a5,%hi(E2)
	lw	s2,%lo(E2)(a5)
	lw	s3,%lo(E2+4)(a5)
	slli	t4,s8,1
	li	t3,1
	li	a0,0
	li	a1,0
	mv	s8,a0
	mv	s9,a1
.L4:
	fadd.d	a0,a0,s0
	fmul.d	t1,s10,a0
	fmul.d	a2,t5,t1
	fadd.d	a2,a2,s4
	fmul.d	a2,a2,t1
	fadd.d	a2,a2,a6
	fmul.d	a4,s6,t1
	fadd.d	a4,a4,s2
	fmul.d	a4,a4,t1
	fadd.d	a4,a4,a6
	fmul.d	a4,a4,t1
	fadd.d	a4,a4,s0
	fdiv.d	a2,a2,a4
	fadd.d	s8,s8,a2
	addi	t3,t3,1
	bne	t3,t4,.L4
	lw	a0,8(sp)
	call	dtime
	lw	a5,24(sp)
	lw	a4,296(a5)
	lw	a5,300(a5)
	lui	a3,%hi(sa)
	sw	a4,%lo(sa)(a3)
	sw	a5,%lo(sa+4)(a3)
	li	a5,256000000
	lw	a4,0(sp)
	bne	a4,a5,.L33
	lw	s3,16(sp)
	fcvt.d.w	a2,s3
	lui	a5,%hi(.LC11)
	lw	a4,%lo(.LC11)(a5)
	lw	a5,%lo(.LC11+4)(a5)
	fdiv.d	a4,a4,a2
	lui	a3,%hi(scale)
	sw	a4,%lo(scale)(a3)
	sw	a5,%lo(scale+4)(a3)
	lui	a0,%hi(.LANCHOR0)
	addi	a0,a0,%lo(.LANCHOR0)
	sw	a4,8(a0)
	sw	a5,12(a0)
	addi	a0,a0,288
	call	dtime
.L32:
	li	a5,1
.L8:
	addi	a5,a5,1
	bne	a5,s3,.L8
.L7:
	lui	s0,%hi(.LANCHOR0)
	addi	s0,s0,%lo(.LANCHOR0)
	addi	a0,s0,288
	call	dtime
	lw	a2,8(s0)
	lw	a3,12(s0)
	lw	a4,296(s0)
	lw	a5,300(s0)
	fmul.d	s0,a2,a4
	li	a0,0
	li	a1,0
	flt.d	a0,s0,a0
	mv	a4,s0
	mv	a5,s1
	beq	a0,zero,.L10
	li	a4,0
	li	a5,0
.L10:
	sw	a4,32(sp)
	sw	a5,36(sp)
	lui	a5,%hi(nulltime)
	lw	a0,32(sp)
	lw	a1,36(sp)
	sw	a0,%lo(nulltime)(a5)
	sw	a1,%lo(nulltime+4)(a5)
	lui	s4,%hi(sa)
	lw	a4,%lo(sa)(s4)
	lw	a5,%lo(sa+4)(s4)
	fmul.d	a4,a2,a4
	lui	a3,%hi(nulltime)
	lw	a2,%lo(nulltime)(a3)
	lw	a3,%lo(nulltime+4)(a3)
	fsub.d	a4,a4,a2
	lui	s3,%hi(.LANCHOR0)
	addi	s3,s3,%lo(.LANCHOR0)
	sw	a4,16(s3)
	sw	a5,20(s3)
	lui	a3,%hi(D1)
	lw	a6,%lo(D1)(a3)
	lw	a7,%lo(D1+4)(a3)
	lui	s5,%hi(one)
	lw	a0,%lo(one)(s5)
	lw	a1,%lo(one+4)(s5)
	lui	a3,%hi(D2)
	lw	t1,%lo(D2)(a3)
	lw	t2,%lo(D2+4)(a3)
	fadd.d	t1,a6,t1
	lui	a3,%hi(D3)
	lw	a2,%lo(D3)(a3)
	lw	a3,%lo(D3+4)(a3)
	fadd.d	t1,t1,a2
	fadd.d	t3,a6,a0
	lui	a3,%hi(E2)
	lw	a2,%lo(E2)(a3)
	lw	a3,%lo(E2+4)(a3)
	fadd.d	a2,t3,a2
	lui	t3,%hi(E3)
	lw	t4,%lo(E3+4)(t3)
	lw	t3,%lo(E3)(t3)
	fadd.d	a2,a2,t3
	fdiv.d	a2,t1,a2
	lui	t1,%hi(.LC12)
	lw	t2,%lo(.LC12+4)(t1)
	lw	t1,%lo(.LC12)(t1)
	fdiv.d	t1,a4,t1
	sw	t1,24(s3)
	sw	t2,28(s3)
	lui	t3,%hi(two)
	lw	t4,%lo(two+4)(t3)
	lw	t3,%lo(two)(t3)
	fadd.d	a2,a6,a2
	fmul.d	s8,t3,s8
	fadd.d	a2,a2,s8
	fmul.d	a2,a2,s10
	fdiv.d	a2,a2,t3
	sw	a2,%lo(sa)(s4)
	sw	a3,%lo(sa+4)(s4)
	fdiv.d	a2,a0,a2
	lui	a6,%hi(sb)
	sw	a2,%lo(sb)(a6)
	sw	a3,%lo(sb+4)(a6)
	fcvt.w.d a6,a2,rtz
	li	a7,40960
	addi	a7,a7,-960
	mul	a6,a6,a7
	fcvt.d.w	a6,a6
	lui	t3,%hi(scale)
	lw	t4,%lo(scale+4)(t3)
	lw	t3,%lo(scale)(t3)
	fdiv.d	a6,a6,t3
	fcvt.w.d s2,a6,rtz
	lui	a6,%hi(.LC13)
	lw	a7,%lo(.LC13+4)(a6)
	lw	a6,%lo(.LC13)(a6)
	fsub.d	a2,a2,a6
	lui	a6,%hi(sc)
	sw	a2,%lo(sc)(a6)
	sw	a3,%lo(sc+4)(a6)
	fdiv.d	a6,a0,t1
	sw	a6,32(s3)
	sw	a7,36(s3)
	lui	a0,%hi(.LC14)
	addi	a0,a0,%lo(.LC14)
	call	printf
	lui	a5,%hi(five)
	lw	s0,%lo(five)(a5)
	lw	s1,%lo(five+4)(a5)
	fneg.d	s0,s0
	lw	a4,%lo(one)(s5)
	lw	a5,%lo(one+4)(s5)
	fneg.d	a4,a4
	sw	a4,%lo(sa)(s4)
	sw	a5,%lo(sa+4)(s4)
	addi	a0,s3,288
	call	dtime
	ble	s2,zero,.L11
	lw	a0,%lo(sa)(s4)
	lw	a1,%lo(sa+4)(s4)
	addi	a2,s2,1
	li	a3,1
.L12:
	mv	a4,s0
	mv	a5,s1
	fsub.d	a0,a0,a4
	fneg.d	s0,s0
	addi	a3,a3,1
	bne	a3,a2,.L12
	lui	a5,%hi(sa)
	sw	a0,%lo(sa)(a5)
	sw	a1,%lo(sa+4)(a5)
	lui	s3,%hi(.LANCHOR0)
	addi	s3,s3,%lo(.LANCHOR0)
	addi	a0,s3,288
	call	dtime
	lw	a4,8(s3)
	lw	a5,12(s3)
	lw	a2,296(s3)
	lw	a3,300(s3)
	fmul.d	a4,a4,a2
	li	a2,0
	li	a3,0
	flt.d	a2,a4,a2
	bne	a2,zero,.L13
	lui	a0,%hi(.LANCHOR0)
	addi	a0,a0,%lo(.LANCHOR0)
	sw	a4,40(a0)
	sw	a5,44(a0)
	fcvt.d.w	a4,s2
	lui	a3,%hi(sc)
	sw	a4,%lo(sc)(a3)
	sw	a5,%lo(sc+4)(a3)
	lui	a5,%hi(sa)
	lw	s4,%lo(sa)(a5)
	lw	s5,%lo(sa+4)(a5)
	addi	a0,a0,288
	call	dtime
.L31:
	lui	a5,%hi(sa)
	lw	a2,%lo(sa)(a5)
	lw	a3,%lo(sa+4)(a5)
	lui	a5,%hi(two)
	lw	a6,%lo(two)(a5)
	lw	a7,%lo(two+4)(a5)
	addi	a4,s2,1
	li	a5,1
	li	s2,0
	li	s3,0
	mv	s8,s2
	mv	s9,s3
	mv	s6,s2
	mv	s7,s3
.L15:
	mv	a0,s0
	mv	a1,s1
	fneg.d	s0,s0
	fsub.d	a2,a2,a0
	fadd.d	s4,s4,a6
	fsub.d	a0,s0,s4
	fadd.d	s2,s2,a0
	fmul.d	a0,s0,s4
	fsub.d	s6,s6,a0
	fdiv.d	a0,s0,s4
	fadd.d	s8,s8,a0
	addi	a5,a5,1
	bne	a4,a5,.L15
	lui	a5,%hi(sa)
	sw	a2,%lo(sa)(a5)
	sw	a3,%lo(sa+4)(a5)
.L28:
	lui	s1,%hi(.LANCHOR0)
	addi	s1,s1,%lo(.LANCHOR0)
	addi	s0,s1,288
	mv	a0,s0
	call	dtime
	lw	a4,8(s1)
	lw	a5,12(s1)
	lw	a2,296(s1)
	lw	a3,300(s1)
	fmul.d	a4,a4,a2
	sw	a4,48(s1)
	sw	a5,52(s1)
	lw	a2,40(s1)
	lw	a3,44(s1)
	fsub.d	a4,a4,a2
	lui	a3,%hi(.LC15)
	lw	a6,%lo(.LC15)(a3)
	lw	a7,%lo(.LC15+4)(a3)
	fdiv.d	a6,a4,a6
	sw	a6,56(s1)
	sw	a7,60(s1)
	lui	t1,%hi(sa)
	lw	a2,%lo(sa)(t1)
	lw	a3,%lo(sa+4)(t1)
	fmul.d	a2,s2,a2
	lui	t3,%hi(sc)
	lw	a0,%lo(sc)(t3)
	lw	a1,%lo(sc+4)(t3)
	fdiv.d	a2,a2,a0
	fcvt.w.d s10,a2,rtz
	lui	a3,%hi(five)
	lw	a2,%lo(five)(a3)
	lw	a3,%lo(five+4)(a3)
	lui	a1,%hi(four)
	lw	a0,%lo(four)(a1)
	lw	a1,%lo(four+4)(a1)
	fmul.d	a0,s8,a0
	fdiv.d	a0,a0,a2
	sw	a0,%lo(sa)(t1)
	sw	a1,%lo(sa+4)(t1)
	fdiv.d	a2,a2,s6
	fadd.d	a2,a2,a0
	lui	a1,%hi(sb)
	sw	a2,%lo(sb)(a1)
	sw	a3,%lo(sb+4)(a1)
	lui	a1,%hi(.LC16)
	lw	t1,%lo(.LC16)(a1)
	lw	t2,%lo(.LC16+4)(a1)
	sw	t1,%lo(sc)(t3)
	sw	t2,%lo(sc+4)(t3)
	fmul.d	a0,s6,s6
	fmul.d	a0,a0,s6
	fdiv.d	t1,t1,a0
	fsub.d	t1,a2,t1
	lui	a3,%hi(piprg)
	sw	t1,%lo(piprg)(a3)
	sw	t2,%lo(piprg+4)(a3)
	lui	s2,%hi(piref)
	lw	a2,%lo(piref)(s2)
	lw	a3,%lo(piref+4)(s2)
	fsub.d	a2,t1,a2
	lui	a1,%hi(pierr)
	sw	a2,%lo(pierr)(a1)
	sw	a3,%lo(pierr+4)(a1)
	lui	a1,%hi(one)
	lw	a0,%lo(one)(a1)
	lw	a1,%lo(one+4)(a1)
	fdiv.d	a6,a0,a6
	sw	a6,64(s1)
	sw	a7,68(s1)
	lui	a0,%hi(.LC17)
	addi	a0,a0,%lo(.LC17)
	call	printf
	fcvt.d.w	a2,s10
	sw	a2,56(sp)
	sw	a3,60(sp)
	lui	a5,%hi(three)
	lw	a4,%lo(three)(a5)
	lw	a5,%lo(three+4)(a5)
	fmul.d	a4,a2,a4
	lw	s8,%lo(piref)(s2)
	lw	s9,%lo(piref+4)(s2)
	fdiv.d	s8,s8,a4
	mv	a0,s0
	call	dtime
	li	a5,1
	ble	s10,a5,.L34
	lui	a5,%hi(one)
	lw	t1,%lo(one)(a5)
	lw	t2,%lo(one+4)(a5)
	lui	a5,%hi(A6)
	lw	t3,%lo(A6)(a5)
	lw	t4,%lo(A6+4)(a5)
	lui	a5,%hi(A5)
	lw	t5,%lo(A5)(a5)
	lw	t6,%lo(A5+4)(a5)
	lui	a5,%hi(A4)
	lw	s0,%lo(A4)(a5)
	lw	s1,%lo(A4+4)(a5)
	lui	a5,%hi(A3)
	lw	s2,%lo(A3)(a5)
	lw	s3,%lo(A3+4)(a5)
	lui	a5,%hi(A2)
	lw	s4,%lo(A2)(a5)
	lw	s5,%lo(A2+4)(a5)
	lui	a5,%hi(A1)
	lw	a4,%lo(A1)(a5)
	lw	a5,%lo(A1+4)(a5)
	sw	a4,0(sp)
	sw	a5,4(sp)
	li	t0,1
	li	a6,0
	li	a7,0
	mv	s6,a6
	mv	s7,a7
	sw	t3,8(sp)
	sw	t4,12(sp)
.L17:
	fadd.d	a6,a6,t1
	fmul.d	a0,s8,a6
	fmul.d	a2,a0,a0
	lw	a4,8(sp)
	lw	a5,12(sp)
	fmul.d	a4,a4,a2
	fsub.d	a4,a4,t5
	fmul.d	a4,a4,a2
	fadd.d	a4,a4,s0
	fmul.d	a4,a4,a2
	fsub.d	a4,a4,s2
	fmul.d	a4,a4,a2
	fadd.d	a4,a4,s4
	fmul.d	a4,a4,a2
	lw	t3,0(sp)
	lw	t4,4(sp)
	fadd.d	a4,a4,t3
	fmul.d	a4,a4,a2
	fadd.d	a4,a4,t1
	fmul.d	a4,a4,a0
	fadd.d	s6,s6,a4
	addi	t0,t0,1
	bne	s10,t0,.L17
.L16:
	lui	s0,%hi(.LANCHOR0)
	addi	s0,s0,%lo(.LANCHOR0)
	addi	s4,s0,288
	mv	a0,s4
	call	dtime
	lw	a4,8(s0)
	lw	a5,12(s0)
	lw	a2,296(s0)
	lw	a3,300(s0)
	fmul.d	a4,a4,a2
	lui	a3,%hi(nulltime)
	lw	a2,%lo(nulltime)(a3)
	lw	a3,%lo(nulltime+4)(a3)
	fsub.d	a4,a4,a2
	sw	a4,72(s0)
	sw	a5,76(s0)
	lui	s2,%hi(piref)
	lui	s3,%hi(three)
	lw	a0,%lo(piref)(s2)
	lw	a1,%lo(piref+4)(s2)
	lw	a2,%lo(three)(s3)
	lw	a3,%lo(three+4)(s3)
	fdiv.d	a2,a0,a2
	fmul.d	a6,a2,a2
	lui	a1,%hi(one)
	lw	t3,%lo(one)(a1)
	lw	t4,%lo(one+4)(a1)
	lui	a1,%hi(A6)
	lw	t1,%lo(A6)(a1)
	lw	t2,%lo(A6+4)(a1)
	fmul.d	t1,a6,t1
	lui	s1,%hi(A5)
	lw	a0,%lo(A5)(s1)
	lw	a1,%lo(A5+4)(s1)
	fsub.d	t1,t1,a0
	fmul.d	t1,t1,a6
	lui	a1,%hi(A4)
	lw	a0,%lo(A4)(a1)
	lw	a1,%lo(A4+4)(a1)
	fadd.d	t1,t1,a0
	fmul.d	t1,t1,a6
	lui	s5,%hi(A3)
	lw	a0,%lo(A3)(s5)
	lw	a1,%lo(A3+4)(s5)
	fsub.d	t1,t1,a0
	fmul.d	t1,t1,a6
	lui	a1,%hi(A2)
	lw	a0,%lo(A2)(a1)
	lw	a1,%lo(A2+4)(a1)
	fadd.d	t1,t1,a0
	fmul.d	t1,t1,a6
	lui	a1,%hi(A1)
	lw	a0,%lo(A1)(a1)
	lw	a1,%lo(A1+4)(a1)
	fadd.d	a0,t1,a0
	fmul.d	a0,a0,a6
	fadd.d	a0,a0,t3
	fmul.d	t1,a0,a2
	lui	a3,%hi(.LC18)
	lw	a0,%lo(.LC18)(a3)
	lw	a1,%lo(.LC18+4)(a3)
	fdiv.d	a0,a4,a0
	sw	a0,80(s0)
	sw	a1,84(s0)
	lui	a3,%hi(two)
	lw	a6,%lo(two)(a3)
	lw	a7,%lo(two+4)(a3)
	fmul.d	a2,a6,s6
	fadd.d	a2,a2,t1
	fmul.d	a2,a2,s8
	fdiv.d	a2,a2,a6
	lui	a6,%hi(sa)
	sw	a2,%lo(sa)(a6)
	sw	a3,%lo(sa+4)(a6)
	lui	a6,%hi(.LC19)
	lw	a7,%lo(.LC19+4)(a6)
	lw	a6,%lo(.LC19)(a6)
	lui	t1,%hi(sb)
	sw	a6,%lo(sb)(t1)
	sw	a7,%lo(sb+4)(t1)
	fsub.d	a2,a2,a6
	lui	a6,%hi(sc)
	sw	a2,%lo(sc)(a6)
	sw	a3,%lo(sc+4)(a6)
	fdiv.d	a6,t3,a0
	sw	a6,88(s0)
	sw	a7,92(s0)
	lui	a0,%hi(.LC20)
	addi	a0,a0,%lo(.LC20)
	call	printf
	lw	a4,%lo(A3)(s5)
	lw	a5,%lo(A3+4)(s5)
	fneg.d	a4,a4
	sw	a4,%lo(A3)(s5)
	sw	a5,%lo(A3+4)(s5)
	lw	a4,%lo(A5)(s1)
	lw	a5,%lo(A5+4)(s1)
	fneg.d	a4,a4
	sw	a4,%lo(A5)(s1)
	sw	a5,%lo(A5+4)(s1)
	lw	a4,%lo(three)(s3)
	lw	a5,%lo(three+4)(s3)
	lw	a2,56(sp)
	lw	a3,60(sp)
	fmul.d	a4,a2,a4
	lw	s3,%lo(piref+4)(s2)
	lw	s2,%lo(piref)(s2)
	fdiv.d	s2,s2,a4
	mv	a0,s4
	call	dtime
	li	a5,1
	ble	s10,a5,.L35
	lui	a5,%hi(B6)
	lw	t5,%lo(B6)(a5)
	lw	t6,%lo(B6+4)(a5)
	lui	a5,%hi(B5)
	lw	t3,%lo(B5)(a5)
	lw	t4,%lo(B5+4)(a5)
	lui	a5,%hi(B4)
	lw	t1,%lo(B4)(a5)
	lw	t2,%lo(B4+4)(a5)
	lui	a5,%hi(B3)
	lw	a6,%lo(B3)(a5)
	lw	a7,%lo(B3+4)(a5)
	lui	a5,%hi(B2)
	lw	a0,%lo(B2)(a5)
	lw	a1,%lo(B2+4)(a5)
	lui	a5,%hi(B1)
	lw	s4,%lo(B1)(a5)
	lw	s5,%lo(B1+4)(a5)
	lui	a5,%hi(one)
	lw	s6,%lo(one)(a5)
	lw	s7,%lo(one+4)(a5)
	li	t0,1
	li	s0,0
	li	s1,0
.L19:
	fcvt.d.w	a2,t0
	fmul.d	a2,a2,s2
	fmul.d	a2,a2,a2
	fmul.d	a4,t5,a2
	fadd.d	a4,a4,t3
	fmul.d	a4,a4,a2
	fadd.d	a4,a4,t1
	fmul.d	a4,a4,a2
	fadd.d	a4,a4,a6
	fmul.d	a4,a4,a2
	fadd.d	a4,a4,a0
	fmul.d	a4,a4,a2
	fadd.d	a4,a4,s4
	fmul.d	a4,a4,a2
	fadd.d	s0,a4,s0
	fadd.d	s0,s0,s6
	addi	t0,t0,1
	bne	s10,t0,.L19
.L18:
	lui	s4,%hi(.LANCHOR0)
	addi	s4,s4,%lo(.LANCHOR0)
	addi	s5,s4,288
	mv	a0,s5
	call	dtime
	lw	a4,8(s4)
	lw	a5,12(s4)
	lw	a2,296(s4)
	lw	a3,300(s4)
	fmul.d	a4,a4,a2
	lui	a3,%hi(nulltime)
	lw	a2,%lo(nulltime)(a3)
	lw	a3,%lo(nulltime+4)(a3)
	fsub.d	a4,a4,a2
	sw	a4,96(s4)
	sw	a5,100(s4)
	lui	s6,%hi(piref)
	lui	s7,%hi(three)
	lw	a0,%lo(piref)(s6)
	lw	a1,%lo(piref+4)(s6)
	lw	t1,%lo(three)(s7)
	lw	t2,%lo(three+4)(s7)
	fdiv.d	t1,a0,t1
	fmul.d	a0,t1,t1
	lui	a3,%hi(one)
	lw	a6,%lo(one)(a3)
	lw	a7,%lo(one+4)(a3)
	lui	a3,%hi(B6)
	lw	t3,%lo(B6)(a3)
	lw	t4,%lo(B6+4)(a3)
	fmul.d	t3,a0,t3
	lui	a3,%hi(B5)
	lw	a2,%lo(B5)(a3)
	lw	a3,%lo(B5+4)(a3)
	fadd.d	t3,t3,a2
	fmul.d	t3,t3,a0
	lui	a3,%hi(B4)
	lw	a2,%lo(B4)(a3)
	lw	a3,%lo(B4+4)(a3)
	fadd.d	t3,t3,a2
	fmul.d	t3,t3,a0
	lui	a3,%hi(B3)
	lw	a2,%lo(B3)(a3)
	lw	a3,%lo(B3+4)(a3)
	fadd.d	t3,t3,a2
	fmul.d	t3,t3,a0
	lui	a3,%hi(B2)
	lw	a2,%lo(B2)(a3)
	lw	a3,%lo(B2+4)(a3)
	fadd.d	t3,t3,a2
	fmul.d	t3,t3,a0
	lui	a3,%hi(B1)
	lw	a2,%lo(B1)(a3)
	lw	a3,%lo(B1+4)(a3)
	fadd.d	a2,t3,a2
	fmul.d	a2,a2,a0
	fadd.d	s8,a2,a6
	lui	a3,%hi(.LC2)
	lw	t5,%lo(.LC2)(a3)
	lw	t6,%lo(.LC2+4)(a3)
	fdiv.d	t5,a4,t5
	sw	t5,104(s4)
	sw	t6,108(s4)
	lui	a3,%hi(two)
	lw	t3,%lo(two)(a3)
	lw	t4,%lo(two+4)(a3)
	fadd.d	s8,a6,s8
	fmul.d	a2,t3,s0
	fadd.d	a2,s8,a2
	fmul.d	a2,a2,s2
	fdiv.d	a2,a2,t3
	lui	t3,%hi(sa)
	sw	a2,%lo(sa)(t3)
	sw	a3,%lo(sa+4)(t3)
	lui	t3,%hi(A6)
	lw	s0,%lo(A6)(t3)
	lw	s1,%lo(A6+4)(t3)
	fmul.d	s0,a0,s0
	lui	t3,%hi(A5)
	lw	t4,%lo(A5+4)(t3)
	lw	t3,%lo(A5)(t3)
	fadd.d	s0,s0,t3
	fmul.d	s0,s0,a0
	lui	t3,%hi(A4)
	lw	t4,%lo(A4+4)(t3)
	lw	t3,%lo(A4)(t3)
	fadd.d	s0,s0,t3
	fmul.d	s0,s0,a0
	lui	t3,%hi(A3)
	lw	t4,%lo(A3+4)(t3)
	lw	t3,%lo(A3)(t3)
	fadd.d	s0,s0,t3
	fmul.d	s0,s0,a0
	lui	t3,%hi(A2)
	lw	t4,%lo(A2+4)(t3)
	lw	t3,%lo(A2)(t3)
	fadd.d	s0,s0,t3
	fmul.d	s0,s0,a0
	lui	t3,%hi(A1)
	lw	t4,%lo(A1+4)(t3)
	lw	t3,%lo(A1)(t3)
	fadd.d	t3,s0,t3
	fmul.d	t3,t3,a0
	lui	a1,%hi(A0)
	lw	a0,%lo(A0)(a1)
	lw	a1,%lo(A0+4)(a1)
	fadd.d	t3,t3,a0
	fmul.d	t3,t3,t1
	lui	a1,%hi(sb)
	sw	t3,%lo(sb)(a1)
	sw	t4,%lo(sb+4)(a1)
	fsub.d	a2,a2,t3
	lui	a1,%hi(sc)
	sw	a2,%lo(sc)(a1)
	sw	a3,%lo(sc+4)(a1)
	fdiv.d	a6,a6,t5
	sw	a6,112(s4)
	sw	a7,116(s4)
	lui	a0,%hi(.LC21)
	addi	a0,a0,%lo(.LC21)
	call	printf
	lw	a4,%lo(three)(s7)
	lw	a5,%lo(three+4)(s7)
	lw	a2,56(sp)
	lw	a3,60(sp)
	fmul.d	a4,a2,a4
	lw	s7,%lo(piref+4)(s6)
	lw	s6,%lo(piref)(s6)
	fdiv.d	s6,s6,a4
	mv	a0,s5
	call	dtime
	li	a5,1
	ble	s10,a5,.L36
	lui	a5,%hi(A6)
	lw	t3,%lo(A6)(a5)
	lw	t4,%lo(A6+4)(a5)
	lui	a5,%hi(A5)
	lw	t1,%lo(A5)(a5)
	lw	t2,%lo(A5+4)(a5)
	lui	a5,%hi(A4)
	lw	t5,%lo(A4)(a5)
	lw	t6,%lo(A4+4)(a5)
	lui	a5,%hi(A3)
	lw	s0,%lo(A3)(a5)
	lw	s1,%lo(A3+4)(a5)
	lui	a5,%hi(A2)
	lw	s2,%lo(A2)(a5)
	lw	s3,%lo(A2+4)(a5)
	lui	a5,%hi(A1)
	lw	s8,%lo(A1)(a5)
	lw	s9,%lo(A1+4)(a5)
	lui	a5,%hi(one)
	lw	a6,%lo(one)(a5)
	lw	a7,%lo(one+4)(a5)
	lui	a5,%hi(B6)
	lw	a4,%lo(B6)(a5)
	lw	a5,%lo(B6+4)(a5)
	sw	a4,0(sp)
	sw	a5,4(sp)
	lui	a5,%hi(B5)
	lw	a4,%lo(B5)(a5)
	lw	a5,%lo(B5+4)(a5)
	sw	a4,8(sp)
	sw	a5,12(sp)
	lui	a5,%hi(B4)
	lw	a4,%lo(B4)(a5)
	lw	a5,%lo(B4+4)(a5)
	sw	a4,16(sp)
	sw	a5,20(sp)
	lui	a5,%hi(B3)
	lw	a4,%lo(B3)(a5)
	lw	a5,%lo(B3+4)(a5)
	sw	a4,24(sp)
	sw	a5,28(sp)
	lui	a5,%hi(B2)
	lw	a4,%lo(B2)(a5)
	lw	a5,%lo(B2+4)(a5)
	sw	a4,32(sp)
	sw	a5,36(sp)
	lui	a5,%hi(B1)
	lw	a4,%lo(B1)(a5)
	lw	a5,%lo(B1+4)(a5)
	sw	a4,40(sp)
	sw	a5,44(sp)
	li	t0,1
	li	s4,0
	li	s5,0
	sw	t1,48(sp)
	sw	t2,52(sp)
.L21:
	fcvt.d.w	a2,t0
	fmul.d	a2,a2,s6
	fmul.d	a0,a2,a2
	fmul.d	a4,t3,a0
	lw	t1,48(sp)
	lw	t2,52(sp)
	fadd.d	a4,a4,t1
	fmul.d	a4,a4,a0
	fadd.d	a4,a4,t5
	fmul.d	a4,a4,a0
	fadd.d	a4,a4,s0
	fmul.d	a4,a4,a0
	fadd.d	a4,a4,s2
	fmul.d	a4,a4,a0
	fadd.d	a4,a4,s8
	fmul.d	a4,a4,a0
	fadd.d	a4,a4,a6
	fmul.d	a4,a4,a2
	lw	a2,0(sp)
	lw	a3,4(sp)
	fmul.d	a2,a2,a0
	lw	t1,8(sp)
	lw	t2,12(sp)
	fadd.d	a2,a2,t1
	fmul.d	a2,a2,a0
	lw	t1,16(sp)
	lw	t2,20(sp)
	fadd.d	a2,a2,t1
	fmul.d	a2,a2,a0
	lw	t1,24(sp)
	lw	t2,28(sp)
	fadd.d	a2,a2,t1
	fmul.d	a2,a2,a0
	lw	t1,32(sp)
	lw	t2,36(sp)
	fadd.d	a2,a2,t1
	fmul.d	a2,a2,a0
	lw	t1,40(sp)
	lw	t2,44(sp)
	fadd.d	a2,a2,t1
	fmul.d	a2,a2,a0
	fadd.d	a2,a2,a6
	fdiv.d	a4,a4,a2
	fadd.d	s4,s4,a4
	addi	t0,t0,1
	bne	s10,t0,.L21
.L20:
	lui	s0,%hi(.LANCHOR0)
	addi	s0,s0,%lo(.LANCHOR0)
	addi	s1,s0,288
	mv	a0,s1
	call	dtime
	lw	a4,8(s0)
	lw	a5,12(s0)
	lw	a2,296(s0)
	lw	a3,300(s0)
	fmul.d	a4,a4,a2
	lui	a3,%hi(nulltime)
	lw	a2,%lo(nulltime)(a3)
	lw	a3,%lo(nulltime+4)(a3)
	fsub.d	a4,a4,a2
	sw	a4,120(s0)
	sw	a5,124(s0)
	lui	s2,%hi(piref)
	lw	t3,%lo(piref)(s2)
	lw	t4,%lo(piref+4)(s2)
	lui	a3,%hi(three)
	lw	a2,%lo(three)(a3)
	lw	a3,%lo(three+4)(a3)
	fdiv.d	t3,t3,a2
	fmul.d	a6,t3,t3
	lui	a3,%hi(one)
	lw	t1,%lo(one)(a3)
	lw	t2,%lo(one+4)(a3)
	lui	a3,%hi(A6)
	lw	a2,%lo(A6)(a3)
	lw	a3,%lo(A6+4)(a3)
	fmul.d	a2,a6,a2
	lui	a1,%hi(A5)
	lw	a0,%lo(A5)(a1)
	lw	a1,%lo(A5+4)(a1)
	fadd.d	a2,a2,a0
	fmul.d	a2,a2,a6
	lui	a1,%hi(A4)
	lw	a0,%lo(A4)(a1)
	lw	a1,%lo(A4+4)(a1)
	fadd.d	a2,a2,a0
	fmul.d	a2,a2,a6
	lui	a1,%hi(A3)
	lw	a0,%lo(A3)(a1)
	lw	a1,%lo(A3+4)(a1)
	fadd.d	a2,a2,a0
	fmul.d	a2,a2,a6
	lui	a1,%hi(A2)
	lw	a0,%lo(A2)(a1)
	lw	a1,%lo(A2+4)(a1)
	fadd.d	a2,a2,a0
	fmul.d	a2,a2,a6
	lui	a1,%hi(A1)
	lw	a0,%lo(A1)(a1)
	lw	a1,%lo(A1+4)(a1)
	fadd.d	a0,a2,a0
	fmul.d	a0,a0,a6
	fadd.d	a0,a0,t1
	fmul.d	a0,a0,t3
	lui	a3,%hi(B6)
	lw	a2,%lo(B6)(a3)
	lw	a3,%lo(B6+4)(a3)
	fmul.d	a2,a6,a2
	lui	t3,%hi(B5)
	lw	t4,%lo(B5+4)(t3)
	lw	t3,%lo(B5)(t3)
	fadd.d	a2,a2,t3
	fmul.d	a2,a2,a6
	lui	t3,%hi(B4)
	lw	t4,%lo(B4+4)(t3)
	lw	t3,%lo(B4)(t3)
	fadd.d	a2,a2,t3
	fmul.d	a2,a2,a6
	lui	t3,%hi(B3)
	lw	t4,%lo(B3+4)(t3)
	lw	t3,%lo(B3)(t3)
	fadd.d	a2,a2,t3
	fmul.d	a2,a2,a6
	lui	t3,%hi(B2)
	lw	t4,%lo(B2+4)(t3)
	lw	t3,%lo(B2)(t3)
	fadd.d	a2,a2,t3
	fmul.d	a2,a2,a6
	lui	t3,%hi(B1)
	lw	t4,%lo(B1+4)(t3)
	lw	t3,%lo(B1)(t3)
	fadd.d	a2,a2,t3
	fmul.d	a2,a2,a6
	fadd.d	a2,a2,t1
	fdiv.d	a2,a0,a2
	lui	a1,%hi(.LC22)
	lw	a0,%lo(.LC22)(a1)
	lw	a1,%lo(.LC22+4)(a1)
	fdiv.d	a0,a4,a0
	sw	a0,128(s0)
	sw	a1,132(s0)
	lui	a6,%hi(two)
	lw	a7,%lo(two+4)(a6)
	lw	a6,%lo(two)(a6)
	fmul.d	s4,a6,s4
	fadd.d	a2,s4,a2
	fmul.d	a2,a2,s6
	fdiv.d	a2,a2,a6
	lui	a6,%hi(sa)
	sw	a2,%lo(sa)(a6)
	sw	a3,%lo(sa+4)(a6)
	lui	a6,%hi(.LC23)
	lw	a7,%lo(.LC23+4)(a6)
	lw	a6,%lo(.LC23)(a6)
	lui	t3,%hi(sb)
	sw	a6,%lo(sb)(t3)
	sw	a7,%lo(sb+4)(t3)
	fsub.d	a2,a2,a6
	lui	a6,%hi(sc)
	sw	a2,%lo(sc)(a6)
	sw	a3,%lo(sc+4)(a6)
	fdiv.d	a6,t1,a0
	sw	a6,136(s0)
	sw	a7,140(s0)
	lui	a0,%hi(.LC24)
	addi	a0,a0,%lo(.LC24)
	call	printf
	lui	a5,%hi(four)
	lw	a4,%lo(four)(a5)
	lw	a5,%lo(four+4)(a5)
	lw	a2,56(sp)
	lw	a3,60(sp)
	fmul.d	a4,a2,a4
	lw	s6,%lo(piref)(s2)
	lw	s7,%lo(piref+4)(s2)
	fdiv.d	s6,s6,a4
	mv	a0,s1
	call	dtime
	li	a5,1
	ble	s10,a5,.L37
	lui	a5,%hi(A6)
	lw	t3,%lo(A6)(a5)
	lw	t4,%lo(A6+4)(a5)
	lui	a5,%hi(A5)
	lw	t1,%lo(A5)(a5)
	lw	t2,%lo(A5+4)(a5)
	lui	a5,%hi(A4)
	lw	t5,%lo(A4)(a5)
	lw	t6,%lo(A4+4)(a5)
	lui	a5,%hi(A3)
	lw	s0,%lo(A3)(a5)
	lw	s1,%lo(A3+4)(a5)
	lui	a5,%hi(A2)
	lw	s2,%lo(A2)(a5)
	lw	s3,%lo(A2+4)(a5)
	lui	a5,%hi(A1)
	lw	s8,%lo(A1)(a5)
	lw	s9,%lo(A1+4)(a5)
	lui	a5,%hi(one)
	lw	a6,%lo(one)(a5)
	lw	a7,%lo(one+4)(a5)
	lui	a5,%hi(B6)
	lw	a4,%lo(B6)(a5)
	lw	a5,%lo(B6+4)(a5)
	sw	a4,0(sp)
	sw	a5,4(sp)
	lui	a5,%hi(B5)
	lw	a4,%lo(B5)(a5)
	lw	a5,%lo(B5+4)(a5)
	sw	a4,8(sp)
	sw	a5,12(sp)
	lui	a5,%hi(B4)
	lw	a4,%lo(B4)(a5)
	lw	a5,%lo(B4+4)(a5)
	sw	a4,16(sp)
	sw	a5,20(sp)
	lui	a5,%hi(B3)
	lw	a4,%lo(B3)(a5)
	lw	a5,%lo(B3+4)(a5)
	sw	a4,24(sp)
	sw	a5,28(sp)
	lui	a5,%hi(B2)
	lw	a4,%lo(B2)(a5)
	lw	a5,%lo(B2+4)(a5)
	sw	a4,32(sp)
	sw	a5,36(sp)
	lui	a5,%hi(B1)
	lw	a4,%lo(B1)(a5)
	lw	a5,%lo(B1+4)(a5)
	sw	a4,40(sp)
	sw	a5,44(sp)
	li	t0,1
	li	s4,0
	li	s5,0
	sw	t1,48(sp)
	sw	t2,52(sp)
.L23:
	fcvt.d.w	a2,t0
	fmul.d	a2,a2,s6
	fmul.d	a0,a2,a2
	fmul.d	a4,t3,a0
	lw	t1,48(sp)
	lw	t2,52(sp)
	fadd.d	a4,a4,t1
	fmul.d	a4,a4,a0
	fadd.d	a4,a4,t5
	fmul.d	a4,a4,a0
	fadd.d	a4,a4,s0
	fmul.d	a4,a4,a0
	fadd.d	a4,a4,s2
	fmul.d	a4,a4,a0
	fadd.d	a4,a4,s8
	fmul.d	a4,a4,a0
	fadd.d	a4,a4,a6
	fmul.d	a4,a4,a2
	lw	a2,0(sp)
	lw	a3,4(sp)
	fmul.d	a2,a2,a0
	lw	t1,8(sp)
	lw	t2,12(sp)
	fadd.d	a2,a2,t1
	fmul.d	a2,a2,a0
	lw	t1,16(sp)
	lw	t2,20(sp)
	fadd.d	a2,a2,t1
	fmul.d	a2,a2,a0
	lw	t1,24(sp)
	lw	t2,28(sp)
	fadd.d	a2,a2,t1
	fmul.d	a2,a2,a0
	lw	t1,32(sp)
	lw	t2,36(sp)
	fadd.d	a2,a2,t1
	fmul.d	a2,a2,a0
	lw	t1,40(sp)
	lw	t2,44(sp)
	fadd.d	a2,a2,t1
	fmul.d	a2,a2,a0
	fadd.d	a2,a2,a6
	fmul.d	a4,a4,a2
	fadd.d	s4,s4,a4
	addi	t0,t0,1
	bne	s10,t0,.L23
.L22:
	lui	s1,%hi(.LANCHOR0)
	addi	s1,s1,%lo(.LANCHOR0)
	addi	s0,s1,288
	mv	a0,s0
	call	dtime
	lw	a4,8(s1)
	lw	a5,12(s1)
	lw	a2,296(s1)
	lw	a3,300(s1)
	fmul.d	a4,a4,a2
	lui	a3,%hi(nulltime)
	lw	a2,%lo(nulltime)(a3)
	lw	a3,%lo(nulltime+4)(a3)
	fsub.d	a4,a4,a2
	sw	a4,144(s1)
	sw	a5,148(s1)
	lui	a3,%hi(piref)
	lw	t3,%lo(piref)(a3)
	lw	t4,%lo(piref+4)(a3)
	lui	a3,%hi(four)
	lw	a2,%lo(four)(a3)
	lw	a3,%lo(four+4)(a3)
	fdiv.d	t3,t3,a2
	fmul.d	a6,t3,t3
	lui	s2,%hi(one)
	lw	t1,%lo(one)(s2)
	lw	t2,%lo(one+4)(s2)
	lui	a3,%hi(A6)
	lw	a2,%lo(A6)(a3)
	lw	a3,%lo(A6+4)(a3)
	fmul.d	a2,a6,a2
	lui	a1,%hi(A5)
	lw	a0,%lo(A5)(a1)
	lw	a1,%lo(A5+4)(a1)
	fadd.d	a2,a2,a0
	fmul.d	a2,a2,a6
	lui	a1,%hi(A4)
	lw	a0,%lo(A4)(a1)
	lw	a1,%lo(A4+4)(a1)
	fadd.d	a2,a2,a0
	fmul.d	a2,a2,a6
	lui	a1,%hi(A3)
	lw	a0,%lo(A3)(a1)
	lw	a1,%lo(A3+4)(a1)
	fadd.d	a2,a2,a0
	fmul.d	a2,a2,a6
	lui	a1,%hi(A2)
	lw	a0,%lo(A2)(a1)
	lw	a1,%lo(A2+4)(a1)
	fadd.d	a2,a2,a0
	fmul.d	a2,a2,a6
	lui	a1,%hi(A1)
	lw	a0,%lo(A1)(a1)
	lw	a1,%lo(A1+4)(a1)
	fadd.d	a0,a2,a0
	fmul.d	a0,a0,a6
	fadd.d	a0,a0,t1
	fmul.d	a0,a0,t3
	lui	a3,%hi(B6)
	lw	a2,%lo(B6)(a3)
	lw	a3,%lo(B6+4)(a3)
	fmul.d	a2,a6,a2
	lui	t3,%hi(B5)
	lw	t4,%lo(B5+4)(t3)
	lw	t3,%lo(B5)(t3)
	fadd.d	a2,a2,t3
	fmul.d	a2,a2,a6
	lui	t3,%hi(B4)
	lw	t4,%lo(B4+4)(t3)
	lw	t3,%lo(B4)(t3)
	fadd.d	a2,a2,t3
	fmul.d	a2,a2,a6
	lui	t3,%hi(B3)
	lw	t4,%lo(B3+4)(t3)
	lw	t3,%lo(B3)(t3)
	fadd.d	a2,a2,t3
	fmul.d	a2,a2,a6
	lui	t3,%hi(B2)
	lw	t4,%lo(B2+4)(t3)
	lw	t3,%lo(B2)(t3)
	fadd.d	a2,a2,t3
	fmul.d	a2,a2,a6
	lui	t3,%hi(B1)
	lw	t4,%lo(B1+4)(t3)
	lw	t3,%lo(B1)(t3)
	fadd.d	a2,a2,t3
	fmul.d	a2,a2,a6
	fadd.d	a2,a2,t1
	fmul.d	a2,a0,a2
	lui	a1,%hi(.LC22)
	lw	a0,%lo(.LC22)(a1)
	lw	a1,%lo(.LC22+4)(a1)
	fdiv.d	a0,a4,a0
	sw	a0,152(s1)
	sw	a1,156(s1)
	lui	a6,%hi(two)
	lw	a7,%lo(two+4)(a6)
	lw	a6,%lo(two)(a6)
	fmul.d	s4,a6,s4
	fadd.d	a2,s4,a2
	fmul.d	a2,a2,s6
	fdiv.d	a2,a2,a6
	lui	s5,%hi(sa)
	sw	a2,%lo(sa)(s5)
	sw	a3,%lo(sa+4)(s5)
	lui	a6,%hi(.LC25)
	lw	a7,%lo(.LC25+4)(a6)
	lw	a6,%lo(.LC25)(a6)
	lui	t3,%hi(sb)
	sw	a6,%lo(sb)(t3)
	sw	a7,%lo(sb+4)(t3)
	fsub.d	a2,a2,a6
	lui	a6,%hi(sc)
	sw	a2,%lo(sc)(a6)
	sw	a3,%lo(sc+4)(a6)
	fdiv.d	a6,t1,a0
	sw	a6,160(s1)
	sw	a7,164(s1)
	lui	a0,%hi(.LC26)
	addi	a0,a0,%lo(.LC26)
	call	printf
	lw	s3,%lo(one+4)(s2)
	lw	s2,%lo(one)(s2)
	lui	a5,%hi(.LC27)
	lw	a4,%lo(.LC27)(a5)
	lw	a5,%lo(.LC27+4)(a5)
	sw	a4,%lo(sa)(s5)
	sw	a5,%lo(sa+4)(s5)
	lw	a2,56(sp)
	lw	a3,60(sp)
	fdiv.d	s4,a4,a2
	mv	a0,s0
	call	dtime
	li	a5,1
	ble	s10,a5,.L38
	mv	a6,a5
	li	s0,0
	li	s1,0
.L25:
	fcvt.d.w	a4,a6
	fmul.d	a4,a4,s4
	fmul.d	a0,a4,a4
	fadd.d	a2,s2,a4
	fdiv.d	a2,s2,a2
	fsub.d	a2,s0,a2
	fadd.d	s0,s2,a0
	fdiv.d	s0,a4,s0
	fsub.d	s0,a2,s0
	fmul.d	a4,a4,a0
	fadd.d	a4,a4,s2
	fdiv.d	a0,a0,a4
	fsub.d	s0,s0,a0
	addi	a6,a6,1
	bne	s10,a6,.L25
.L24:
	lui	s6,%hi(.LANCHOR0)
	addi	s6,s6,%lo(.LANCHOR0)
	addi	s7,s6,288
	mv	a0,s7
	call	dtime
	lw	a4,8(s6)
	lw	a5,12(s6)
	lw	a2,296(s6)
	lw	a3,300(s6)
	fmul.d	a4,a4,a2
	lui	a3,%hi(nulltime)
	lw	a2,%lo(nulltime)(a3)
	lw	a3,%lo(nulltime+4)(a3)
	fsub.d	a4,a4,a2
	sw	a4,168(s6)
	sw	a5,172(s6)
	lui	a3,%hi(.LC28)
	lw	a6,%lo(.LC28)(a3)
	lw	a7,%lo(.LC28+4)(a3)
	fdiv.d	a6,a4,a6
	sw	a6,176(s6)
	sw	a7,180(s6)
	lui	t5,%hi(sa)
	lw	a0,%lo(sa)(t5)
	lw	a1,%lo(sa+4)(t5)
	fmul.d	a2,a0,a0
	fneg.d	t3,s2
	fadd.d	t1,s2,a0
	fdiv.d	t1,s2,t1
	fsub.d	t3,t3,t1
	fadd.d	t1,s2,a2
	fdiv.d	t1,a0,t1
	fsub.d	t1,t3,t1
	fmul.d	a0,a0,a2
	fadd.d	a0,a0,s2
	fdiv.d	a2,a2,a0
	fsub.d	a2,t1,a2
	lui	a1,%hi(two)
	lw	a0,%lo(two)(a1)
	lw	a1,%lo(two+4)(a1)
	fmul.d	s0,s0,a0
	fadd.d	a2,a2,s0
	lui	a1,%hi(.LC29)
	lw	a0,%lo(.LC29)(a1)
	lw	a1,%lo(.LC29+4)(a1)
	fmul.d	s4,s4,a0
	fmul.d	a2,a2,s4
	sw	a2,%lo(sa)(t5)
	sw	a3,%lo(sa+4)(t5)
	fcvt.w.d a0,a2,rtz
	li	a1,-2000
	mul	a0,a0,a1
	fcvt.d.w	a0,a0
	lui	t1,%hi(scale)
	lw	t2,%lo(scale+4)(t1)
	lw	t1,%lo(scale)(t1)
	fdiv.d	a0,a0,t1
	fcvt.w.d s4,a0,rtz
	lui	a1,%hi(.LC30)
	lw	a0,%lo(.LC30)(a1)
	lw	a1,%lo(.LC30+4)(a1)
	fadd.d	a2,a2,a0
	lui	a1,%hi(sc)
	sw	a2,%lo(sc)(a1)
	sw	a3,%lo(sc+4)(a1)
	lui	a1,%hi(one)
	lw	a0,%lo(one)(a1)
	lw	a1,%lo(one+4)(a1)
	fdiv.d	a6,a0,a6
	sw	a6,184(s6)
	sw	a7,188(s6)
	lui	a0,%hi(.LC31)
	addi	a0,a0,%lo(.LC31)
	call	printf
	fcvt.d.w	a4,s4
	lui	a3,%hi(three)
	lw	a2,%lo(three)(a3)
	lw	a3,%lo(three+4)(a3)
	fmul.d	a4,a4,a2
	lui	a3,%hi(piref)
	lw	s8,%lo(piref)(a3)
	lw	s9,%lo(piref+4)(a3)
	fdiv.d	s8,s8,a4
	mv	a0,s7
	call	dtime
	li	a5,1
	ble	s4,a5,.L39
	lui	a5,%hi(B6)
	lw	t3,%lo(B6)(a5)
	lw	t4,%lo(B6+4)(a5)
	lui	a5,%hi(B5)
	lw	t5,%lo(B5)(a5)
	lw	t6,%lo(B5+4)(a5)
	lui	a5,%hi(B4)
	lw	s0,%lo(B4)(a5)
	lw	s1,%lo(B4+4)(a5)
	lui	a5,%hi(B3)
	lw	s2,%lo(B3)(a5)
	lw	s3,%lo(B3+4)(a5)
	lui	a5,%hi(B2)
	lw	s10,%lo(B2)(a5)
	lw	s11,%lo(B2+4)(a5)
	lui	a5,%hi(B1)
	lw	a4,%lo(B1)(a5)
	lw	a5,%lo(B1+4)(a5)
	sw	a4,0(sp)
	sw	a5,4(sp)
	lui	a5,%hi(one)
	lw	t1,%lo(one)(a5)
	lw	t2,%lo(one+4)(a5)
	lui	a5,%hi(A6)
	lw	a4,%lo(A6)(a5)
	lw	a5,%lo(A6+4)(a5)
	sw	a4,8(sp)
	sw	a5,12(sp)
	lui	a5,%hi(A5)
	lw	a4,%lo(A5)(a5)
	lw	a5,%lo(A5+4)(a5)
	sw	a4,16(sp)
	sw	a5,20(sp)
	lui	a5,%hi(A4)
	lw	a4,%lo(A4)(a5)
	lw	a5,%lo(A4+4)(a5)
	sw	a4,24(sp)
	sw	a5,28(sp)
	lui	a5,%hi(A3)
	lw	a4,%lo(A3)(a5)
	lw	a5,%lo(A3+4)(a5)
	sw	a4,32(sp)
	sw	a5,36(sp)
	lui	a5,%hi(A2)
	lw	a4,%lo(A2)(a5)
	lw	a5,%lo(A2+4)(a5)
	sw	a4,40(sp)
	sw	a5,44(sp)
	lui	a5,%hi(A1)
	lw	a4,%lo(A1)(a5)
	lw	a5,%lo(A1+4)(a5)
	sw	a4,48(sp)
	sw	a5,52(sp)
	li	t0,1
	li	s6,0
	li	s7,0
	sw	t3,56(sp)
	sw	t4,60(sp)
.L27:
	fcvt.d.w	a6,t0
	fmul.d	a6,a6,s8
	fmul.d	a0,a6,a6
	lw	a4,56(sp)
	lw	a5,60(sp)
	fmul.d	a4,a4,a0
	fadd.d	a4,a4,t5
	fmul.d	a4,a4,a0
	fadd.d	a4,a4,s0
	fmul.d	a4,a4,a0
	fadd.d	a4,a4,s2
	fmul.d	a4,a4,a0
	fadd.d	a4,a4,s10
	fmul.d	a4,a4,a0
	lw	a2,0(sp)
	lw	a3,4(sp)
	fadd.d	a4,a4,a2
	fmul.d	a4,a4,a0
	fadd.d	a4,a4,t1
	lw	a2,8(sp)
	lw	a3,12(sp)
	fmul.d	a2,a2,a0
	lw	t3,16(sp)
	lw	t4,20(sp)
	fadd.d	a2,a2,t3
	fmul.d	a2,a2,a0
	lw	t3,24(sp)
	lw	t4,28(sp)
	fadd.d	a2,a2,t3
	fmul.d	a2,a2,a0
	lw	t3,32(sp)
	lw	t4,36(sp)
	fadd.d	a2,a2,t3
	fmul.d	a2,a2,a0
	lw	t3,40(sp)
	lw	t4,44(sp)
	fadd.d	a2,a2,t3
	fmul.d	a2,a2,a0
	lw	t3,48(sp)
	lw	t4,52(sp)
	fadd.d	a2,a2,t3
	fmul.d	a2,a2,a0
	fadd.d	a2,a2,t1
	fmul.d	a4,a4,a4
	fmul.d	a4,a4,a6
	fmul.d	a2,a2,a4
	fadd.d	s6,s6,a2
	addi	t0,t0,1
	bne	s4,t0,.L27
.L26:
	lui	s0,%hi(.LANCHOR0)
	addi	s0,s0,%lo(.LANCHOR0)
	addi	a0,s0,288
	call	dtime
	lw	a4,8(s0)
	lw	a5,12(s0)
	lw	a2,296(s0)
	lw	a3,300(s0)
	fmul.d	a4,a4,a2
	lui	s1,%hi(nulltime)
	lw	a2,%lo(nulltime)(s1)
	lw	a3,%lo(nulltime+4)(s1)
	fsub.d	a4,a4,a2
	sw	a4,192(s0)
	sw	a5,196(s0)
	lui	a3,%hi(piref)
	lw	t3,%lo(piref)(a3)
	lw	t4,%lo(piref+4)(a3)
	lui	a3,%hi(three)
	lw	a2,%lo(three)(a3)
	lw	a3,%lo(three+4)(a3)
	fdiv.d	t3,t3,a2
	fmul.d	a6,t3,t3
	lui	s2,%hi(one)
	lw	t1,%lo(one)(s2)
	lw	t2,%lo(one+4)(s2)
	lui	a3,%hi(B6)
	lw	a2,%lo(B6)(a3)
	lw	a3,%lo(B6+4)(a3)
	fmul.d	a2,a6,a2
	lui	a1,%hi(B5)
	lw	a0,%lo(B5)(a1)
	lw	a1,%lo(B5+4)(a1)
	fadd.d	a2,a2,a0
	fmul.d	a2,a2,a6
	lui	a1,%hi(B4)
	lw	a0,%lo(B4)(a1)
	lw	a1,%lo(B4+4)(a1)
	fadd.d	a2,a2,a0
	fmul.d	a2,a2,a6
	lui	a1,%hi(B3)
	lw	a0,%lo(B3)(a1)
	lw	a1,%lo(B3+4)(a1)
	fadd.d	a2,a2,a0
	fmul.d	a2,a2,a6
	lui	a1,%hi(B2)
	lw	a0,%lo(B2)(a1)
	lw	a1,%lo(B2+4)(a1)
	fadd.d	a2,a2,a0
	fmul.d	a2,a2,a6
	lui	a1,%hi(B1)
	lw	a0,%lo(B1)(a1)
	lw	a1,%lo(B1+4)(a1)
	fadd.d	a0,a2,a0
	fmul.d	a0,a0,a6
	fadd.d	a0,a0,t1
	lui	a3,%hi(A6)
	lw	a2,%lo(A6)(a3)
	lw	a3,%lo(A6+4)(a3)
	fmul.d	a2,a6,a2
	lui	t5,%hi(A5)
	lw	t6,%lo(A5+4)(t5)
	lw	t5,%lo(A5)(t5)
	fadd.d	a2,a2,t5
	fmul.d	a2,a2,a6
	lui	t5,%hi(A4)
	lw	t6,%lo(A4+4)(t5)
	lw	t5,%lo(A4)(t5)
	fadd.d	a2,a2,t5
	fmul.d	a2,a2,a6
	lui	t5,%hi(A3)
	lw	t6,%lo(A3+4)(t5)
	lw	t5,%lo(A3)(t5)
	fadd.d	a2,a2,t5
	fmul.d	a2,a2,a6
	lui	t5,%hi(A2)
	lw	t6,%lo(A2+4)(t5)
	lw	t5,%lo(A2)(t5)
	fadd.d	a2,a2,t5
	fmul.d	a2,a2,a6
	lui	t5,%hi(A1)
	lw	t6,%lo(A1+4)(t5)
	lw	t5,%lo(A1)(t5)
	fadd.d	a2,a2,t5
	fmul.d	a2,a2,a6
	fadd.d	a2,a2,t1
	fmul.d	a2,a2,t3
	fmul.d	a2,a2,a0
	fmul.d	a2,a2,a0
	lui	a1,%hi(.LC32)
	lw	a0,%lo(.LC32)(a1)
	lw	a1,%lo(.LC32+4)(a1)
	fdiv.d	a0,a4,a0
	sw	a0,200(s0)
	sw	a1,204(s0)
	lui	a6,%hi(two)
	lw	a7,%lo(two+4)(a6)
	lw	a6,%lo(two)(a6)
	fmul.d	s6,a6,s6
	fadd.d	a2,s6,a2
	fmul.d	a2,a2,s8
	fdiv.d	a2,a2,a6
	lui	a6,%hi(sa)
	sw	a2,%lo(sa)(a6)
	sw	a3,%lo(sa+4)(a6)
	lui	a6,%hi(.LC33)
	lw	a7,%lo(.LC33+4)(a6)
	lw	a6,%lo(.LC33)(a6)
	lui	t3,%hi(sb)
	sw	a6,%lo(sb)(t3)
	sw	a7,%lo(sb+4)(t3)
	fsub.d	a2,a2,a6
	lui	a6,%hi(sc)
	sw	a2,%lo(sc)(a6)
	sw	a3,%lo(sc+4)(a6)
	fdiv.d	a6,t1,a0
	sw	a6,208(s0)
	sw	a7,212(s0)
	lui	a0,%hi(.LC34)
	addi	a0,a0,%lo(.LC34)
	call	printf
	lw	a2,72(s0)
	lw	a3,76(s0)
	lw	a4,48(s0)
	lw	a5,52(s0)
	lw	a0,40(s0)
	lw	a1,44(s0)
	fsub.d	a4,a4,a0
	lui	a1,%hi(five)
	lw	a0,%lo(five)(a1)
	lw	a1,%lo(five+4)(a1)
	fmul.d	a4,a4,a0
	fadd.d	a4,a4,a2
	lui	a1,%hi(.LC35)
	lw	a0,%lo(.LC35)(a1)
	lw	a1,%lo(.LC35+4)(a1)
	fdiv.d	a4,a4,a0
	sw	a4,216(s0)
	sw	a5,220(s0)
	lw	a6,%lo(one)(s2)
	lw	a7,%lo(one+4)(s2)
	fdiv.d	a4,a6,a4
	sw	a4,224(s0)
	sw	a5,228(s0)
	lw	t1,96(s0)
	lw	t2,100(s0)
	lw	s2,144(s0)
	lw	s3,148(s0)
	lw	a4,16(s0)
	lw	a5,20(s0)
	fadd.d	a4,a2,a4
	fadd.d	a4,a4,t1
	lw	a0,120(s0)
	lw	a1,124(s0)
	fadd.d	a4,a4,a0
	fadd.d	a4,a4,s2
	lw	t3,168(s0)
	lw	t4,172(s0)
	lui	a1,%hi(four)
	lw	a0,%lo(four)(a1)
	lw	a1,%lo(four+4)(a1)
	fmul.d	a0,t3,a0
	fadd.d	a0,a0,a4
	lui	t5,%hi(.LC36)
	lw	t6,%lo(.LC36+4)(t5)
	lw	t5,%lo(.LC36)(t5)
	fdiv.d	a0,a0,t5
	sw	a0,232(s0)
	sw	a1,236(s0)
	fdiv.d	a0,a6,a0
	sw	a0,240(s0)
	sw	a1,244(s0)
	lw	a0,192(s0)
	lw	a1,196(s0)
	fadd.d	a4,a4,t3
	fadd.d	a4,a4,a0
	lui	t3,%hi(.LC37)
	lw	t4,%lo(.LC37+4)(t3)
	lw	t3,%lo(.LC37)(t3)
	fdiv.d	a4,a4,t3
	sw	a4,248(s0)
	sw	a5,252(s0)
	fdiv.d	a4,a6,a4
	sw	a4,256(s0)
	sw	a5,260(s0)
	fadd.d	a4,a2,t1
	fadd.d	a4,a4,s2
	fadd.d	a4,a4,a0
	lui	a3,%hi(.LC38)
	lw	a2,%lo(.LC38)(a3)
	lw	a3,%lo(.LC38+4)(a3)
	fdiv.d	a4,a4,a2
	sw	a4,264(s0)
	sw	a5,268(s0)
	fdiv.d	a6,a6,a4
	sw	a6,272(s0)
	sw	a7,276(s0)
	li	a0,10
	call	putchar
	mv	a1,s4
	lui	a0,%hi(.LC39)
	addi	a0,a0,%lo(.LC39)
	call	printf
	lw	a2,%lo(nulltime)(s1)
	lw	a3,%lo(nulltime+4)(s1)
	lui	a0,%hi(.LC40)
	addi	a0,a0,%lo(.LC40)
	call	printf
	lw	a2,224(s0)
	lw	a3,228(s0)
	lui	a0,%hi(.LC41)
	addi	a0,a0,%lo(.LC41)
	call	printf
	lw	a2,240(s0)
	lw	a3,244(s0)
	lui	a0,%hi(.LC42)
	addi	a0,a0,%lo(.LC42)
	call	printf
	lw	a2,256(s0)
	lw	a3,260(s0)
	lui	a0,%hi(.LC43)
	addi	a0,a0,%lo(.LC43)
	call	printf
	lw	a2,272(s0)
	lw	a3,276(s0)
	lui	a0,%hi(.LC44)
	addi	a0,a0,%lo(.LC44)
	call	printf
	li	a0,0
	lw	ra,124(sp)
	lw	s0,120(sp)
	lw	s1,116(sp)
	lw	s2,112(sp)
	lw	s3,108(sp)
	lw	s4,104(sp)
	lw	s5,100(sp)
	lw	s6,96(sp)
	lw	s7,92(sp)
	lw	s8,88(sp)
	lw	s9,84(sp)
	lw	s10,80(sp)
	lw	s11,76(sp)
	addi	sp,sp,128
	jr	ra
	.size	main, .-main
	.globl	E3
	.globl	E2
	.globl	D3
	.globl	D2
	.globl	D1
	.globl	C8
	.globl	C7
	.globl	C6
	.globl	C5
	.globl	C4
	.globl	C3
	.globl	C2
	.globl	C1
	.globl	C0
	.globl	B6
	.globl	B5
	.globl	B4
	.globl	B3
	.globl	B2
	.globl	B1
	.globl	B0
	.globl	A6
	.globl	A5
	.globl	A4
	.globl	A3
	.globl	A2
	.globl	A1
	.globl	A0
	.globl	pierr
	.globl	scale
	.globl	piprg
	.globl	piref
	.globl	five
	.globl	four
	.globl	three
	.globl	two
	.globl	one
	.globl	sd
	.globl	sc
	.globl	sb
	.globl	sa
	.globl	T
	.globl	TLimit
	.globl	TimeArray
	.globl	nulltime
	.section	.srodata.cst8,"aM",@progbits,8
	.align	3
.LC1:
	.word	0
	.word	1078984704
	.align	3
.LC2:
	.word	0
	.word	1076756480
	.align	3
.LC3:
	.word	1413754136
	.word	1074340347
	.align	3
.LC4:
	.word	0
	.word	1072693248
	.align	3
.LC5:
	.word	0
	.word	1073741824
	.align	3
.LC6:
	.word	0
	.word	1074266112
	.align	3
.LC7:
	.word	0
	.word	1074790400
	.align	3
.LC8:
	.word	0
	.word	1075052544
	.align	3
.LC11:
	.word	0
	.word	1093567616
	.align	3
.LC12:
	.word	0
	.word	1076625408
	.align	3
.LC13:
	.word	858993459
	.word	1077490483
	.align	3
.LC15:
	.word	0
	.word	1075576832
	.align	3
.LC16:
	.word	0
	.word	1077886976
	.align	3
.LC18:
	.word	0
	.word	1076953088
	.align	3
.LC19:
	.word	0
	.word	1071644672
	.align	3
.LC22:
	.word	0
	.word	1077739520
	.align	3
.LC23:
	.word	-17155601
	.word	1072049730
	.align	3
.LC25:
	.word	0
	.word	1070596096
	.align	3
.LC27:
	.word	-135163228
	.word	1079612737
	.align	3
.LC28:
	.word	0
	.word	1076363264
	.align	3
.LC29:
	.word	0
	.word	1077018624
	.align	3
.LC30:
	.word	858993459
	.word	1082082099
	.align	3
.LC32:
	.word	0
	.word	1077805056
	.align	3
.LC33:
	.word	-1431655765
	.word	1070770858
	.align	3
.LC35:
	.word	0
	.word	1078591488
	.align	3
.LC36:
	.word	0
	.word	1080229888
	.align	3
.LC37:
	.word	0
	.word	1080180736
	.align	3
.LC38:
	.word	0
	.word	1079427072
	.bss
	.align	3
	.set	.LANCHOR0,. + 0
	.type	T, @object
	.size	T, 288
T:
	.zero	288
	.type	TimeArray, @object
	.size	TimeArray, 24
TimeArray:
	.zero	24
	.section	.sbss,"aw",@nobits
	.align	3
	.type	pierr, @object
	.size	pierr, 8
pierr:
	.zero	8
	.type	scale, @object
	.size	scale, 8
scale:
	.zero	8
	.type	piprg, @object
	.size	piprg, 8
piprg:
	.zero	8
	.type	piref, @object
	.size	piref, 8
piref:
	.zero	8
	.type	five, @object
	.size	five, 8
five:
	.zero	8
	.type	four, @object
	.size	four, 8
four:
	.zero	8
	.type	three, @object
	.size	three, 8
three:
	.zero	8
	.type	two, @object
	.size	two, 8
two:
	.zero	8
	.type	one, @object
	.size	one, 8
one:
	.zero	8
	.type	sd, @object
	.size	sd, 8
sd:
	.zero	8
	.type	sc, @object
	.size	sc, 8
sc:
	.zero	8
	.type	sb, @object
	.size	sb, 8
sb:
	.zero	8
	.type	sa, @object
	.size	sa, 8
sa:
	.zero	8
	.type	TLimit, @object
	.size	TLimit, 8
TLimit:
	.zero	8
	.type	nulltime, @object
	.size	nulltime, 8
nulltime:
	.zero	8
	.section	.sdata,"aw"
	.align	3
	.type	E3, @object
	.size	E3, 8
E3:
	.word	1788578186
	.word	1050383821
	.type	E2, @object
	.size	E2, 8
E2:
	.word	1297423721
	.word	1061123344
	.type	D3, @object
	.size	D3, 8
D3:
	.word	267691816
	.word	1052029018
	.type	D2, @object
	.size	D2, 8
D2:
	.word	1297423721
	.word	1062171920
	.type	D1, @object
	.size	D1, 8
D1:
	.word	1125352308
	.word	1067743969
	.type	C8, @object
	.size	C8, 8
C8:
	.word	-33246769
	.word	1057266947
	.type	C7, @object
	.size	C7, 8
C7:
	.word	-938000582
	.word	1059504247
	.type	C6, @object
	.size	C6, 8
C6:
	.word	2128317445
	.word	1062671128
	.type	C5, @object
	.size	C5, 8
C5:
	.word	1255739916
	.word	1065422234
	.type	C4, @object
	.size	C4, 8
C4:
	.word	2121880904
	.word	1067799899
	.type	C3, @object
	.size	C3, 8
C3:
	.word	2085054741
	.word	1069897048
	.type	C2, @object
	.size	C2, 8
C2:
	.word	-869555016
	.word	1071644671
	.type	C1, @object
	.size	C1, 8
C1:
	.word	-29903902
	.word	1072693247
	.type	C0, @object
	.size	C0, 8
C0:
	.word	0
	.word	1072693248
	.type	B6, @object
	.size	B6, 8
B6:
	.word	1977554986
	.word	1042372530
	.type	B5, @object
	.size	B5, 8
B5:
	.word	-729949298
	.word	-1097696333
	.type	B4, @object
	.size	B4, 8
B4:
	.word	673459639
	.word	1056571797
	.type	B3, @object
	.size	B3, 8
B3:
	.word	-1612522
	.word	-1084833429
	.type	B2, @object
	.size	B2, 8
B2:
	.word	1428750884
	.word	1067799893
	.type	B1, @object
	.size	B1, 8
B1:
	.word	-32426
	.word	-1075838977
	.type	B0, @object
	.size	B0, 8
B0:
	.word	0
	.word	1072693248
	.type	A6, @object
	.size	A6, 8
A6:
	.word	1378468262
	.word	1038519799
	.type	A5, @object
	.size	A5, 8
A5:
	.word	-1929862106
	.word	1046145882
	.type	A4, @object
	.size	A4, 8
A4:
	.word	-2069219994
	.word	1053236722
	.type	A3, @object
	.size	A3, 8
A3:
	.word	1068616305
	.word	1059717536
	.type	A2, @object
	.size	A2, 8
A2:
	.word	289073571
	.word	1065423121
	.type	A1, @object
	.size	A1, 8
A1:
	.word	1431672581
	.word	-1077586603
	.type	A0, @object
	.size	A0, 8
A0:
	.word	0
	.word	1072693248
	.ident	"GCC: (crosstool-NG 1.27.0.68_b286f6b) 14.3.0"
	.section	.note.GNU-stack,"",@progbits
