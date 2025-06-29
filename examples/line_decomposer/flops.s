	.file	"flops.c"
	.option nopic
	.attribute arch, "rv32i2p1_m2p0_a2p1_f2p2_d2p2_c2p0_zicsr2p0_zifencei2p0"
	.attribute unaligned_access, 0
	.attribute stack_align, 16
	.text
	.align	1
	.globl	dtime
	.type	dtime, @function
dtime:
	addi	sp,sp,-32
	sw	ra,28(sp)
	sw	s0,24(sp)
	sw	s1,20(sp)
	fsd	fs0,8(sp)
	mv	s0,a0
	fld	fs0,16(a0)
	lui	a1,%hi(.LANCHOR0)
	addi	s1,a1,%lo(.LANCHOR0)
	addi	a1,a1,%lo(.LANCHOR0)
	li	a0,0
	call	getrusage
	lw	a0,0(s1)
	lw	a1,4(s1)
	call	__floatdidf
	lw	a5,8(s1)
	fcvt.d.w	fa5,a5
	lui	a5,%hi(.LC0)
	fld	fa4,%lo(.LC0)(a5)
	fmul.d	fa5,fa5,fa4
	fadd.d	fa0,fa0,fa5
	fsd	fa0,16(s0)
	fsub.d	fa0,fa0,fs0
	fsd	fa0,8(s0)
	li	a0,0
	lw	ra,28(sp)
	lw	s0,24(sp)
	lw	s1,20(sp)
	fld	fs0,8(sp)
	addi	sp,sp,32
	jr	ra
	.size	dtime, .-dtime
	.section	.rodata.str1.4,"aMS",@progbits,1
	.align	2
.LC1:
	.string	"   FLOPS C Program (Double Precision), V2.0 18 Dec 1992\n"
	.align	2
.LC10:
	.string	"   Module     Error        RunTime      MFLOPS"
	.align	2
.LC11:
	.string	"                            (usec)"
	.align	2
.LC15:
	.string	"     1   %13.4le  %10.4lf  %10.4lf\n"
	.align	2
.LC18:
	.string	"     2   %13.4le  %10.4lf  %10.4lf\n"
	.align	2
.LC21:
	.string	"     3   %13.4le  %10.4lf  %10.4lf\n"
	.align	2
.LC22:
	.string	"     4   %13.4le  %10.4lf  %10.4lf\n"
	.align	2
.LC25:
	.string	"     5   %13.4le  %10.4lf  %10.4lf\n"
	.align	2
.LC27:
	.string	"     6   %13.4le  %10.4lf  %10.4lf\n"
	.align	2
.LC32:
	.string	"     7   %13.4le  %10.4lf  %10.4lf\n"
	.align	2
.LC35:
	.string	"     8   %13.4le  %10.4lf  %10.4lf\n"
	.align	2
.LC40:
	.string	"   Iterations      = %10ld\n"
	.align	2
.LC41:
	.string	"   NullTime (usec) = %10.4lf\n"
	.align	2
.LC42:
	.string	"   MFLOPS(1)       = %10.4lf\n"
	.align	2
.LC43:
	.string	"   MFLOPS(2)       = %10.4lf\n"
	.align	2
.LC44:
	.string	"   MFLOPS(3)       = %10.4lf\n"
	.align	2
.LC45:
	.string	"   MFLOPS(4)       = %10.4lf\n\n"
	.text
	.align	1
	.globl	main
	.type	main, @function
main:
	addi	sp,sp,-112
	sw	ra,108(sp)
	sw	s0,104(sp)
	sw	s1,100(sp)
	sw	s2,96(sp)
	sw	s3,92(sp)
	sw	s4,88(sp)
	sw	s5,84(sp)
	sw	s6,80(sp)
	sw	s7,76(sp)
	sw	s8,72(sp)
	sw	s9,68(sp)
	fsd	fs0,56(sp)
	fsd	fs1,48(sp)
	fsd	fs2,40(sp)
	fsd	fs3,32(sp)
	fsd	fs4,24(sp)
	li	a0,10
	call	putchar
	lui	a0,%hi(.LC1)
	addi	a0,a0,%lo(.LC1)
	call	puts
	lui	s0,%hi(.LANCHOR0)
	addi	s0,s0,%lo(.LANCHOR0)
	lui	a5,%hi(.LC2)
	fld	fa5,%lo(.LC2)(a5)
	fsd	fa5,40(s0)
	lui	a4,%hi(.LC3)
	lui	a5,%hi(TLimit)
	fld	fa5,%lo(.LC3)(a4)
	fsd	fa5,%lo(TLimit)(a5)
	lui	a4,%hi(.LC4)
	lui	a5,%hi(piref)
	fld	fa5,%lo(.LC4)(a4)
	fsd	fa5,%lo(piref)(a5)
	lui	a5,%hi(.LC5)
	fld	fa5,%lo(.LC5)(a5)
	lui	a5,%hi(one)
	fsd	fa5,%lo(one)(a5)
	lui	a4,%hi(.LC6)
	lui	a5,%hi(two)
	fld	fa4,%lo(.LC6)(a4)
	fsd	fa4,%lo(two)(a5)
	lui	a4,%hi(.LC7)
	lui	a5,%hi(three)
	fld	fa4,%lo(.LC7)(a4)
	fsd	fa4,%lo(three)(a5)
	lui	a4,%hi(.LC8)
	lui	a5,%hi(four)
	fld	fa4,%lo(.LC8)(a4)
	fsd	fa4,%lo(four)(a5)
	lui	a4,%hi(.LC9)
	lui	a5,%hi(five)
	fld	fa4,%lo(.LC9)(a4)
	fsd	fa4,%lo(five)(a5)
	lui	a5,%hi(scale)
	fsd	fa5,%lo(scale)(a5)
	lui	a0,%hi(.LC10)
	addi	a0,a0,%lo(.LC10)
	call	puts
	lui	a0,%hi(.LC11)
	addi	a0,a0,%lo(.LC11)
	call	puts
	addi	s0,s0,320
	mv	a0,s0
	call	dtime
	mv	a0,s0
	call	dtime
	lui	a5,%hi(sa)
	sw	zero,%lo(sa)(a5)
	sw	zero,%lo(sa+4)(a5)
	li	s0,16384
	addi	s0,s0,-759
	mv	s4,a5
	lui	s7,%hi(TLimit)
	lui	s9,%hi(one)
	lui	s6,%hi(.LANCHOR0)
	addi	s6,s6,%lo(.LANCHOR0)
	addi	s5,s6,320
	li	s8,1
	j	.L4
.L56:
	fcvt.d.w	fa4,s0
	lui	a5,%hi(.LC12)
	fld	fa5,%lo(.LC12)(a5)
	fdiv.d	fa5,fa5,fa4
	lui	a5,%hi(scale)
	fsd	fa5,%lo(scale)(a5)
	lui	a0,%hi(.LANCHOR0)
	addi	a0,a0,%lo(.LANCHOR0)
	fsd	fa5,40(a0)
	addi	a0,a0,320
	call	dtime
	li	a5,1
	bgt	s0,a5,.L34
	j	.L9
.L36:
	fcvt.d.w	fs0,x0
	j	.L18
.L37:
	fcvt.d.w	fs1,x0
	j	.L20
.L38:
	fcvt.d.w	fs0,x0
	j	.L22
.L39:
	fcvt.d.w	fs0,x0
	j	.L24
.L40:
	fcvt.d.w	fs2,x0
	j	.L26
.L41:
	fcvt.d.w	fs0,x0
	j	.L28
.L31:
	lui	a0,%hi(.LANCHOR0)
	addi	a0,a0,%lo(.LANCHOR0)
	sw	zero,72(a0)
	sw	zero,76(a0)
	fcvt.d.w	fa5,s0
	lui	a5,%hi(sc)
	fsd	fa5,%lo(sc)(a5)
	addi	a0,a0,320
	call	dtime
	fcvt.d.w	fs3,x0
	fmv.d	fs4,fs3
	fmv.d	fs2,fs3
	j	.L30
.L13:
	lui	s1,%hi(.LANCHOR0)
	addi	s1,s1,%lo(.LANCHOR0)
	addi	a0,s1,320
	call	dtime
	fld	fa5,40(s1)
	fld	fa4,328(s1)
	fmul.d	fa5,fa5,fa4
	fcvt.d.w	fa4,x0
	flt.d	a5,fa5,fa4
	bne	a5,zero,.L31
	lui	a0,%hi(.LANCHOR0)
	addi	a0,a0,%lo(.LANCHOR0)
	fsd	fa5,72(a0)
	fcvt.d.w	fa5,s0
	lui	a5,%hi(sc)
	fsd	fa5,%lo(sc)(a5)
	addi	a0,a0,320
	call	dtime
	fcvt.d.w	fs3,x0
	fmv.d	fs4,fs3
	fmv.d	fs2,fs3
	j	.L30
.L15:
	lui	a0,%hi(.LANCHOR0)
	addi	a0,a0,%lo(.LANCHOR0)
	sw	zero,72(a0)
	sw	zero,76(a0)
	fcvt.d.w	fa5,s0
	lui	a5,%hi(sc)
	fsd	fa5,%lo(sc)(a5)
	lui	a5,%hi(sa)
	fld	fs1,%lo(sa)(a5)
	addi	a0,a0,320
	call	dtime
	j	.L33
.L5:
	lui	s0,%hi(.LANCHOR0)
	addi	s0,s0,%lo(.LANCHOR0)
	addi	a0,s0,320
	call	dtime
	fld	fa5,328(s0)
	lui	a5,%hi(sa)
	fsd	fa5,%lo(sa)(a5)
	fcvt.d.w	fs1,x0
.L35:
	mv	s0,s1
.L4:
	fld	fa4,%lo(sa)(s4)
	fld	fa5,%lo(TLimit)(s7)
	flt.d	a5,fa4,fa5
	beq	a5,zero,.L56
	slli	s1,s0,1
	fld	fs0,%lo(one)(s9)
	fcvt.d.w	fs2,s1
	fdiv.d	fs2,fs0,fs2
	mv	a0,s5
	call	dtime
	ble	s1,s8,.L5
	lui	a5,%hi(D3)
	fld	ft2,%lo(D3)(a5)
	lui	a5,%hi(D2)
	fld	ft1,%lo(D2)(a5)
	lui	a5,%hi(D1)
	fld	fa1,%lo(D1)(a5)
	lui	a5,%hi(E3)
	fld	ft0,%lo(E3)(a5)
	lui	a5,%hi(E2)
	fld	fa0,%lo(E2)(a5)
	mv	a4,s1
	li	a5,1
	fcvt.d.w	fa2,x0
	fmv.d	fs1,fa2
.L6:
	fadd.d	fa2,fa2,fs0
	fmul.d	fa3,fs2,fa2
	fmul.d	fa5,ft2,fa3
	fadd.d	fa5,fa5,ft1
	fmul.d	fa5,fa5,fa3
	fadd.d	fa4,fa5,fa1
	fmul.d	fa5,ft0,fa3
	fadd.d	fa5,fa5,fa0
	fmul.d	fa5,fa5,fa3
	fadd.d	fa5,fa5,fa1
	fmul.d	fa5,fa5,fa3
	fadd.d	fa5,fa5,fs0
	fdiv.d	fa5,fa4,fa5
	fadd.d	fs1,fs1,fa5
	addi	a5,a5,1
	bne	a5,a4,.L6
	mv	a0,s5
	call	dtime
	fld	fa5,328(s6)
	fsd	fa5,%lo(sa)(s4)
	li	a5,256000000
	bne	s0,a5,.L35
	fcvt.d.w	fa4,s1
	lui	a5,%hi(.LC12)
	fld	fa5,%lo(.LC12)(a5)
	fdiv.d	fa5,fa5,fa4
	lui	a5,%hi(scale)
	fsd	fa5,%lo(scale)(a5)
	lui	a0,%hi(.LANCHOR0)
	addi	a0,a0,%lo(.LANCHOR0)
	fsd	fa5,40(a0)
	addi	a0,a0,320
	call	dtime
	mv	s0,s1
.L34:
	li	a5,1
.L10:
	addi	a5,a5,1
	bne	a5,s0,.L10
.L9:
	lui	s0,%hi(.LANCHOR0)
	addi	s0,s0,%lo(.LANCHOR0)
	addi	a0,s0,320
	call	dtime
	fld	fa5,40(s0)
	fld	fa4,328(s0)
	fmul.d	fa4,fa5,fa4
	fsd	fa4,8(sp)
	fcvt.d.w	fa4,x0
	fld	fa3,8(sp)
	flt.d	a3,fa3,fa4
	lw	a4,8(sp)
	lw	a5,12(sp)
	beq	a3,zero,.L12
	li	a4,0
	li	a5,0
.L12:
	mv	s3,a5
	lui	a5,%hi(nulltime)
	sw	a4,%lo(nulltime)(a5)
	sw	s3,%lo(nulltime+4)(a5)
	lui	s2,%hi(sa)
	fld	fa4,%lo(sa)(s2)
	fmul.d	fa5,fa5,fa4
	fld	fa4,%lo(nulltime)(a5)
	fsub.d	fa4,fa5,fa4
	lui	s1,%hi(.LANCHOR0)
	addi	s1,s1,%lo(.LANCHOR0)
	fsd	fa4,48(s1)
	lui	a5,%hi(D1)
	fld	fa5,%lo(D1)(a5)
	lui	s3,%hi(one)
	fld	fa2,%lo(one)(s3)
	lui	a5,%hi(D2)
	fld	fa3,%lo(D2)(a5)
	fadd.d	fa3,fa5,fa3
	lui	a5,%hi(D3)
	fld	fa1,%lo(D3)(a5)
	fadd.d	fa3,fa3,fa1
	fadd.d	fa1,fa5,fa2
	lui	a5,%hi(E2)
	fld	fa0,%lo(E2)(a5)
	fadd.d	fa1,fa1,fa0
	lui	a5,%hi(E3)
	fld	fa0,%lo(E3)(a5)
	fadd.d	fa1,fa1,fa0
	fdiv.d	fa3,fa3,fa1
	lui	a5,%hi(.LC13)
	fld	fa1,%lo(.LC13)(a5)
	fdiv.d	fa1,fa4,fa1
	fsd	fa1,56(s1)
	lui	a5,%hi(two)
	fld	fa0,%lo(two)(a5)
	fadd.d	fa5,fa5,fa3
	fmul.d	fs1,fa0,fs1
	fadd.d	fa5,fa5,fs1
	fmul.d	fa5,fa5,fs2
	fdiv.d	fa5,fa5,fa0
	fsd	fa5,%lo(sa)(s2)
	fdiv.d	fa5,fa2,fa5
	lui	a5,%hi(sb)
	fsd	fa5,%lo(sb)(a5)
	fcvt.w.d a5,fa5,rtz
	li	a4,40960
	addi	a4,a4,-960
	mul	a5,a5,a4
	fcvt.d.w	fa3,a5
	lui	a5,%hi(scale)
	fld	fa0,%lo(scale)(a5)
	fdiv.d	fa3,fa3,fa0
	fcvt.w.d s0,fa3,rtz
	lui	a5,%hi(.LC14)
	fld	fa3,%lo(.LC14)(a5)
	fsub.d	fa5,fa5,fa3
	lui	a5,%hi(sc)
	fsd	fa5,%lo(sc)(a5)
	fdiv.d	fa2,fa2,fa1
	fsd	fa2,64(s1)
	fsd	fa2,8(sp)
	lw	a6,8(sp)
	lw	a7,12(sp)
	fsd	fa4,8(sp)
	lw	a4,8(sp)
	lw	a5,12(sp)
	fsd	fa5,8(sp)
	lw	a2,8(sp)
	lw	a3,12(sp)
	lui	a0,%hi(.LC15)
	addi	a0,a0,%lo(.LC15)
	call	printf
	lui	a5,%hi(five)
	fld	fs0,%lo(five)(a5)
	fneg.d	fs0,fs0
	fld	fa5,%lo(one)(s3)
	fneg.d	fa5,fa5
	fsd	fa5,%lo(sa)(s2)
	addi	a0,s1,320
	call	dtime
	ble	s0,zero,.L13
	fld	fa4,%lo(sa)(s2)
	addi	a4,s0,1
	li	a5,1
.L14:
	fsub.d	fa4,fa4,fs0
	fneg.d	fs0,fs0
	addi	a5,a5,1
	bne	a5,a4,.L14
	lui	a5,%hi(sa)
	fsd	fa4,%lo(sa)(a5)
	lui	s1,%hi(.LANCHOR0)
	addi	s1,s1,%lo(.LANCHOR0)
	addi	a0,s1,320
	call	dtime
	fld	fa5,40(s1)
	fld	fa4,328(s1)
	fmul.d	fa5,fa5,fa4
	fcvt.d.w	fa4,x0
	flt.d	a5,fa5,fa4
	bne	a5,zero,.L15
	lui	a0,%hi(.LANCHOR0)
	addi	a0,a0,%lo(.LANCHOR0)
	fsd	fa5,72(a0)
	fcvt.d.w	fa5,s0
	lui	a5,%hi(sc)
	fsd	fa5,%lo(sc)(a5)
	lui	a5,%hi(sa)
	fld	fs1,%lo(sa)(a5)
	addi	a0,a0,320
	call	dtime
.L33:
	lui	a5,%hi(sa)
	fld	fa5,%lo(sa)(a5)
	lui	a5,%hi(two)
	fld	fa3,%lo(two)(a5)
	addi	a4,s0,1
	li	a5,1
	fcvt.d.w	fs3,x0
	fmv.d	fs4,fs3
	fmv.d	fs2,fs3
.L17:
	fmv.d	fa4,fs0
	fneg.d	fs0,fs0
	fsub.d	fa5,fa5,fa4
	fadd.d	fs1,fs1,fa3
	fsub.d	fa4,fs0,fs1
	fadd.d	fs3,fs3,fa4
	fmul.d	fa4,fs0,fs1
	fsub.d	fs2,fs2,fa4
	fdiv.d	fa4,fs0,fs1
	fadd.d	fs4,fs4,fa4
	addi	a5,a5,1
	bne	a4,a5,.L17
	lui	a5,%hi(sa)
	fsd	fa5,%lo(sa)(a5)
.L30:
	lui	s1,%hi(.LANCHOR0)
	addi	s1,s1,%lo(.LANCHOR0)
	addi	s2,s1,320
	mv	a0,s2
	call	dtime
	fld	fa4,40(s1)
	fld	fa5,328(s1)
	fmul.d	fa4,fa4,fa5
	fsd	fa4,80(s1)
	fld	fa5,72(s1)
	fsub.d	fa4,fa4,fa5
	lui	a5,%hi(.LC16)
	fld	fa2,%lo(.LC16)(a5)
	fdiv.d	fa2,fa4,fa2
	fsd	fa2,88(s1)
	lui	a4,%hi(sa)
	fld	fa5,%lo(sa)(a4)
	fmul.d	fa5,fs3,fa5
	lui	a5,%hi(sc)
	fld	fa3,%lo(sc)(a5)
	fdiv.d	fa5,fa5,fa3
	fcvt.w.d s0,fa5,rtz
	lui	a3,%hi(five)
	fld	fa5,%lo(five)(a3)
	lui	a3,%hi(four)
	fld	fa3,%lo(four)(a3)
	fmul.d	fa3,fs4,fa3
	fdiv.d	fa3,fa3,fa5
	fsd	fa3,%lo(sa)(a4)
	fdiv.d	fa5,fa5,fs2
	fadd.d	fa5,fa5,fa3
	lui	a4,%hi(sb)
	fsd	fa5,%lo(sb)(a4)
	lui	a4,%hi(.LC17)
	fld	fa3,%lo(.LC17)(a4)
	fsd	fa3,%lo(sc)(a5)
	fmul.d	fa1,fs2,fs2
	fmul.d	fa1,fa1,fs2
	fdiv.d	fa3,fa3,fa1
	fsub.d	fa5,fa5,fa3
	lui	a5,%hi(piprg)
	fsd	fa5,%lo(piprg)(a5)
	lui	s3,%hi(piref)
	fld	fa3,%lo(piref)(s3)
	fsub.d	fa5,fa5,fa3
	lui	a5,%hi(pierr)
	fsd	fa5,%lo(pierr)(a5)
	lui	a5,%hi(one)
	fld	fa3,%lo(one)(a5)
	fdiv.d	fa3,fa3,fa2
	fsd	fa3,96(s1)
	fsd	fa3,8(sp)
	lw	a6,8(sp)
	lw	a7,12(sp)
	fsd	fa4,8(sp)
	lw	a4,8(sp)
	lw	a5,12(sp)
	fsd	fa5,8(sp)
	lw	a2,8(sp)
	lw	a3,12(sp)
	lui	a0,%hi(.LC18)
	addi	a0,a0,%lo(.LC18)
	call	printf
	fcvt.d.w	fs2,s0
	lui	a5,%hi(three)
	fld	fa5,%lo(three)(a5)
	fmul.d	fa5,fs2,fa5
	fld	fs1,%lo(piref)(s3)
	fdiv.d	fs1,fs1,fa5
	mv	a0,s2
	call	dtime
	li	a5,1
	ble	s0,a5,.L36
	lui	a5,%hi(one)
	fld	fa1,%lo(one)(a5)
	lui	a5,%hi(A6)
	fld	ft4,%lo(A6)(a5)
	lui	a5,%hi(A5)
	fld	ft3,%lo(A5)(a5)
	lui	a5,%hi(A4)
	fld	ft2,%lo(A4)(a5)
	lui	a5,%hi(A3)
	fld	ft1,%lo(A3)(a5)
	lui	a5,%hi(A2)
	fld	ft0,%lo(A2)(a5)
	lui	a5,%hi(A1)
	fld	fa0,%lo(A1)(a5)
	li	a5,1
	fcvt.d.w	fa2,x0
	fmv.d	fs0,fa2
.L19:
	fadd.d	fa2,fa2,fa1
	fmul.d	fa3,fs1,fa2
	fmul.d	fa4,fa3,fa3
	fmul.d	fa5,ft4,fa4
	fsub.d	fa5,fa5,ft3
	fmul.d	fa5,fa5,fa4
	fadd.d	fa5,fa5,ft2
	fmul.d	fa5,fa5,fa4
	fsub.d	fa5,fa5,ft1
	fmul.d	fa5,fa5,fa4
	fadd.d	fa5,fa5,ft0
	fmul.d	fa5,fa5,fa4
	fadd.d	fa5,fa5,fa0
	fmul.d	fa5,fa5,fa4
	fadd.d	fa5,fa5,fa1
	fmul.d	fa5,fa5,fa3
	fadd.d	fs0,fs0,fa5
	addi	a5,a5,1
	bne	s0,a5,.L19
.L18:
	lui	s1,%hi(.LANCHOR0)
	addi	s1,s1,%lo(.LANCHOR0)
	addi	s4,s1,320
	mv	a0,s4
	call	dtime
	fld	fa3,40(s1)
	fld	fa5,328(s1)
	fmul.d	fa3,fa3,fa5
	lui	a5,%hi(nulltime)
	fld	fa5,%lo(nulltime)(a5)
	fsub.d	fa3,fa3,fa5
	fsd	fa3,104(s1)
	lui	s5,%hi(piref)
	lui	s6,%hi(three)
	fld	fa2,%lo(piref)(s5)
	fld	fa5,%lo(three)(s6)
	fdiv.d	fa2,fa2,fa5
	fmul.d	fa5,fa2,fa2
	lui	a5,%hi(one)
	fld	fa1,%lo(one)(a5)
	lui	a5,%hi(A6)
	fld	fa4,%lo(A6)(a5)
	fmul.d	fa4,fa5,fa4
	lui	s2,%hi(A5)
	fld	fa0,%lo(A5)(s2)
	fsub.d	fa4,fa4,fa0
	fmul.d	fa4,fa4,fa5
	lui	a5,%hi(A4)
	fld	fa0,%lo(A4)(a5)
	fadd.d	fa4,fa4,fa0
	fmul.d	fa4,fa4,fa5
	lui	s3,%hi(A3)
	fld	fa0,%lo(A3)(s3)
	fsub.d	fa4,fa4,fa0
	fmul.d	fa4,fa4,fa5
	lui	a5,%hi(A2)
	fld	fa0,%lo(A2)(a5)
	fadd.d	fa4,fa4,fa0
	fmul.d	fa4,fa4,fa5
	lui	a5,%hi(A1)
	fld	fa0,%lo(A1)(a5)
	fadd.d	fa4,fa4,fa0
	fmul.d	fa4,fa4,fa5
	fadd.d	fa4,fa4,fa1
	fmul.d	fa4,fa4,fa2
	lui	a5,%hi(.LC19)
	fld	fa2,%lo(.LC19)(a5)
	fdiv.d	fa2,fa3,fa2
	fsd	fa2,112(s1)
	lui	a5,%hi(two)
	fld	fa0,%lo(two)(a5)
	fmul.d	fa5,fa0,fs0
	fadd.d	fa5,fa5,fa4
	fmul.d	fa5,fa5,fs1
	fdiv.d	fa5,fa5,fa0
	lui	a5,%hi(sa)
	fsd	fa5,%lo(sa)(a5)
	lui	a5,%hi(.LC20)
	fld	fa4,%lo(.LC20)(a5)
	lui	a5,%hi(sb)
	fsd	fa4,%lo(sb)(a5)
	fsub.d	fa5,fa5,fa4
	lui	a5,%hi(sc)
	fsd	fa5,%lo(sc)(a5)
	fdiv.d	fa4,fa1,fa2
	fsd	fa4,120(s1)
	fsd	fa4,8(sp)
	lw	a6,8(sp)
	lw	a7,12(sp)
	fsd	fa3,8(sp)
	lw	a4,8(sp)
	lw	a5,12(sp)
	fsd	fa5,8(sp)
	lw	a2,8(sp)
	lw	a3,12(sp)
	lui	a0,%hi(.LC21)
	addi	a0,a0,%lo(.LC21)
	call	printf
	fld	fa5,%lo(A3)(s3)
	fneg.d	fa5,fa5
	fsd	fa5,%lo(A3)(s3)
	fld	fa5,%lo(A5)(s2)
	fneg.d	fa5,fa5
	fsd	fa5,%lo(A5)(s2)
	fld	fa5,%lo(three)(s6)
	fmul.d	fa5,fs2,fa5
	fld	fs0,%lo(piref)(s5)
	fdiv.d	fs0,fs0,fa5
	mv	a0,s4
	call	dtime
	li	a5,1
	ble	s0,a5,.L37
	lui	a5,%hi(B6)
	fld	ft2,%lo(B6)(a5)
	lui	a5,%hi(B5)
	fld	ft1,%lo(B5)(a5)
	lui	a5,%hi(B4)
	fld	ft0,%lo(B4)(a5)
	lui	a5,%hi(B3)
	fld	fa0,%lo(B3)(a5)
	lui	a5,%hi(B2)
	fld	fa1,%lo(B2)(a5)
	lui	a5,%hi(B1)
	fld	fa2,%lo(B1)(a5)
	lui	a5,%hi(one)
	fld	fa3,%lo(one)(a5)
	li	a5,1
	fcvt.d.w	fs1,x0
.L21:
	fcvt.d.w	fa4,a5
	fmul.d	fa4,fa4,fs0
	fmul.d	fa4,fa4,fa4
	fmul.d	fa5,ft2,fa4
	fadd.d	fa5,fa5,ft1
	fmul.d	fa5,fa5,fa4
	fadd.d	fa5,fa5,ft0
	fmul.d	fa5,fa5,fa4
	fadd.d	fa5,fa5,fa0
	fmul.d	fa5,fa5,fa4
	fadd.d	fa5,fa5,fa1
	fmul.d	fa5,fa5,fa4
	fadd.d	fa5,fa5,fa2
	fmul.d	fa5,fa5,fa4
	fadd.d	fa5,fa5,fs1
	fadd.d	fs1,fa5,fa3
	addi	a5,a5,1
	bne	s0,a5,.L21
.L20:
	lui	s1,%hi(.LANCHOR0)
	addi	s1,s1,%lo(.LANCHOR0)
	addi	s2,s1,320
	mv	a0,s2
	call	dtime
	fld	fa3,40(s1)
	fld	fa5,328(s1)
	fmul.d	fa3,fa3,fa5
	lui	a5,%hi(nulltime)
	fld	fa5,%lo(nulltime)(a5)
	fsub.d	fa3,fa3,fa5
	fsd	fa3,128(s1)
	lui	s3,%hi(piref)
	lui	s4,%hi(three)
	fld	fa1,%lo(piref)(s3)
	fld	fa5,%lo(three)(s4)
	fdiv.d	fa1,fa1,fa5
	fmul.d	fa0,fa1,fa1
	lui	a5,%hi(one)
	fld	fa2,%lo(one)(a5)
	lui	a5,%hi(B6)
	fld	fa5,%lo(B6)(a5)
	fmul.d	fa5,fa0,fa5
	lui	a5,%hi(B5)
	fld	fa4,%lo(B5)(a5)
	fadd.d	fa5,fa5,fa4
	fmul.d	fa5,fa5,fa0
	lui	a5,%hi(B4)
	fld	fa4,%lo(B4)(a5)
	fadd.d	fa5,fa5,fa4
	fmul.d	fa5,fa5,fa0
	lui	a5,%hi(B3)
	fld	fa4,%lo(B3)(a5)
	fadd.d	fa5,fa5,fa4
	fmul.d	fa5,fa5,fa0
	lui	a5,%hi(B2)
	fld	fa4,%lo(B2)(a5)
	fadd.d	fa5,fa5,fa4
	fmul.d	fa5,fa5,fa0
	lui	a5,%hi(B1)
	fld	fa4,%lo(B1)(a5)
	fadd.d	fa5,fa5,fa4
	fmul.d	fa5,fa5,fa0
	fadd.d	fa5,fa5,fa2
	lui	a5,%hi(.LC3)
	fld	ft0,%lo(.LC3)(a5)
	fdiv.d	ft0,fa3,ft0
	fsd	ft0,136(s1)
	lui	a5,%hi(two)
	fld	fa4,%lo(two)(a5)
	fadd.d	fa5,fa2,fa5
	fmul.d	fs1,fa4,fs1
	fadd.d	fa5,fa5,fs1
	fmul.d	fa5,fa5,fs0
	fdiv.d	fa5,fa5,fa4
	lui	a5,%hi(sa)
	fsd	fa5,%lo(sa)(a5)
	lui	a5,%hi(A6)
	fld	fa4,%lo(A6)(a5)
	fmul.d	fa4,fa0,fa4
	lui	a5,%hi(A5)
	fld	ft1,%lo(A5)(a5)
	fadd.d	fa4,fa4,ft1
	fmul.d	fa4,fa4,fa0
	lui	a5,%hi(A4)
	fld	ft1,%lo(A4)(a5)
	fadd.d	fa4,fa4,ft1
	fmul.d	fa4,fa4,fa0
	lui	a5,%hi(A3)
	fld	ft1,%lo(A3)(a5)
	fadd.d	fa4,fa4,ft1
	fmul.d	fa4,fa4,fa0
	lui	a5,%hi(A2)
	fld	ft1,%lo(A2)(a5)
	fadd.d	fa4,fa4,ft1
	fmul.d	fa4,fa4,fa0
	lui	a5,%hi(A1)
	fld	ft1,%lo(A1)(a5)
	fadd.d	fa4,fa4,ft1
	fmul.d	fa4,fa4,fa0
	lui	a5,%hi(A0)
	fld	fa0,%lo(A0)(a5)
	fadd.d	fa4,fa4,fa0
	fmul.d	fa4,fa4,fa1
	lui	a5,%hi(sb)
	fsd	fa4,%lo(sb)(a5)
	fsub.d	fa5,fa5,fa4
	lui	a5,%hi(sc)
	fsd	fa5,%lo(sc)(a5)
	fdiv.d	fa2,fa2,ft0
	fsd	fa2,144(s1)
	fsd	fa2,8(sp)
	lw	a6,8(sp)
	lw	a7,12(sp)
	fsd	fa3,8(sp)
	lw	a4,8(sp)
	lw	a5,12(sp)
	fsd	fa5,8(sp)
	lw	a2,8(sp)
	lw	a3,12(sp)
	lui	a0,%hi(.LC22)
	addi	a0,a0,%lo(.LC22)
	call	printf
	fld	fa5,%lo(three)(s4)
	fmul.d	fa5,fs2,fa5
	fld	fs1,%lo(piref)(s3)
	fdiv.d	fs1,fs1,fa5
	mv	a0,s2
	call	dtime
	li	a5,1
	ble	s0,a5,.L38
	lui	a5,%hi(A6)
	fld	fa6,%lo(A6)(a5)
	lui	a5,%hi(A5)
	fld	ft7,%lo(A5)(a5)
	lui	a5,%hi(A4)
	fld	ft6,%lo(A4)(a5)
	lui	a5,%hi(A3)
	fld	ft5,%lo(A3)(a5)
	lui	a5,%hi(A2)
	fld	ft4,%lo(A2)(a5)
	lui	a5,%hi(A1)
	fld	ft3,%lo(A1)(a5)
	lui	a5,%hi(one)
	fld	fa3,%lo(one)(a5)
	lui	a5,%hi(B6)
	fld	ft2,%lo(B6)(a5)
	lui	a5,%hi(B5)
	fld	ft1,%lo(B5)(a5)
	lui	a5,%hi(B4)
	fld	ft0,%lo(B4)(a5)
	lui	a5,%hi(B3)
	fld	fa0,%lo(B3)(a5)
	lui	a5,%hi(B2)
	fld	fa1,%lo(B2)(a5)
	lui	a5,%hi(B1)
	fld	fa2,%lo(B1)(a5)
	li	a5,1
	fcvt.d.w	fs0,x0
.L23:
	fcvt.d.w	fa4,a5
	fmul.d	fa4,fa4,fs1
	fmul.d	fa7,fa4,fa4
	fmul.d	fa5,fa6,fa7
	fadd.d	fa5,fa5,ft7
	fmul.d	fa5,fa5,fa7
	fadd.d	fa5,fa5,ft6
	fmul.d	fa5,fa5,fa7
	fadd.d	fa5,fa5,ft5
	fmul.d	fa5,fa5,fa7
	fadd.d	fa5,fa5,ft4
	fmul.d	fa5,fa5,fa7
	fadd.d	fa5,fa5,ft3
	fmul.d	fa5,fa5,fa7
	fadd.d	fa5,fa5,fa3
	fmul.d	fa5,fa5,fa4
	fmul.d	fa4,ft2,fa7
	fadd.d	fa4,fa4,ft1
	fmul.d	fa4,fa4,fa7
	fadd.d	fa4,fa4,ft0
	fmul.d	fa4,fa4,fa7
	fadd.d	fa4,fa4,fa0
	fmul.d	fa4,fa4,fa7
	fadd.d	fa4,fa4,fa1
	fmul.d	fa4,fa4,fa7
	fadd.d	fa4,fa4,fa2
	fmul.d	fa4,fa4,fa7
	fadd.d	fa4,fa4,fa3
	fdiv.d	fa5,fa5,fa4
	fadd.d	fs0,fs0,fa5
	addi	a5,a5,1
	bne	s0,a5,.L23
.L22:
	lui	s1,%hi(.LANCHOR0)
	addi	s1,s1,%lo(.LANCHOR0)
	addi	s2,s1,320
	mv	a0,s2
	call	dtime
	fld	fa3,40(s1)
	fld	fa5,328(s1)
	fmul.d	fa3,fa3,fa5
	lui	a5,%hi(nulltime)
	fld	fa5,%lo(nulltime)(a5)
	fsub.d	fa3,fa3,fa5
	fsd	fa3,152(s1)
	lui	s3,%hi(piref)
	fld	fa5,%lo(piref)(s3)
	lui	a5,%hi(three)
	fld	fa4,%lo(three)(a5)
	fdiv.d	fa5,fa5,fa4
	fmul.d	fa1,fa5,fa5
	lui	a5,%hi(one)
	fld	fa2,%lo(one)(a5)
	lui	a5,%hi(A6)
	fld	fa4,%lo(A6)(a5)
	fmul.d	fa4,fa1,fa4
	lui	a5,%hi(A5)
	fld	fa0,%lo(A5)(a5)
	fadd.d	fa4,fa4,fa0
	fmul.d	fa4,fa4,fa1
	lui	a5,%hi(A4)
	fld	fa0,%lo(A4)(a5)
	fadd.d	fa4,fa4,fa0
	fmul.d	fa4,fa4,fa1
	lui	a5,%hi(A3)
	fld	fa0,%lo(A3)(a5)
	fadd.d	fa4,fa4,fa0
	fmul.d	fa4,fa4,fa1
	lui	a5,%hi(A2)
	fld	fa0,%lo(A2)(a5)
	fadd.d	fa4,fa4,fa0
	fmul.d	fa4,fa4,fa1
	lui	a5,%hi(A1)
	fld	fa0,%lo(A1)(a5)
	fadd.d	fa4,fa4,fa0
	fmul.d	fa4,fa4,fa1
	fadd.d	fa4,fa4,fa2
	fmul.d	fa4,fa4,fa5
	lui	a5,%hi(B6)
	fld	fa5,%lo(B6)(a5)
	fmul.d	fa5,fa1,fa5
	lui	a5,%hi(B5)
	fld	fa0,%lo(B5)(a5)
	fadd.d	fa5,fa5,fa0
	fmul.d	fa5,fa5,fa1
	lui	a5,%hi(B4)
	fld	fa0,%lo(B4)(a5)
	fadd.d	fa5,fa5,fa0
	fmul.d	fa5,fa5,fa1
	lui	a5,%hi(B3)
	fld	fa0,%lo(B3)(a5)
	fadd.d	fa5,fa5,fa0
	fmul.d	fa5,fa5,fa1
	lui	a5,%hi(B2)
	fld	fa0,%lo(B2)(a5)
	fadd.d	fa5,fa5,fa0
	fmul.d	fa5,fa5,fa1
	lui	a5,%hi(B1)
	fld	fa0,%lo(B1)(a5)
	fadd.d	fa5,fa5,fa0
	fmul.d	fa5,fa5,fa1
	fadd.d	fa5,fa5,fa2
	fdiv.d	fa4,fa4,fa5
	lui	a5,%hi(.LC23)
	fld	fa1,%lo(.LC23)(a5)
	fdiv.d	fa1,fa3,fa1
	fsd	fa1,160(s1)
	lui	a5,%hi(two)
	fld	fa0,%lo(two)(a5)
	fmul.d	fa5,fa0,fs0
	fadd.d	fa5,fa5,fa4
	fmul.d	fa5,fa5,fs1
	fdiv.d	fa5,fa5,fa0
	lui	a5,%hi(sa)
	fsd	fa5,%lo(sa)(a5)
	lui	a5,%hi(.LC24)
	fld	fa4,%lo(.LC24)(a5)
	lui	a5,%hi(sb)
	fsd	fa4,%lo(sb)(a5)
	fsub.d	fa5,fa5,fa4
	lui	a5,%hi(sc)
	fsd	fa5,%lo(sc)(a5)
	fdiv.d	fa2,fa2,fa1
	fsd	fa2,168(s1)
	fsd	fa2,8(sp)
	lw	a6,8(sp)
	lw	a7,12(sp)
	fsd	fa3,8(sp)
	lw	a4,8(sp)
	lw	a5,12(sp)
	fsd	fa5,8(sp)
	lw	a2,8(sp)
	lw	a3,12(sp)
	lui	a0,%hi(.LC25)
	addi	a0,a0,%lo(.LC25)
	call	printf
	lui	a5,%hi(four)
	fld	fa5,%lo(four)(a5)
	fmul.d	fa5,fs2,fa5
	fld	fs1,%lo(piref)(s3)
	fdiv.d	fs1,fs1,fa5
	mv	a0,s2
	call	dtime
	li	a5,1
	ble	s0,a5,.L39
	lui	a5,%hi(A6)
	fld	fa6,%lo(A6)(a5)
	lui	a5,%hi(A5)
	fld	ft7,%lo(A5)(a5)
	lui	a5,%hi(A4)
	fld	ft6,%lo(A4)(a5)
	lui	a5,%hi(A3)
	fld	ft5,%lo(A3)(a5)
	lui	a5,%hi(A2)
	fld	ft4,%lo(A2)(a5)
	lui	a5,%hi(A1)
	fld	ft3,%lo(A1)(a5)
	lui	a5,%hi(one)
	fld	fa3,%lo(one)(a5)
	lui	a5,%hi(B6)
	fld	ft2,%lo(B6)(a5)
	lui	a5,%hi(B5)
	fld	ft1,%lo(B5)(a5)
	lui	a5,%hi(B4)
	fld	ft0,%lo(B4)(a5)
	lui	a5,%hi(B3)
	fld	fa0,%lo(B3)(a5)
	lui	a5,%hi(B2)
	fld	fa1,%lo(B2)(a5)
	lui	a5,%hi(B1)
	fld	fa2,%lo(B1)(a5)
	li	a5,1
	fcvt.d.w	fs0,x0
.L25:
	fcvt.d.w	fa4,a5
	fmul.d	fa4,fa4,fs1
	fmul.d	fa7,fa4,fa4
	fmul.d	fa5,fa6,fa7
	fadd.d	fa5,fa5,ft7
	fmul.d	fa5,fa5,fa7
	fadd.d	fa5,fa5,ft6
	fmul.d	fa5,fa5,fa7
	fadd.d	fa5,fa5,ft5
	fmul.d	fa5,fa5,fa7
	fadd.d	fa5,fa5,ft4
	fmul.d	fa5,fa5,fa7
	fadd.d	fa5,fa5,ft3
	fmul.d	fa5,fa5,fa7
	fadd.d	fa5,fa5,fa3
	fmul.d	fa5,fa5,fa4
	fmul.d	fa4,ft2,fa7
	fadd.d	fa4,fa4,ft1
	fmul.d	fa4,fa4,fa7
	fadd.d	fa4,fa4,ft0
	fmul.d	fa4,fa4,fa7
	fadd.d	fa4,fa4,fa0
	fmul.d	fa4,fa4,fa7
	fadd.d	fa4,fa4,fa1
	fmul.d	fa4,fa4,fa7
	fadd.d	fa4,fa4,fa2
	fmul.d	fa4,fa4,fa7
	fadd.d	fa4,fa4,fa3
	fmul.d	fa5,fa5,fa4
	fadd.d	fs0,fs0,fa5
	addi	a5,a5,1
	bne	s0,a5,.L25
.L24:
	lui	s1,%hi(.LANCHOR0)
	addi	s1,s1,%lo(.LANCHOR0)
	addi	s2,s1,320
	mv	a0,s2
	call	dtime
	fld	fa3,40(s1)
	fld	fa5,328(s1)
	fmul.d	fa3,fa3,fa5
	lui	a5,%hi(nulltime)
	fld	fa5,%lo(nulltime)(a5)
	fsub.d	fa3,fa3,fa5
	fsd	fa3,176(s1)
	lui	a5,%hi(piref)
	fld	fa5,%lo(piref)(a5)
	lui	a5,%hi(four)
	fld	fa4,%lo(four)(a5)
	fdiv.d	fa5,fa5,fa4
	fmul.d	fa1,fa5,fa5
	lui	s4,%hi(one)
	fld	fa2,%lo(one)(s4)
	lui	a5,%hi(A6)
	fld	fa4,%lo(A6)(a5)
	fmul.d	fa4,fa1,fa4
	lui	a5,%hi(A5)
	fld	fa0,%lo(A5)(a5)
	fadd.d	fa4,fa4,fa0
	fmul.d	fa4,fa4,fa1
	lui	a5,%hi(A4)
	fld	fa0,%lo(A4)(a5)
	fadd.d	fa4,fa4,fa0
	fmul.d	fa4,fa4,fa1
	lui	a5,%hi(A3)
	fld	fa0,%lo(A3)(a5)
	fadd.d	fa4,fa4,fa0
	fmul.d	fa4,fa4,fa1
	lui	a5,%hi(A2)
	fld	fa0,%lo(A2)(a5)
	fadd.d	fa4,fa4,fa0
	fmul.d	fa4,fa4,fa1
	lui	a5,%hi(A1)
	fld	fa0,%lo(A1)(a5)
	fadd.d	fa4,fa4,fa0
	fmul.d	fa4,fa4,fa1
	fadd.d	fa4,fa4,fa2
	fmul.d	fa4,fa4,fa5
	lui	a5,%hi(B6)
	fld	fa5,%lo(B6)(a5)
	fmul.d	fa5,fa1,fa5
	lui	a5,%hi(B5)
	fld	fa0,%lo(B5)(a5)
	fadd.d	fa5,fa5,fa0
	fmul.d	fa5,fa5,fa1
	lui	a5,%hi(B4)
	fld	fa0,%lo(B4)(a5)
	fadd.d	fa5,fa5,fa0
	fmul.d	fa5,fa5,fa1
	lui	a5,%hi(B3)
	fld	fa0,%lo(B3)(a5)
	fadd.d	fa5,fa5,fa0
	fmul.d	fa5,fa5,fa1
	lui	a5,%hi(B2)
	fld	fa0,%lo(B2)(a5)
	fadd.d	fa5,fa5,fa0
	fmul.d	fa5,fa5,fa1
	lui	a5,%hi(B1)
	fld	fa0,%lo(B1)(a5)
	fadd.d	fa5,fa5,fa0
	fmul.d	fa5,fa5,fa1
	fadd.d	fa5,fa5,fa2
	fmul.d	fa4,fa4,fa5
	lui	a5,%hi(.LC23)
	fld	fa1,%lo(.LC23)(a5)
	fdiv.d	fa1,fa3,fa1
	fsd	fa1,184(s1)
	lui	a5,%hi(two)
	fld	fa0,%lo(two)(a5)
	fmul.d	fa5,fa0,fs0
	fadd.d	fa5,fa5,fa4
	fmul.d	fa5,fa5,fs1
	fdiv.d	fa5,fa5,fa0
	lui	s3,%hi(sa)
	fsd	fa5,%lo(sa)(s3)
	lui	a5,%hi(.LC26)
	fld	fa4,%lo(.LC26)(a5)
	lui	a5,%hi(sb)
	fsd	fa4,%lo(sb)(a5)
	fsub.d	fa5,fa5,fa4
	lui	a5,%hi(sc)
	fsd	fa5,%lo(sc)(a5)
	fdiv.d	fa2,fa2,fa1
	fsd	fa2,192(s1)
	fsd	fa2,8(sp)
	lw	a6,8(sp)
	lw	a7,12(sp)
	fsd	fa3,8(sp)
	lw	a4,8(sp)
	lw	a5,12(sp)
	fsd	fa5,8(sp)
	lw	a2,8(sp)
	lw	a3,12(sp)
	lui	a0,%hi(.LC27)
	addi	a0,a0,%lo(.LC27)
	call	printf
	fld	fs0,%lo(one)(s4)
	lui	a5,%hi(.LC28)
	fld	fs1,%lo(.LC28)(a5)
	fsd	fs1,%lo(sa)(s3)
	fdiv.d	fs1,fs1,fs2
	mv	a0,s2
	call	dtime
	li	a5,1
	ble	s0,a5,.L40
	fcvt.d.w	fs2,x0
.L27:
	fcvt.d.w	fa5,a5
	fmul.d	fa5,fa5,fs1
	fmul.d	fa3,fa5,fa5
	fadd.d	fa4,fs0,fa5
	fdiv.d	fa4,fs0,fa4
	fsub.d	fa4,fs2,fa4
	fadd.d	fa2,fs0,fa3
	fdiv.d	fa2,fa5,fa2
	fsub.d	fa4,fa4,fa2
	fmul.d	fa5,fa5,fa3
	fadd.d	fa5,fa5,fs0
	fdiv.d	fa3,fa3,fa5
	fsub.d	fs2,fa4,fa3
	addi	a5,a5,1
	bne	s0,a5,.L27
.L26:
	lui	s0,%hi(.LANCHOR0)
	addi	s0,s0,%lo(.LANCHOR0)
	addi	s2,s0,320
	mv	a0,s2
	call	dtime
	fld	fa3,40(s0)
	fld	fa5,328(s0)
	fmul.d	fa3,fa3,fa5
	lui	a5,%hi(nulltime)
	fld	fa5,%lo(nulltime)(a5)
	fsub.d	fa3,fa3,fa5
	fsd	fa3,200(s0)
	lui	a5,%hi(.LC29)
	fld	fa1,%lo(.LC29)(a5)
	fdiv.d	fa1,fa3,fa1
	fsd	fa1,208(s0)
	lui	a5,%hi(sa)
	fld	fa4,%lo(sa)(a5)
	fmul.d	fa2,fa4,fa4
	fneg.d	fa5,fs0
	fadd.d	fa0,fs0,fa4
	fdiv.d	fa0,fs0,fa0
	fsub.d	fa5,fa5,fa0
	fadd.d	fa0,fs0,fa2
	fdiv.d	fa0,fa4,fa0
	fsub.d	fa5,fa5,fa0
	fmul.d	fa4,fa4,fa2
	fadd.d	fa4,fa4,fs0
	fdiv.d	fa2,fa2,fa4
	fsub.d	fa5,fa5,fa2
	lui	a4,%hi(two)
	fld	fa4,%lo(two)(a4)
	fmul.d	fs2,fs2,fa4
	fadd.d	fa5,fa5,fs2
	lui	a4,%hi(.LC30)
	fld	fa4,%lo(.LC30)(a4)
	fmul.d	fs1,fs1,fa4
	fmul.d	fa5,fa5,fs1
	fsd	fa5,%lo(sa)(a5)
	fcvt.w.d a5,fa5,rtz
	li	a4,-2000
	mul	a5,a5,a4
	fcvt.d.w	fa4,a5
	lui	a5,%hi(scale)
	fld	fa2,%lo(scale)(a5)
	fdiv.d	fa4,fa4,fa2
	fcvt.w.d s1,fa4,rtz
	lui	a5,%hi(.LC31)
	fld	fa4,%lo(.LC31)(a5)
	fadd.d	fa5,fa5,fa4
	lui	a5,%hi(sc)
	fsd	fa5,%lo(sc)(a5)
	lui	a5,%hi(one)
	fld	fa4,%lo(one)(a5)
	fdiv.d	fa4,fa4,fa1
	fsd	fa4,216(s0)
	fsd	fa4,8(sp)
	lw	a6,8(sp)
	lw	a7,12(sp)
	fsd	fa3,8(sp)
	lw	a4,8(sp)
	lw	a5,12(sp)
	fsd	fa5,8(sp)
	lw	a2,8(sp)
	lw	a3,12(sp)
	lui	a0,%hi(.LC32)
	addi	a0,a0,%lo(.LC32)
	call	printf
	fcvt.d.w	fa5,s1
	lui	a5,%hi(three)
	fld	fa4,%lo(three)(a5)
	fmul.d	fa5,fa5,fa4
	lui	a5,%hi(piref)
	fld	fs1,%lo(piref)(a5)
	fdiv.d	fs1,fs1,fa5
	mv	a0,s2
	call	dtime
	li	a5,1
	ble	s1,a5,.L41
	lui	a5,%hi(B6)
	fld	fa6,%lo(B6)(a5)
	lui	a5,%hi(B5)
	fld	ft7,%lo(B5)(a5)
	lui	a5,%hi(B4)
	fld	ft6,%lo(B4)(a5)
	lui	a5,%hi(B3)
	fld	ft5,%lo(B3)(a5)
	lui	a5,%hi(B2)
	fld	ft4,%lo(B2)(a5)
	lui	a5,%hi(B1)
	fld	ft3,%lo(B1)(a5)
	lui	a5,%hi(one)
	fld	fa3,%lo(one)(a5)
	lui	a5,%hi(A6)
	fld	ft2,%lo(A6)(a5)
	lui	a5,%hi(A5)
	fld	ft1,%lo(A5)(a5)
	lui	a5,%hi(A4)
	fld	ft0,%lo(A4)(a5)
	lui	a5,%hi(A3)
	fld	fa0,%lo(A3)(a5)
	lui	a5,%hi(A2)
	fld	fa1,%lo(A2)(a5)
	lui	a5,%hi(A1)
	fld	fa2,%lo(A1)(a5)
	li	a5,1
	fcvt.d.w	fs0,x0
.L29:
	fcvt.d.w	fa7,a5
	fmul.d	fa7,fa7,fs1
	fmul.d	ft8,fa7,fa7
	fmul.d	fa5,fa6,ft8
	fadd.d	fa5,fa5,ft7
	fmul.d	fa5,fa5,ft8
	fadd.d	fa5,fa5,ft6
	fmul.d	fa5,fa5,ft8
	fadd.d	fa5,fa5,ft5
	fmul.d	fa5,fa5,ft8
	fadd.d	fa5,fa5,ft4
	fmul.d	fa5,fa5,ft8
	fadd.d	fa5,fa5,ft3
	fmul.d	fa5,fa5,ft8
	fadd.d	fa5,fa5,fa3
	fmul.d	fa4,ft2,ft8
	fadd.d	fa4,fa4,ft1
	fmul.d	fa4,fa4,ft8
	fadd.d	fa4,fa4,ft0
	fmul.d	fa4,fa4,ft8
	fadd.d	fa4,fa4,fa0
	fmul.d	fa4,fa4,ft8
	fadd.d	fa4,fa4,fa1
	fmul.d	fa4,fa4,ft8
	fadd.d	fa4,fa4,fa2
	fmul.d	fa4,fa4,ft8
	fadd.d	fa4,fa4,fa3
	fmul.d	fa5,fa5,fa5
	fmul.d	fa5,fa5,fa7
	fmul.d	fa5,fa4,fa5
	fadd.d	fs0,fs0,fa5
	addi	a5,a5,1
	bne	s1,a5,.L29
.L28:
	lui	s0,%hi(.LANCHOR0)
	addi	s0,s0,%lo(.LANCHOR0)
	addi	a0,s0,320
	call	dtime
	fld	fa3,40(s0)
	fld	fa5,328(s0)
	fmul.d	fa3,fa3,fa5
	lui	s2,%hi(nulltime)
	fld	fa5,%lo(nulltime)(s2)
	fsub.d	fa3,fa3,fa5
	fsd	fa3,224(s0)
	lui	a5,%hi(piref)
	fld	fa0,%lo(piref)(a5)
	lui	a5,%hi(three)
	fld	fa5,%lo(three)(a5)
	fdiv.d	fa0,fa0,fa5
	fmul.d	fa1,fa0,fa0
	lui	s3,%hi(one)
	fld	fa2,%lo(one)(s3)
	lui	a5,%hi(B6)
	fld	fa4,%lo(B6)(a5)
	fmul.d	fa4,fa1,fa4
	lui	a5,%hi(B5)
	fld	fa5,%lo(B5)(a5)
	fadd.d	fa4,fa4,fa5
	fmul.d	fa4,fa4,fa1
	lui	a5,%hi(B4)
	fld	fa5,%lo(B4)(a5)
	fadd.d	fa4,fa4,fa5
	fmul.d	fa4,fa4,fa1
	lui	a5,%hi(B3)
	fld	fa5,%lo(B3)(a5)
	fadd.d	fa4,fa4,fa5
	fmul.d	fa4,fa4,fa1
	lui	a5,%hi(B2)
	fld	fa5,%lo(B2)(a5)
	fadd.d	fa4,fa4,fa5
	fmul.d	fa4,fa4,fa1
	lui	a5,%hi(B1)
	fld	fa5,%lo(B1)(a5)
	fadd.d	fa4,fa4,fa5
	fmul.d	fa4,fa4,fa1
	fadd.d	fa4,fa4,fa2
	lui	a5,%hi(A6)
	fld	fa5,%lo(A6)(a5)
	fmul.d	fa5,fa1,fa5
	lui	a5,%hi(A5)
	fld	ft0,%lo(A5)(a5)
	fadd.d	fa5,fa5,ft0
	fmul.d	fa5,fa5,fa1
	lui	a5,%hi(A4)
	fld	ft0,%lo(A4)(a5)
	fadd.d	fa5,fa5,ft0
	fmul.d	fa5,fa5,fa1
	lui	a5,%hi(A3)
	fld	ft0,%lo(A3)(a5)
	fadd.d	fa5,fa5,ft0
	fmul.d	fa5,fa5,fa1
	lui	a5,%hi(A2)
	fld	ft0,%lo(A2)(a5)
	fadd.d	fa5,fa5,ft0
	fmul.d	fa5,fa5,fa1
	lui	a5,%hi(A1)
	fld	ft0,%lo(A1)(a5)
	fadd.d	fa5,fa5,ft0
	fmul.d	fa5,fa5,fa1
	fadd.d	fa5,fa5,fa2
	fmul.d	fa5,fa5,fa0
	fmul.d	fa5,fa5,fa4
	fmul.d	fa5,fa5,fa4
	lui	a5,%hi(.LC33)
	fld	fa4,%lo(.LC33)(a5)
	fdiv.d	fa4,fa3,fa4
	fsd	fa4,232(s0)
	lui	a5,%hi(two)
	fld	fa1,%lo(two)(a5)
	fmul.d	fs0,fa1,fs0
	fadd.d	fa5,fs0,fa5
	fmul.d	fa5,fa5,fs1
	fdiv.d	fa5,fa5,fa1
	lui	a5,%hi(sa)
	fsd	fa5,%lo(sa)(a5)
	lui	a5,%hi(.LC34)
	fld	fa1,%lo(.LC34)(a5)
	lui	a5,%hi(sb)
	fsd	fa1,%lo(sb)(a5)
	fsub.d	fa5,fa5,fa1
	lui	a5,%hi(sc)
	fsd	fa5,%lo(sc)(a5)
	fdiv.d	fa2,fa2,fa4
	fsd	fa2,240(s0)
	fsd	fa2,8(sp)
	lw	a6,8(sp)
	lw	a7,12(sp)
	fsd	fa3,8(sp)
	lw	a4,8(sp)
	lw	a5,12(sp)
	fsd	fa5,8(sp)
	lw	a2,8(sp)
	lw	a3,12(sp)
	lui	a0,%hi(.LC35)
	addi	a0,a0,%lo(.LC35)
	call	printf
	fld	fa4,104(s0)
	fld	fa5,80(s0)
	fld	fa3,72(s0)
	fsub.d	fa5,fa5,fa3
	lui	a5,%hi(five)
	fld	fa3,%lo(five)(a5)
	fmul.d	fa5,fa5,fa3
	fadd.d	fa5,fa5,fa4
	lui	a5,%hi(.LC36)
	fld	fa3,%lo(.LC36)(a5)
	fdiv.d	fa5,fa5,fa3
	fsd	fa5,248(s0)
	fld	fa2,%lo(one)(s3)
	fdiv.d	fa5,fa2,fa5
	fsd	fa5,256(s0)
	fld	fa0,128(s0)
	fld	fa1,176(s0)
	fld	fa5,48(s0)
	fadd.d	fa5,fa4,fa5
	fadd.d	fa5,fa5,fa0
	fld	fa3,152(s0)
	fadd.d	fa5,fa5,fa3
	fadd.d	fa5,fa5,fa1
	fld	ft0,200(s0)
	lui	a5,%hi(four)
	fld	fa3,%lo(four)(a5)
	fmul.d	fa3,ft0,fa3
	fadd.d	fa3,fa3,fa5
	lui	a5,%hi(.LC37)
	fld	ft1,%lo(.LC37)(a5)
	fdiv.d	fa3,fa3,ft1
	fsd	fa3,264(s0)
	fdiv.d	fa3,fa2,fa3
	fsd	fa3,272(s0)
	fld	fa3,224(s0)
	fadd.d	fa5,fa5,ft0
	fadd.d	fa5,fa5,fa3
	lui	a5,%hi(.LC38)
	fld	ft0,%lo(.LC38)(a5)
	fdiv.d	fa5,fa5,ft0
	fsd	fa5,280(s0)
	fdiv.d	fa5,fa2,fa5
	fsd	fa5,288(s0)
	fadd.d	fa5,fa4,fa0
	fadd.d	fa5,fa5,fa1
	fadd.d	fa5,fa5,fa3
	lui	a5,%hi(.LC39)
	fld	fa4,%lo(.LC39)(a5)
	fdiv.d	fa5,fa5,fa4
	fsd	fa5,296(s0)
	fdiv.d	fa2,fa2,fa5
	fsd	fa2,304(s0)
	li	a0,10
	call	putchar
	mv	a1,s1
	lui	a0,%hi(.LC40)
	addi	a0,a0,%lo(.LC40)
	call	printf
	lw	a2,%lo(nulltime)(s2)
	lw	a3,%lo(nulltime+4)(s2)
	lui	a0,%hi(.LC41)
	addi	a0,a0,%lo(.LC41)
	call	printf
	lw	a2,256(s0)
	lw	a3,260(s0)
	lui	a0,%hi(.LC42)
	addi	a0,a0,%lo(.LC42)
	call	printf
	lw	a2,272(s0)
	lw	a3,276(s0)
	lui	a0,%hi(.LC43)
	addi	a0,a0,%lo(.LC43)
	call	printf
	lw	a2,288(s0)
	lw	a3,292(s0)
	lui	a0,%hi(.LC44)
	addi	a0,a0,%lo(.LC44)
	call	printf
	lw	a2,304(s0)
	lw	a3,308(s0)
	lui	a0,%hi(.LC45)
	addi	a0,a0,%lo(.LC45)
	call	printf
	li	a0,0
	lw	ra,108(sp)
	lw	s0,104(sp)
	lw	s1,100(sp)
	lw	s2,96(sp)
	lw	s3,92(sp)
	lw	s4,88(sp)
	lw	s5,84(sp)
	lw	s6,80(sp)
	lw	s7,76(sp)
	lw	s8,72(sp)
	lw	s9,68(sp)
	fld	fs0,56(sp)
	fld	fs1,48(sp)
	fld	fs2,40(sp)
	fld	fs3,32(sp)
	fld	fs4,24(sp)
	addi	sp,sp,112
	jr	ra
	.size	main, .-main
	.globl	rusage
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
.LC0:
	.word	-1598689907
	.word	1051772663
	.align	3
.LC2:
	.word	0
	.word	1078984704
	.align	3
.LC3:
	.word	0
	.word	1076756480
	.align	3
.LC4:
	.word	1413754136
	.word	1074340347
	.align	3
.LC5:
	.word	0
	.word	1072693248
	.align	3
.LC6:
	.word	0
	.word	1073741824
	.align	3
.LC7:
	.word	0
	.word	1074266112
	.align	3
.LC8:
	.word	0
	.word	1074790400
	.align	3
.LC9:
	.word	0
	.word	1075052544
	.align	3
.LC12:
	.word	0
	.word	1093567616
	.align	3
.LC13:
	.word	0
	.word	1076625408
	.align	3
.LC14:
	.word	858993459
	.word	1077490483
	.align	3
.LC16:
	.word	0
	.word	1075576832
	.align	3
.LC17:
	.word	0
	.word	1077886976
	.align	3
.LC19:
	.word	0
	.word	1076953088
	.align	3
.LC20:
	.word	0
	.word	1071644672
	.align	3
.LC23:
	.word	0
	.word	1077739520
	.align	3
.LC24:
	.word	-17155601
	.word	1072049730
	.align	3
.LC26:
	.word	0
	.word	1070596096
	.align	3
.LC28:
	.word	-135163228
	.word	1079612737
	.align	3
.LC29:
	.word	0
	.word	1076363264
	.align	3
.LC30:
	.word	0
	.word	1077018624
	.align	3
.LC31:
	.word	858993459
	.word	1082082099
	.align	3
.LC33:
	.word	0
	.word	1077805056
	.align	3
.LC34:
	.word	-1431655765
	.word	1070770858
	.align	3
.LC36:
	.word	0
	.word	1078591488
	.align	3
.LC37:
	.word	0
	.word	1080229888
	.align	3
.LC38:
	.word	0
	.word	1080180736
	.align	3
.LC39:
	.word	0
	.word	1079427072
	.bss
	.align	3
	.set	.LANCHOR0,. + 0
	.type	rusage, @object
	.size	rusage, 32
rusage:
	.zero	32
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
	.globl	__floatdidf
	.ident	"GCC: (crosstool-NG 1.27.0.68_b286f6b) 14.3.0"
	.section	.note.GNU-stack,"",@progbits
