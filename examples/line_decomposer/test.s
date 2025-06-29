	.file	"test.cc"
	.option nopic
	.attribute arch, "rv32i2p1_m2p0_a2p1_f2p2_d2p2_c2p0_zicsr2p0_zifencei2p0"
	.attribute unaligned_access, 0
	.attribute stack_align, 16
	.text
	.section	.text._Z7combineIhET_S0_S0_,"axG",@progbits,_Z7combineIhET_S0_S0_,comdat
	.align	1
	.weak	_Z7combineIhET_S0_S0_
	.type	_Z7combineIhET_S0_S0_, @function
_Z7combineIhET_S0_S0_:
.LFB4:
	.cfi_startproc
	add	a0,a0,a1
	andi	a0,a0,0xff
	ret
	.cfi_endproc
.LFE4:
	.size	_Z7combineIhET_S0_S0_, .-_Z7combineIhET_S0_S0_
	.section	.text._Z7combineIaET_S0_S0_,"axG",@progbits,_Z7combineIaET_S0_S0_,comdat
	.align	1
	.weak	_Z7combineIaET_S0_S0_
	.type	_Z7combineIaET_S0_S0_, @function
_Z7combineIaET_S0_S0_:
.LFB5:
	.cfi_startproc
	add	a0,a0,a1
	slli	a0,a0,24
	srai	a0,a0,24
	ret
	.cfi_endproc
.LFE5:
	.size	_Z7combineIaET_S0_S0_, .-_Z7combineIaET_S0_S0_
	.section	.text._Z7combineItET_S0_S0_,"axG",@progbits,_Z7combineItET_S0_S0_,comdat
	.align	1
	.weak	_Z7combineItET_S0_S0_
	.type	_Z7combineItET_S0_S0_, @function
_Z7combineItET_S0_S0_:
.LFB6:
	.cfi_startproc
	add	a0,a0,a1
	slli	a0,a0,16
	srli	a0,a0,16
	ret
	.cfi_endproc
.LFE6:
	.size	_Z7combineItET_S0_S0_, .-_Z7combineItET_S0_S0_
	.section	.text._Z7combineIsET_S0_S0_,"axG",@progbits,_Z7combineIsET_S0_S0_,comdat
	.align	1
	.weak	_Z7combineIsET_S0_S0_
	.type	_Z7combineIsET_S0_S0_, @function
_Z7combineIsET_S0_S0_:
.LFB7:
	.cfi_startproc
	add	a0,a0,a1
	slli	a0,a0,16
	srai	a0,a0,16
	ret
	.cfi_endproc
.LFE7:
	.size	_Z7combineIsET_S0_S0_, .-_Z7combineIsET_S0_S0_
	.ident	"GCC: (crosstool-NG 1.27.0.68_b286f6b) 14.3.0"
	.section	.note.GNU-stack,"",@progbits
