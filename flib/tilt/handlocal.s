	.file	"bpsumlocal.f"
	.version	"01.01"
gcc2_compiled.:
.text
	.align 4
.globl bpsumlocal_
	.type	 bpsumlocal_,@function
bpsumlocal_:
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%edi
	pushl	%esi
	pushl	%ebx
	subl	$168, %esp
	movl	48(%ebp), %edx
	movl	$5, %ecx
	movl	(%edx), %eax
	movl	%eax, -84(%ebp)
	movl	52(%ebp), %edx
	movl	(%edx), %eax
	movl	%eax, %ebx
	subl	-84(%ebp), %ebx
	incl	%ebx
	movl	%ebx, %eax
	cltd
	idivl	%ecx
	movl	%eax, %ecx
	imull	$5, %ecx, %eax
	addl	-84(%ebp), %eax
	movl	%eax, -24(%ebp)
	imull	$5, %ecx, %ecx
	movl	%ecx, %eax
	movl	$5, %ecx
	cltd
	idivl	%ecx
	movl	%eax, -88(%ebp)
	movl	-84(%ebp), %eax
	movl	%eax, -28(%ebp)
	decl	-88(%ebp)
	js	.L4
	movl	16(%ebp), %edx
	flds	(%edx)
	fstps	-96(%ebp)
	movl	44(%ebp), %eax
	movl	(%eax), %eax
	movl	%eax, -108(%ebp)
	movl	36(%ebp), %eax
	movl	(%eax), %eax
	movl	%eax, -112(%ebp)
	movl	40(%ebp), %eax
	movl	(%eax), %eax
	movl	%eax, -116(%ebp)
	.p2align 2
.L6:
	movl	-28(%ebp), %edx
	leal	-4(,%edx,4), %eax
	flds	-96(%ebp)
	movl	24(%ebp), %ecx
	fmuls	(%eax,%ecx)
	movl	20(%ebp), %edx
	fadds	(%eax,%edx)
	flds	-96(%ebp)
	movl	32(%ebp), %ecx
	fmuls	(%eax,%ecx)
	movl	28(%ebp), %edx
	fadds	(%eax,%edx)
	movl	-28(%ebp), %eax
	sall	$2, %eax
	flds	-96(%ebp)
	movl	24(%ebp), %ecx
	fmuls	(%eax,%ecx)
	movl	20(%ebp), %edx
	fadds	(%eax,%edx)
	flds	-96(%ebp)
	movl	32(%ebp), %ecx
	fmuls	(%eax,%ecx)
	movl	28(%ebp), %edx
	fadds	(%eax,%edx)
	movl	-28(%ebp), %ecx
	leal	4(,%ecx,4), %eax
	flds	-96(%ebp)
	movl	24(%ebp), %edx
	fmuls	(%eax,%edx)
	movl	20(%ebp), %edx
	fadds	(%eax,%edx)
	fstps	-32(%ebp)
	flds	-96(%ebp)
	movl	32(%ebp), %ecx
	fmuls	(%eax,%ecx)
	movl	28(%ebp), %edx
	fadds	(%eax,%edx)
	fstps	-36(%ebp)
	movl	-28(%ebp), %ecx
	leal	8(,%ecx,4), %eax
	flds	-96(%ebp)
	movl	24(%ebp), %edx
	fmuls	(%eax,%edx)
	movl	20(%ebp), %ecx
	fadds	(%eax,%ecx)
	fstps	-40(%ebp)
	flds	-96(%ebp)
	movl	32(%ebp), %edx
	fmuls	(%eax,%edx)
	movl	28(%ebp), %ecx
	fadds	(%eax,%ecx)
	fstps	-44(%ebp)
	movl	-28(%ebp), %edx
	leal	12(,%edx,4), %eax
	flds	-96(%ebp)
	movl	24(%ebp), %ecx
	fmuls	(%eax,%ecx)
	movl	20(%ebp), %edx
	fadds	(%eax,%edx)
	fstps	-48(%ebp)
	flds	-96(%ebp)
	movl	32(%ebp), %ecx
	fmuls	(%eax,%ecx)
	movl	28(%ebp), %edx
	fadds	(%eax,%edx)
	fstps	-52(%ebp)
	fxch	%st(3)
	fnstcw	-16(%ebp)
	movl	-16(%ebp), %ecx
	movb	$12, -15(%ebp)
	fldcw	-16(%ebp)
	movl	%ecx, -16(%ebp)
	fistl	-56(%ebp)
#	fldcw	-16(%ebp)
	fxch	%st(2)
#	fnstcw	-16(%ebp)
#	movl	-16(%ebp), %edx
#	movb	$12, -15(%ebp)
#	fldcw	-16(%ebp)
#	movl	%edx, -16(%ebp)
	fistl	-20(%ebp)
#	fldcw	-16(%ebp)
	fxch	%st(1)
	movl	-20(%ebp), %eax
#	fnstcw	-16(%ebp)
#	movl	-16(%ebp), %ecx
#	movb	$12, -15(%ebp)
#	fldcw	-16(%ebp)
#	movl	%ecx, -16(%ebp)
	fistl	-60(%ebp)
#	fldcw	-16(%ebp)
	fxch	%st(3)
#	fnstcw	-16(%ebp)
#	movl	-16(%ebp), %edx
#	movb	$12, -15(%ebp)
#	fldcw	-16(%ebp)
#	movl	%edx, -16(%ebp)
	fistl	-20(%ebp)
#	fldcw	-16(%ebp)
	movl	-20(%ebp), %ecx
	flds	-32(%ebp)
#	fnstcw	-16(%ebp)
#	movl	-16(%ebp), %edx
#	movb	$12, -15(%ebp)
#	fldcw	-16(%ebp)
#	movl	%edx, -16(%ebp)
	fistpl	-64(%ebp)
#	fldcw	-16(%ebp)
	flds	-36(%ebp)
#	fnstcw	-16(%ebp)
#	movl	-16(%ebp), %edx
#	movb	$12, -15(%ebp)
#	fldcw	-16(%ebp)
#	movl	%edx, -16(%ebp)
	fistpl	-20(%ebp)
#	fldcw	-16(%ebp)
	movl	-20(%ebp), %ebx
	flds	-40(%ebp)
#	fnstcw	-16(%ebp)
#	movl	-16(%ebp), %edx
#	movb	$12, -15(%ebp)
#	fldcw	-16(%ebp)
#	movl	%edx, -16(%ebp)
	fistpl	-68(%ebp)
#	fldcw	-16(%ebp)
	flds	-44(%ebp)
#	fnstcw	-16(%ebp)
#	movl	-16(%ebp), %edx
#	movb	$12, -15(%ebp)
#	fldcw	-16(%ebp)
#	movl	%edx, -16(%ebp)
	fistpl	-20(%ebp)
#	fldcw	-16(%ebp)
	movl	-20(%ebp), %esi
	flds	-48(%ebp)
#	fnstcw	-16(%ebp)
#	movl	-16(%ebp), %edx
#	movb	$12, -15(%ebp)
#	fldcw	-16(%ebp)
#	movl	%edx, -16(%ebp)
	fistpl	-72(%ebp)
#	fldcw	-16(%ebp)
	flds	-52(%ebp)
#	fnstcw	-16(%ebp)
#	movl	-16(%ebp), %edx
#	movb	$12, -15(%ebp)
#	fldcw	-16(%ebp)
#	movl	%edx, -16(%ebp)
	fistpl	-76(%ebp)
	fldcw	-16(%ebp)
	fxch	%st(2)
	fisubl	-56(%ebp)
	fxch	%st(1)
	pushl	%eax
	fisubl	(%esp)
	popl	%eax
	subl	-108(%ebp), %eax
	movl	40(%ebp), %edx
	imull	(%edx), %eax
	addl	-112(%ebp), %eax
	movl	-56(%ebp), %edx
	addl	%edx, %eax
	movl	%eax, -180(%ebp)
	movl	%eax, %edi
	addl	-116(%ebp), %edi
	movl	12(%ebp), %eax
	movl	(%eax), %eax
	leal	-4(,%eax,4), %edx
	movl	%edx, -120(%ebp)
	fld1
	fsub	%st(1), %st
	fld1
	fsub	%st(3), %st
	movl	-180(%ebp), %eax
	sall	$2, %eax
	fld	%st(0)
	movl	8(%ebp), %edx
	fmuls	-4(%edx,%eax)
	fld	%st(4)
	fmuls	(%eax,%edx)
	faddp	%st, %st(1)
	fmulp	%st, %st(2)
	fxch	%st(1)
	movl	-120(%ebp), %eax
	fadds	(%eax,%edx)
	fxch	%st(1)
	leal	0(,%edi,4), %eax
	fmuls	-4(%edx,%eax)
	fxch	%st(3)
	fmuls	(%eax,%edx)
	faddp	%st, %st(3)
	fxch	%st(1)
	fmulp	%st, %st(2)
	faddp	%st, %st(1)
	movl	-120(%ebp), %eax
	fstps	(%eax,%edx)
	fxch	%st(1)
	fisubl	-60(%ebp)
	fxch	%st(1)
	pushl	%ecx
	fisubl	(%esp)
	popl	%ecx
	subl	-108(%ebp), %ecx
	movl	40(%ebp), %edx
	imull	(%edx), %ecx
	addl	-112(%ebp), %ecx
	movl	-60(%ebp), %eax
	addl	%eax, %ecx
	movl	%ecx, %edi
	addl	-116(%ebp), %edi
	movl	12(%ebp), %eax
	movl	(%eax), %eax
	sall	$2, %eax
	movl	%eax, -124(%ebp)
	fld1
	fsub	%st(1), %st
	fld1
	fsub	%st(3), %st
	movl	%ecx, %eax
	sall	$2, %eax
	fld	%st(0)
	movl	8(%ebp), %edx
	fmuls	-4(%edx,%eax)
	fld	%st(4)
	fmuls	(%eax,%edx)
	faddp	%st, %st(1)
	fmulp	%st, %st(2)
	fxch	%st(1)
	movl	-124(%ebp), %ecx
	fadds	(%ecx,%edx)
	fxch	%st(1)
	leal	0(,%edi,4), %eax
	fmuls	-4(%edx,%eax)
	fxch	%st(3)
	fmuls	(%eax,%edx)
	faddp	%st, %st(3)
	fxch	%st(1)
	fmulp	%st, %st(2)
	faddp	%st, %st(1)
	fstps	(%ecx,%edx)
	movl	12(%ebp), %eax
	movl	(%eax), %edx
	addl	$2, %edx
	movl	%edx, (%eax)
	flds	-32(%ebp)
	fisubl	-64(%ebp)
	flds	-36(%ebp)
	pushl	%ebx
	fisubl	(%esp)
	popl	%ebx
	subl	-108(%ebp), %ebx
	movl	40(%ebp), %ecx
	imull	(%ecx), %ebx
	addl	-112(%ebp), %ebx
	movl	%ebx, -180(%ebp)
	movl	-64(%ebp), %eax
	addl	%eax, -180(%ebp)
	movl	-180(%ebp), %edi
	addl	-116(%ebp), %edi
	leal	-4(,%edx,4), %edx
	fld1
	fsub	%st(1), %st
	fld1
	fsub	%st(3), %st
	movl	-180(%ebp), %eax
	sall	$2, %eax
	fld	%st(0)
	movl	8(%ebp), %ecx
	fmuls	-4(%ecx,%eax)
	fld	%st(4)
	fmuls	(%eax,%ecx)
	faddp	%st, %st(1)
	fmulp	%st, %st(2)
	fxch	%st(1)
	fadds	(%edx,%ecx)
	fxch	%st(1)
	leal	0(,%edi,4), %eax
	fmuls	-4(%ecx,%eax)
	fxch	%st(3)
	fmuls	(%eax,%ecx)
	faddp	%st, %st(3)
	fxch	%st(1)
	fmulp	%st, %st(2)
	faddp	%st, %st(1)
	fstps	(%edx,%ecx)
	movl	12(%ebp), %eax
	movl	(%eax), %edx
	incl	%edx
	movl	%edx, (%eax)
	flds	-40(%ebp)
	fisubl	-68(%ebp)
	flds	-44(%ebp)
	pushl	%esi
	fisubl	(%esp)
	popl	%esi
	subl	-108(%ebp), %esi
	movl	40(%ebp), %ecx
	imull	(%ecx), %esi
	addl	-112(%ebp), %esi
	movl	%esi, -180(%ebp)
	movl	-68(%ebp), %eax
	addl	%eax, -180(%ebp)
	movl	-180(%ebp), %edi
	addl	-116(%ebp), %edi
	leal	-4(,%edx,4), %edx
	fld1
	fsub	%st(1), %st
	fld1
	fsub	%st(3), %st
	movl	-180(%ebp), %eax
	sall	$2, %eax
	fld	%st(0)
	movl	8(%ebp), %ecx
	fmuls	-4(%ecx,%eax)
	fld	%st(4)
	fmuls	(%eax,%ecx)
	faddp	%st, %st(1)
	fmulp	%st, %st(2)
	fxch	%st(1)
	fadds	(%edx,%ecx)
	fxch	%st(1)
	leal	0(,%edi,4), %eax
	fmuls	-4(%ecx,%eax)
	fxch	%st(3)
	fmuls	(%eax,%ecx)
	faddp	%st, %st(3)
	fxch	%st(1)
	fmulp	%st, %st(2)
	faddp	%st, %st(1)
	fstps	(%edx,%ecx)
	movl	12(%ebp), %eax
	movl	(%eax), %edx
	incl	%edx
	movl	%edx, -144(%ebp)
	movl	%edx, (%eax)
	flds	-48(%ebp)
	fisubl	-72(%ebp)
	flds	-52(%ebp)
	fisubl	-76(%ebp)
	movl	-76(%ebp), %ecx
	subl	-108(%ebp), %ecx
	movl	40(%ebp), %eax
	imull	(%eax), %ecx
	addl	-112(%ebp), %ecx
	movl	-72(%ebp), %edx
	addl	%edx, %ecx
	movl	%ecx, -180(%ebp)
	movl	%ecx, %edi
	addl	-116(%ebp), %edi
	movl	-144(%ebp), %ecx
	leal	-4(,%ecx,4), %ecx
	fld1
	fsub	%st(1), %st
	fld1
	fsub	%st(3), %st
	movl	-180(%ebp), %eax
	sall	$2, %eax
	fld	%st(0)
	movl	8(%ebp), %edx
	fmuls	-4(%edx,%eax)
	fld	%st(4)
	fmuls	(%eax,%edx)
	faddp	%st, %st(1)
	fmulp	%st, %st(2)
	fxch	%st(1)
	fadds	(%ecx,%edx)
	fxch	%st(1)
	leal	0(,%edi,4), %eax
	fmuls	-4(%edx,%eax)
	fxch	%st(3)
	fmuls	(%eax,%edx)
	faddp	%st, %st(3)
	fxch	%st(1)
	fmulp	%st, %st(2)
	faddp	%st, %st(1)
	fstps	(%ecx,%edx)
	movl	12(%ebp), %ecx
	incl	(%ecx)
	addl	$5, -28(%ebp)
	decl	-88(%ebp)
	jns	.L6
.L4:
	movl	52(%ebp), %edx
	movl	(%edx), %eax
	movl	-24(%ebp), %ecx
	movl	%ecx, -28(%ebp)
	movl	%eax, %ecx
	subl	-24(%ebp), %ecx
	js	.L9
	movl	16(%ebp), %eax
	flds	(%eax)
	movl	44(%ebp), %eax
	movl	(%eax), %eax
	movl	%eax, -160(%ebp)
	movl	36(%ebp), %edx
	movl	(%edx), %esi
	movl	40(%ebp), %eax
	movl	(%eax), %ebx
	fld1
	.p2align 2
.L11:
	movl	-28(%ebp), %edx
	leal	-4(,%edx,4), %eax
	fld	%st(1)
	movl	24(%ebp), %edx
	fmuls	(%eax,%edx)
	movl	20(%ebp), %edx
	fadds	(%eax,%edx)
	fld	%st(2)
	movl	32(%ebp), %edx
	fmuls	(%eax,%edx)
	movl	28(%ebp), %edx
	fadds	(%eax,%edx)
	fxch	%st(1)
	fnstcw	-16(%ebp)
	movl	-16(%ebp), %eax
	movb	$12, -15(%ebp)
	fldcw	-16(%ebp)
	movl	%eax, -16(%ebp)
	fistl	-80(%ebp)
	fldcw	-16(%ebp)
	fisubl	-80(%ebp)
	fxch	%st(1)
	fnstcw	-16(%ebp)
	movl	-16(%ebp), %edx
	movb	$12, -15(%ebp)
	fldcw	-16(%ebp)
	movl	%edx, -16(%ebp)
	fistl	-20(%ebp)
	fldcw	-16(%ebp)
	movl	-20(%ebp), %eax
	pushl	%eax
	fisubl	(%esp)
	popl	%eax
	subl	-160(%ebp), %eax
	movl	40(%ebp), %edx
	imull	(%edx), %eax
	addl	%esi, %eax
	movl	-80(%ebp), %edx
	addl	%edx, %eax
	movl	%eax, -180(%ebp)
	movl	%eax, %edi
	addl	%ebx, %edi
	movl	12(%ebp), %eax
	movl	(%eax), %eax
	leal	-4(,%eax,4), %edx
	movl	%edx, -164(%ebp)
	fld	%st(2)
	fsub	%st(1), %st
	fld	%st(3)
	fsub	%st(3), %st
	movl	-180(%ebp), %eax
	sall	$2, %eax
	fld	%st(0)
	movl	8(%ebp), %edx
	fmuls	-4(%edx,%eax)
	fld	%st(4)
	fmuls	(%eax,%edx)
	faddp	%st, %st(1)
	fmulp	%st, %st(2)
	fxch	%st(1)
	movl	-164(%ebp), %eax
	fadds	(%eax,%edx)
	fxch	%st(1)
	leal	0(,%edi,4), %eax
	fmuls	-4(%edx,%eax)
	fxch	%st(3)
	fmuls	(%eax,%edx)
	faddp	%st, %st(3)
	fxch	%st(1)
	fmulp	%st, %st(2)
	faddp	%st, %st(1)
	movl	-164(%ebp), %eax
	fstps	(%eax,%edx)
	movl	12(%ebp), %edx
	incl	(%edx)
	incl	-28(%ebp)
	decl	%ecx
	jns	.L11
	fstp	%st(0)
	fstp	%st(0)
.L9:
	addl	$168, %esp
	popl	%ebx
	popl	%esi
	popl	%edi
	popl	%ebp
	ret
.Lfe1:
	.size	 bpsumlocal_,.Lfe1-bpsumlocal_
	.ident	"GCC: (GNU) 2.96 20000731 (Red Hat Linux 7.0)"
