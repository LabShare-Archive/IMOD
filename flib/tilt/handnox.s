	.file	"bpsumnox.f"
	.version	"01.01"
gcc2_compiled.:
.text
	.align 4
.globl bpsumnox_
	.type	 bpsumnox_,@function
bpsumnox_:
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%edi
	pushl	%esi
	pushl	%ebx
	subl	$108, %esp
	movl	8(%ebp), %ecx
	movl	20(%ebp), %edx
	movl	(%edx), %eax
	testl	%eax, %eax
	jns	.L3
	addl	$7, %eax
.L3:
	sarl	$3, %eax
	leal	0(,%eax,8), %ebx
	movl	(%edx), %edx
	subl	%ebx, %edx
	movl	%edx, -44(%ebp)
	movl	16(%ebp), %edx
	movl	(%edx), %ebx
	incl	%ebx
	movl	%ebx, -48(%ebp)
	decl	%eax
	movl	%eax, -80(%ebp)
	js	.L5
	movl	28(%ebp), %edi
	flds	(%edi)
	fstpl	-88(%ebp)
	movl	(%edx), %eax
	movl	%eax, -92(%ebp)
	.p2align 2
.L7:
	fldl	-88(%ebp)
	movl	24(%ebp), %edx
	faddl	(%edx)
	fldl	-88(%ebp)
	fadd	%st(1), %st
	fldl	-88(%ebp)
	fadd	%st(1), %st
	fldl	-88(%ebp)
	fadd	%st(1), %st
	fldl	-88(%ebp)
	fadd	%st(1), %st
	fldl	-88(%ebp)
	fadd	%st(1), %st
	fldl	-88(%ebp)
	fadd	%st(1), %st
	fstpl	-40(%ebp)
	fldl	(%edx)
	fnstcw	-20(%ebp)
	movl	-20(%ebp), %eax
	movb	$12, -19(%ebp)
	fldcw	-20(%ebp)
	movl	%eax, -20(%ebp)
	fistpl	-24(%ebp)
#	fldcw	-20(%ebp)
	fxch	%st(5)
	movl	-24(%ebp), %edi
#	fnstcw	-20(%ebp)
#	movl	-20(%ebp), %edx
#	movb	$12, -19(%ebp)
#	fldcw	-20(%ebp)
#	movl	%edx, -20(%ebp)
	fistl	-24(%ebp)
#	fldcw	-20(%ebp)
	fxch	%st(4)
	movl	-24(%ebp), %ebx
#	fnstcw	-20(%ebp)
#	movl	-20(%ebp), %eax
#	movb	$12, -19(%ebp)
#	fldcw	-20(%ebp)
#	movl	%eax, -20(%ebp)
	fistl	-24(%ebp)
#	fldcw	-20(%ebp)
	fxch	%st(3)
	movl	-24(%ebp), %esi
#	fnstcw	-20(%ebp)
#	movl	-20(%ebp), %edx
#	movb	$12, -19(%ebp)
#	fldcw	-20(%ebp)
#	movl	%edx, -20(%ebp)
	fistl	-52(%ebp)
#	fldcw	-20(%ebp)
	fxch	%st(2)
#	fnstcw	-20(%ebp)
#	movl	-20(%ebp), %eax
#	movb	$12, -19(%ebp)
#	fldcw	-20(%ebp)
#	movl	%eax, -20(%ebp)
	fistl	-56(%ebp)
#	fldcw	-20(%ebp)
	fxch	%st(1)
#	fnstcw	-20(%ebp)
#	movl	-20(%ebp), %edx
#	movb	$12, -19(%ebp)
#	fldcw	-20(%ebp)
#	movl	%edx, -20(%ebp)
	fistl	-60(%ebp)
#	fldcw	-20(%ebp)
	fxch	%st(5)
#	fnstcw	-20(%ebp)
#	movl	-20(%ebp), %eax
#	movb	$12, -19(%ebp)
#	fldcw	-20(%ebp)
#	movl	%eax, -20(%ebp)
	fistl	-64(%ebp)
#	fldcw	-20(%ebp)
	fldl	-40(%ebp)
#	fnstcw	-20(%ebp)
#	movl	-20(%ebp), %edx
#	movb	$12, -19(%ebp)
#	fldcw	-20(%ebp)
#	movl	%edx, -20(%ebp)
	fistpl	-68(%ebp)
	fldcw	-20(%ebp)
	movl	24(%ebp), %eax
	fldl	(%eax)
	pushl	%edi
	fisubl	(%esp)
	popl	%edi
	fstps	-28(%ebp)
	flds	-28(%ebp)
	fxch	%st(5)
	pushl	%ebx
	fisubl	(%esp)
	popl	%ebx
	fstps	-72(%ebp)
	fxch	%st(3)
	pushl	%esi
	fisubl	(%esp)
	popl	%esi
	fstps	-28(%ebp)
	flds	-28(%ebp)
	fxch	%st(2)
	fisubl	-52(%ebp)
	fstps	-28(%ebp)
	flds	-28(%ebp)
	fxch	%st(1)
	fisubl	-56(%ebp)
	fstps	-28(%ebp)
	flds	-28(%ebp)
	fxch	%st(5)
	fisubl	-60(%ebp)
	fstps	-28(%ebp)
	flds	-28(%ebp)
	fxch	%st(3)
	fisubl	-64(%ebp)
	fstps	-28(%ebp)
	flds	-28(%ebp)
	fldl	-40(%ebp)
	fisubl	-68(%ebp)
	fstps	-76(%ebp)
	movl	12(%ebp), %eax
	movl	(%eax), %eax
	movl	%eax, -108(%ebp)
	leal	-4(,%eax,4), %edx
	fld1
	fsub	%st(5), %st
	movl	-92(%ebp), %eax
	addl	%edi, %eax
	fmuls	-4(%ecx,%eax,4)
	fadds	(%edx,%ecx)
	fxch	%st(5)
	movl	-48(%ebp), %eax
	addl	%edi, %eax
	fmuls	-4(%ecx,%eax,4)
	faddp	%st, %st(5)
	fxch	%st(4)
	fstps	(%edx,%ecx)
	movl	-108(%ebp), %edx
	sall	$2, %edx
	fld1
	fsubs	-72(%ebp)
	movl	-92(%ebp), %edi
	leal	(%edi,%ebx), %eax
	fmuls	-4(%ecx,%eax,4)
	fadds	(%edx,%ecx)
	addl	-48(%ebp), %ebx
	flds	-72(%ebp)
	fmuls	-4(%ecx,%ebx,4)
	faddp	%st, %st(1)
	fstps	(%edx,%ecx)
	movl	-108(%ebp), %eax
	leal	4(,%eax,4), %edx
	fld1
	fsub	%st(2), %st
	leal	(%edi,%esi), %eax
	fmuls	-4(%ecx,%eax,4)
	fadds	(%edx,%ecx)
	fxch	%st(2)
	addl	-48(%ebp), %esi
	fmuls	-4(%ecx,%esi,4)
	faddp	%st, %st(2)
	fxch	%st(1)
	fstps	(%edx,%ecx)
	movl	-108(%ebp), %ebx
	leal	8(,%ebx,4), %edx
	fld1
	fsub	%st(1), %st
	movl	-52(%ebp), %eax
	addl	%edi, %eax
	fmuls	-4(%ecx,%eax,4)
	fadds	(%edx,%ecx)
	fxch	%st(1)
	movl	-48(%ebp), %edi
	addl	-52(%ebp), %edi
	fmuls	-4(%ecx,%edi,4)
	faddp	%st, %st(1)
	fstps	(%edx,%ecx)
	leal	12(,%ebx,4), %edx
	fld1
	fsub	%st(3), %st
	movl	-56(%ebp), %eax
	addl	-92(%ebp), %eax
	fmuls	-4(%ecx,%eax,4)
	fadds	(%edx,%ecx)
	fxch	%st(3)
	movl	-48(%ebp), %eax
	addl	-56(%ebp), %eax
	fmuls	-4(%ecx,%eax,4)
	faddp	%st, %st(3)
	fxch	%st(2)
	fstps	(%edx,%ecx)
	leal	16(,%ebx,4), %edx
	fld1
	fsub	%st(2), %st
	movl	-60(%ebp), %eax
	addl	-92(%ebp), %eax
	fmuls	-4(%ecx,%eax,4)
	fadds	(%edx,%ecx)
	fxch	%st(2)
	movl	-48(%ebp), %eax
	addl	-60(%ebp), %eax
	fmuls	-4(%ecx,%eax,4)
	faddp	%st, %st(2)
	fxch	%st(1)
	fstps	(%edx,%ecx)
	leal	20(,%ebx,4), %edx
	fld1
	fsub	%st(1), %st
	movl	-64(%ebp), %eax
	addl	-92(%ebp), %eax
	fmuls	-4(%ecx,%eax,4)
	fadds	(%edx,%ecx)
	fxch	%st(1)
	movl	-48(%ebp), %eax
	addl	-64(%ebp), %eax
	fmuls	-4(%ecx,%eax,4)
	faddp	%st, %st(1)
	fstps	(%edx,%ecx)
	leal	24(,%ebx,4), %edx
	fld1
	fsubs	-76(%ebp)
	movl	-68(%ebp), %eax
	addl	-92(%ebp), %eax
	fmuls	-4(%ecx,%eax,4)
	fadds	(%edx,%ecx)
	movl	-48(%ebp), %eax
	addl	-68(%ebp), %eax
	flds	-76(%ebp)
	fmuls	-4(%ecx,%eax,4)
	faddp	%st, %st(1)
	fstps	(%edx,%ecx)
	addl	$8, %ebx
	movl	12(%ebp), %eax
	movl	%ebx, (%eax)
	fldl	-40(%ebp)
	faddl	-88(%ebp)
	movl	24(%ebp), %edx
	fstpl	(%edx)
	decl	-80(%ebp)
	jns	.L7
.L5:
	movl	-44(%ebp), %ebx
	decl	%ebx
	js	.L10
	fld1
	movl	16(%ebp), %edi
	movl	(%edi), %esi
	movl	28(%ebp), %eax
	flds	(%eax)
	movl	24(%ebp), %edx
	fldl	(%edx)
	.p2align 2
.L12:
	fnstcw	-20(%ebp)
	movl	-20(%ebp), %eax
	movb	$12, -19(%ebp)
	fldcw	-20(%ebp)
	movl	%eax, -20(%ebp)
	fistl	-24(%ebp)
	fldcw	-20(%ebp)
	movl	-24(%ebp), %edi
	fld	%st(0)
	pushl	%edi
	fisubl	(%esp)
	popl	%edi
	fstps	-28(%ebp)
	flds	-28(%ebp)
	movl	12(%ebp), %eax
	movl	(%eax), %edx
	leal	-4(,%edx,4), %edx
	fld	%st(3)
	fsub	%st(1), %st
	leal	(%esi,%edi), %eax
	fmuls	-4(%ecx,%eax,4)
	fadds	(%edx,%ecx)
	fxch	%st(1)
	movl	-48(%ebp), %eax
	addl	%edi, %eax
	fmuls	-4(%ecx,%eax,4)
	faddp	%st, %st(1)
	fstps	(%edx,%ecx)
	movl	12(%ebp), %edx
	incl	(%edx)
	fadd	%st(1), %st
	decl	%ebx
	jns	.L12
	fstp	%st(1)
	fstp	%st(1)
	movl	24(%ebp), %ebx
	fstpl	(%ebx)
.L10:
	addl	$108, %esp
	popl	%ebx
	popl	%esi
	popl	%edi
	popl	%ebp
	ret
.Lfe1:
	.size	 bpsumnox_,.Lfe1-bpsumnox_
	.ident	"GCC: (GNU) 2.96 20000731 (Red Hat Linux 7.0)"
