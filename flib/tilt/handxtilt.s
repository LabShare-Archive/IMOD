	.file	"bpsumxtilt.f"
	.version	"01.01"
gcc2_compiled.:
.text
	.align 4
.globl bpsumxtilt_
	.type	 bpsumxtilt_,@function
bpsumxtilt_:
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%edi
	pushl	%esi
	pushl	%ebx
	subl	$164, %esp
	movl	8(%ebp), %edi
	movl	24(%ebp), %edx
	movl	(%edx), %eax
	testl	%eax, %eax
	jns	.L3
	addl	$7, %eax
.L3:
	sarl	$3, %eax
	leal	0(,%eax,8), %ecx
	movl	(%edx), %edx
	subl	%ecx, %edx
	movl	%edx, -60(%ebp)
	decl	%eax
	movl	%eax, -104(%ebp)
	js	.L5
	movl	32(%ebp), %eax
	flds	(%eax)
	fstpl	-112(%ebp)
	movl	16(%ebp), %eax
	movl	(%eax), %eax
	movl	%eax, -132(%ebp)
	movl	20(%ebp), %eax
	movl	(%eax), %eax
	movl	%eax, -136(%ebp)
	movl	40(%ebp), %edx
	flds	(%edx)
	movl	36(%ebp), %ebx
	flds	(%ebx)
	.p2align 2
.L7:
	fldl	-112(%ebp)
	movl	28(%ebp), %esi
	faddl	(%esi)
	fldl	-112(%ebp)
	fadd	%st(1), %st
	fldl	-112(%ebp)
	fadd	%st(1), %st
	fldl	-112(%ebp)
	fadd	%st(1), %st
	fldl	-112(%ebp)
	fadd	%st(1), %st
	fstl	-48(%ebp)
	faddl	-112(%ebp)
	fstl	-56(%ebp)
	faddl	-112(%ebp)
	fstpl	-40(%ebp)
	fldl	(%esi)
	fnstcw	-20(%ebp)
	movl	-20(%ebp), %edx
	movb	$12, -19(%ebp)
	fldcw	-20(%ebp)
	movl	%edx, -20(%ebp)
	fistpl	-24(%ebp)
#	fldcw	-20(%ebp)
	fxch	%st(3)
	movl	-24(%ebp), %eax
#	fnstcw	-20(%ebp)
#	movl	-20(%ebp), %esi
#	movb	$12, -19(%ebp)
#	fldcw	-20(%ebp)
#	movl	%esi, -20(%ebp)
	fistl	-24(%ebp)
#	fldcw	-20(%ebp)
	fxch	%st(2)
	movl	-24(%ebp), %ebx
#	fnstcw	-20(%ebp)
#	movl	-20(%ebp), %edx
#	movb	$12, -19(%ebp)
#	fldcw	-20(%ebp)
#	movl	%edx, -20(%ebp)
	fistl	-24(%ebp)
#	fldcw	-20(%ebp)
	fxch	%st(1)
	movl	-24(%ebp), %esi
#	fnstcw	-20(%ebp)
#	movl	-20(%ebp), %edx
#	movb	$12, -19(%ebp)
#	fldcw	-20(%ebp)
#	movl	%edx, -20(%ebp)
	fistl	-64(%ebp)
#	fldcw	-20(%ebp)
	fxch	%st(3)
#	fnstcw	-20(%ebp)
#	movl	-20(%ebp), %edx
#	movb	$12, -19(%ebp)
#	fldcw	-20(%ebp)
#	movl	%edx, -20(%ebp)
	fistl	-68(%ebp)
#	fldcw	-20(%ebp)
	fldl	-48(%ebp)
#	fnstcw	-20(%ebp)
#	movl	-20(%ebp), %edx
#	movb	$12, -19(%ebp)
#	fldcw	-20(%ebp)
#	movl	%edx, -20(%ebp)
	fistpl	-72(%ebp)
#	fldcw	-20(%ebp)
	fldl	-56(%ebp)
#	fnstcw	-20(%ebp)
#	movl	-20(%ebp), %edx
#	movb	$12, -19(%ebp)
#	fldcw	-20(%ebp)
#	movl	%edx, -20(%ebp)
	fistpl	-76(%ebp)
#	fldcw	-20(%ebp)
	fldl	-40(%ebp)
#	fnstcw	-20(%ebp)
#	movl	-20(%ebp), %edx
#	movb	$12, -19(%ebp)
#	fldcw	-20(%ebp)
#	movl	%edx, -20(%ebp)
	fistpl	-80(%ebp)
	fldcw	-20(%ebp)
	movl	28(%ebp), %edx
	fldl	(%edx)
	pushl	%eax
	fisubl	(%esp)
	popl	%eax
	fstps	-28(%ebp)
	flds	-28(%ebp)
	fxch	%st(3)
	pushl	%ebx
	fisubl	(%esp)
	popl	%ebx
	fstps	-84(%ebp)
	fxch	%st(1)
	pushl	%esi
	fisubl	(%esp)
	popl	%esi
	fstps	-88(%ebp)
	fxch	%st(2)
	fisubl	-64(%ebp)
	fstps	-28(%ebp)
	flds	-28(%ebp)
	fxch	%st(2)
	fisubl	-68(%ebp)
	fstps	-28(%ebp)
	flds	-28(%ebp)
	fldl	-48(%ebp)
	fisubl	-72(%ebp)
	fstps	-92(%ebp)
	fldl	-56(%ebp)
	fisubl	-76(%ebp)
	fstps	-96(%ebp)
	fldl	-40(%ebp)
	fisubl	-80(%ebp)
	fstps	-100(%ebp)
	movl	-132(%ebp), %edx
	leal	(%edx,%eax), %ecx
	movl	-136(%ebp), %eax
	leal	(%eax,%ecx), %edx
	movl	12(%ebp), %eax
	movl	(%eax), %eax
	leal	-4(,%eax,4), %eax
	fld1
	fsub	%st(2), %st
	sall	$2, %ecx
	fld	%st(5)
	fmuls	-4(%edi,%ecx)
	sall	$2, %edx
	fld	%st(5)
	fmuls	-4(%edi,%edx)
	faddp	%st, %st(1)
	fmulp	%st, %st(1)
	fadds	(%eax,%edi)
	fld	%st(5)
	fmuls	(%ecx,%edi)
	fld	%st(5)
	fmuls	(%edx,%edi)
	faddp	%st, %st(1)
	fmulp	%st, %st(3)
	faddp	%st, %st(2)
	fxch	%st(1)
	fstps	(%eax,%edi)
	movl	12(%ebp), %edx
	movl	(%edx), %eax
	incl	%eax
	movl	%eax, (%edx)
	movl	-132(%ebp), %edx
	leal	(%edx,%ebx), %ecx
	movl	-136(%ebp), %ebx
	leal	(%ebx,%ecx), %edx
	leal	-4(,%eax,4), %eax
	fld1
	fsubs	-84(%ebp)
	sall	$2, %ecx
	fld	%st(4)
	fmuls	-4(%edi,%ecx)
	sall	$2, %edx
	fld	%st(4)
	fmuls	-4(%edi,%edx)
	faddp	%st, %st(1)
	fmulp	%st, %st(1)
	fadds	(%eax,%edi)
	fld	%st(4)
	fmuls	(%ecx,%edi)
	fld	%st(4)
	fmuls	(%edx,%edi)
	faddp	%st, %st(1)
	fmuls	-84(%ebp)
	faddp	%st, %st(1)
	fstps	(%eax,%edi)
	movl	12(%ebp), %edx
	movl	(%edx), %eax
	incl	%eax
	movl	%eax, (%edx)
	movl	-132(%ebp), %ebx
	leal	(%ebx,%esi), %ecx
	movl	-136(%ebp), %esi
	leal	(%esi,%ecx), %edx
	leal	-4(,%eax,4), %eax
	fld1
	fsubs	-88(%ebp)
	sall	$2, %ecx
	fld	%st(4)
	fmuls	-4(%edi,%ecx)
	sall	$2, %edx
	fld	%st(4)
	fmuls	-4(%edi,%edx)
	faddp	%st, %st(1)
	fmulp	%st, %st(1)
	fadds	(%eax,%edi)
	fld	%st(4)
	fmuls	(%ecx,%edi)
	fld	%st(4)
	fmuls	(%edx,%edi)
	faddp	%st, %st(1)
	fmuls	-88(%ebp)
	faddp	%st, %st(1)
	fstps	(%eax,%edi)
	movl	12(%ebp), %edx
	movl	(%edx), %eax
	incl	%eax
	movl	%eax, (%edx)
	movl	-64(%ebp), %ecx
	addl	%ebx, %ecx
	leal	(%esi,%ecx), %edx
	leal	-4(,%eax,4), %eax
	fld1
	fsub	%st(2), %st
	sall	$2, %ecx
	fld	%st(4)
	fmuls	-4(%edi,%ecx)
	sall	$2, %edx
	fld	%st(4)
	fmuls	-4(%edi,%edx)
	faddp	%st, %st(1)
	fmulp	%st, %st(1)
	fadds	(%eax,%edi)
	fld	%st(4)
	fmuls	(%ecx,%edi)
	fld	%st(4)
	fmuls	(%edx,%edi)
	faddp	%st, %st(1)
	fmulp	%st, %st(3)
	faddp	%st, %st(2)
	fxch	%st(1)
	fstps	(%eax,%edi)
	movl	12(%ebp), %ebx
	movl	(%ebx), %eax
	incl	%eax
	movl	%eax, (%ebx)
	movl	-68(%ebp), %ecx
	addl	-132(%ebp), %ecx
	leal	(%esi,%ecx), %edx
	leal	-4(,%eax,4), %eax
	fld1
	fsub	%st(1), %st
	sall	$2, %ecx
	fld	%st(3)
	fmuls	-4(%edi,%ecx)
	sall	$2, %edx
	fld	%st(3)
	fmuls	-4(%edi,%edx)
	faddp	%st, %st(1)
	fmulp	%st, %st(1)
	fadds	(%eax,%edi)
	fld	%st(3)
	fmuls	(%ecx,%edi)
	fld	%st(3)
	fmuls	(%edx,%edi)
	faddp	%st, %st(1)
	fmulp	%st, %st(2)
	faddp	%st, %st(1)
	fstps	(%eax,%edi)
	movl	(%ebx), %eax
	incl	%eax
	movl	%eax, (%ebx)
	movl	-72(%ebp), %ecx
	addl	-132(%ebp), %ecx
	leal	(%esi,%ecx), %edx
	leal	-4(,%eax,4), %eax
	fld1
	fsubs	-92(%ebp)
	sall	$2, %ecx
	fld	%st(2)
	fmuls	-4(%edi,%ecx)
	sall	$2, %edx
	fld	%st(2)
	fmuls	-4(%edi,%edx)
	faddp	%st, %st(1)
	fmulp	%st, %st(1)
	fadds	(%eax,%edi)
	fld	%st(2)
	fmuls	(%ecx,%edi)
	fld	%st(2)
	fmuls	(%edx,%edi)
	faddp	%st, %st(1)
	fmuls	-92(%ebp)
	faddp	%st, %st(1)
	fstps	(%eax,%edi)
	movl	(%ebx), %eax
	incl	%eax
	movl	%eax, (%ebx)
	movl	-76(%ebp), %ecx
	addl	-132(%ebp), %ecx
	leal	(%esi,%ecx), %edx
	leal	-4(,%eax,4), %eax
	fld1
	fsubs	-96(%ebp)
	sall	$2, %ecx
	fld	%st(2)
	fmuls	-4(%edi,%ecx)
	sall	$2, %edx
	fld	%st(2)
	fmuls	-4(%edi,%edx)
	faddp	%st, %st(1)
	fmulp	%st, %st(1)
	fadds	(%eax,%edi)
	fld	%st(2)
	fmuls	(%ecx,%edi)
	fld	%st(2)
	fmuls	(%edx,%edi)
	faddp	%st, %st(1)
	fmuls	-96(%ebp)
	faddp	%st, %st(1)
	fstps	(%eax,%edi)
	movl	(%ebx), %eax
	incl	%eax
	movl	%eax, (%ebx)
	movl	-80(%ebp), %ecx
	addl	-132(%ebp), %ecx
	leal	(%esi,%ecx), %edx
	leal	-4(,%eax,4), %eax
	fld1
	fsubs	-100(%ebp)
	sall	$2, %ecx
	fld	%st(2)
	fmuls	-4(%edi,%ecx)
	sall	$2, %edx
	fld	%st(2)
	fmuls	-4(%edi,%edx)
	faddp	%st, %st(1)
	fmulp	%st, %st(1)
	fadds	(%eax,%edi)
	fld	%st(2)
	fmuls	(%ecx,%edi)
	fld	%st(2)
	fmuls	(%edx,%edi)
	faddp	%st, %st(1)
	fmuls	-100(%ebp)
	faddp	%st, %st(1)
	fstps	(%eax,%edi)
	incl	(%ebx)
	fldl	-40(%ebp)
	faddl	-112(%ebp)
	movl	28(%ebp), %esi
	fstpl	(%esi)
	decl	-104(%ebp)
	jns	.L7
	fstp	%st(0)
	fstp	%st(0)
.L5:
	movl	-60(%ebp), %ebx
	decl	%ebx
	js	.L10
	movl	16(%ebp), %eax
	movl	(%eax), %eax
	movl	%eax, -152(%ebp)
	movl	20(%ebp), %eax
	movl	(%eax), %eax
	movl	%eax, -156(%ebp)
	movl	40(%ebp), %edx
	flds	(%edx)
	movl	36(%ebp), %esi
	flds	(%esi)
	movl	32(%ebp), %eax
	flds	(%eax)
	movl	28(%ebp), %edx
	fldl	(%edx)
	.p2align 2
.L12:
	fnstcw	-20(%ebp)
	movl	-20(%ebp), %esi
	movb	$12, -19(%ebp)
	fldcw	-20(%ebp)
	movl	%esi, -20(%ebp)
	fistl	-24(%ebp)
	fldcw	-20(%ebp)
	movl	-24(%ebp), %eax
	fld	%st(0)
	pushl	%eax
	fisubl	(%esp)
	popl	%eax
	fstps	-28(%ebp)
	flds	-28(%ebp)
	movl	-152(%ebp), %edx
	leal	(%edx,%eax), %ecx
	movl	-156(%ebp), %esi
	leal	(%esi,%ecx), %edx
	movl	12(%ebp), %esi
	movl	(%esi), %eax
	leal	-4(,%eax,4), %eax
	fld1
	fsub	%st(1), %st
	sall	$2, %ecx
	fld	%st(5)
	fmuls	-4(%edi,%ecx)
	sall	$2, %edx
	fld	%st(5)
	fmuls	-4(%edi,%edx)
	faddp	%st, %st(1)
	fmulp	%st, %st(1)
	fadds	(%eax,%edi)
	fld	%st(5)
	fmuls	(%ecx,%edi)
	fld	%st(5)
	fmuls	(%edx,%edi)
	faddp	%st, %st(1)
	fmulp	%st, %st(2)
	faddp	%st, %st(1)
	fstps	(%eax,%edi)
	incl	(%esi)
	fadd	%st(1), %st
	decl	%ebx
	jns	.L12
	fstp	%st(1)
	fstp	%st(1)
	fstp	%st(1)
	movl	28(%ebp), %eax
	fstpl	(%eax)
.L10:
	addl	$164, %esp
	popl	%ebx
	popl	%esi
	popl	%edi
	popl	%ebp
	ret
.Lfe1:
	.size	 bpsumxtilt_,.Lfe1-bpsumxtilt_
	.ident	"GCC: (GNU) 2.96 20000731 (Red Hat Linux 7.0)"
