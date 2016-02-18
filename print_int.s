	.section .bss
	.lcomm my_buf, 12
	.section .text
	.globl print_int
	.type print_int @function

print_int:
	pushl %ebp
    pushl %eax
    pushl %ebx
    pushl %ecx
    pushl %edx
    pushl %edi
    pushl %esi
	movl %esp, %ebp
    movl $my_buf, %edi
    movl $11, %esi
    movl $10, (%edi, %esi, 4)
    decl %esi
.PI1:
    cmpl $0, %esi
    jl .PI2
    movl $20, (%edi, %esi, 4)
    decl %esi
    jmp .PI1
.PI2:
    movl $10, %esi
    .equ value, 32
    .equ base, -4
    pushl $10
    .equ negative, -8
    pushl $0
    movl value(%ebp), %eax
    cmpl $0, %eax
    jge .PI3
    movl $1, negative(%ebp)
    negl %eax
.PI3:
    cdq
    divl base(%ebp)
    addl $48, %edx
    movl %edx, (%edi, %esi, 4)
    decl %esi
    cmpl $0, %eax
    jne .PI3
    cmpl $0, negative(%ebp)
    je .PI4
    movl $45, (%edi, %esi, 4)
    decl %esi
.PI4:
    incl %esi
	movl $4, %eax
	movl $1, %ebx
    leal (%edi, %esi, 4), %ecx
	#movl $my_buf, %ecx
	movl $48, %edx
	int $0x80

	# postlude
	movl %ebp, %esp
    popl %esi
    popl %edi
    popl %edx
    popl %ecx
    popl %ebx
    popl %eax
	popl %ebp
	ret
