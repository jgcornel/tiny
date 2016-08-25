    .section .text
    .globl read_chr
    .type read_chr @function

read_chr:
    
    pushl %ebp
    movl %esp, %ebp
    pushl $0                    # -4(%ebp)  : returned value 
    pushl %eax
    pushl %ebx
    pushl %ecx
    pushl %edx
    pushl %edi
    pushl %esi

    movl $3, %eax
    movl $1, %ebx
    leal -4(%ebp), %ecx
    movl $1, %edx
    int $0x80

    popl %esi
    popl %edi
    popl %edx
    popl %ecx
    popl %ebx
    popl %eax
    movl %ebp, %esp
    popl %ebp
    ret
