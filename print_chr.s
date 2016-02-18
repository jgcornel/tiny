    .section .text
    .globl print_chr
    .type print_chr @function

print_chr:
    
    pushl %ebp
    movl %esp, %ebp
    pushl $0                    # -4(%ebp)  : returned value 
    pushl %eax
    pushl %ebx
    pushl %ecx
    pushl %edx
    pushl %edi
    pushl %esi

    movl $4, %eax
    movl $1, %ebx
    leal 8(%ebp), %ecx
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
