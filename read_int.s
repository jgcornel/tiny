    .section .bss
    .lcomm read_buf, 12
    .section .text
    .globl read_int
    .type read_int @function
read_int:
    
    pushl %ebp
    movl %esp, %ebp
    pushl $0                    # -4(%ebp)  : returned value 
    pushl $0                    # -8(%ebp)  : negative?
    pushl %eax
    pushl %ebx
    pushl %ecx
    pushl %edx
    pushl %edi
    pushl %esi

    movl $3, %eax
    movl $1, %ebx
    movl $read_buf, %ecx
    movl $12, %edx
    int $0x80                   # read!
                                # read count is in %eax
    movl $read_buf, %edi
    movl $0, %ebx
    movl $0, %ecx
    movl $0, %esi

    movb (%edi, %esi, 1), %cl   # first character '-' ?
    cmpl $45, %ecx
    jne ._ri_loop
    movl $1, -8(%ebp)
    jmp ._ri_next
._ri_loop:
    movb (%edi, %esi, 1), %cl
    cmpl $48, %ecx
    jl ._ri_next
    cmpl $57, %ecx
    jg ._ri_next
    imull $10, %ebx
    subl $48, %ecx
    addl %ecx, %ebx
._ri_next:
    incl %esi
    cmpl %eax, %esi             # read count is in %eax
    je ._ri_end_loop
    jmp ._ri_loop
._ri_end_loop:
    cmpl $0, -8(%ebp)
    je ._ri_end
    negl %ebx
._ri_end:
    movl %ebx, -4(%ebp)
    popl %esi
    popl %edi
    popl %edx
    popl %ecx
    popl %ebx
    popl %eax
    movl %ebp, %esp
    popl %ebp
    ret
