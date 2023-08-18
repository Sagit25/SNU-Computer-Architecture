    .text
    .align  2
    .globl  _start
_start: 
    lui     sp, 0x80020
    li      a1, 1
    li      a2, 2
    add     a3, a1, a2
    push    a1
    pop     a1
    push    a1
    pop     a1 
    push    a1
    pop     a1
    add     a2, a1, a1 
    push    sp
    add     x0, x0, x0
    pop     a1
    ebreak