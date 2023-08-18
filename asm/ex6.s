    .text
    .align  2
    .globl  _start
_start:
    lui     sp, 0x80020                  
    bne     x0, x0, _start
    add     x0, x0, x0
    add     x0, x0, x0
    push    t4
    pop     a0
    push    a0
    ebreak
