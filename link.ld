/****************************************************************
 *
 *  4190.308 Computer Architecture (Fall 2022)
 *
 *  Project #3: Image Resizing in RISC-V Assembly
 *
 *  November 20, 2022
 *
 *  Seongyeop Jeong (seongyeop.jeong@snu.ac.kr)
 *  Jaehoon Shim (mattjs@snu.ac.kr)
 *  IlKueon Kang (kangilkueon@snu.ac.kr)
 *  Wookje Han (gksdnrwp@snu.ac.kr)
 *  Jinsol Park (jinsolpark@snu.ac.kr)
 *  Systems Software & Architecture Laboratory
 *  Dept. of Computer Science and Engineering
 *  Seoul National University
 *
 ****************************************************************/


OUTPUT_ARCH( "riscv" )
ENTRY(_start)

SECTIONS
{
  . = 0x80000000;
  .text.init : { *(.text.init) }
  . = ALIGN(0x1000);
  .tohost : { *(.tohost) }
  . = ALIGN(0x1000);
  .text : { *(.text) }
  . = 0x80010000;
  .data : { *(.data) }
  .bss : { *(.bss) }
  _end = .;
}
