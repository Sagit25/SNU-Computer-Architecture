#----------------------------------------------------------------
#
#  4190.308 Computer Architecture (Fall 2022)
#
#  Project #3: Image Resizing in RISC-V Assembly
#
#  November 20, 2022
# 
#  Seongyeop Jeong (seongyeop.jeong@snu.ac.kr)
#  Jaehoon Shim (mattjs@snu.ac.kr)
#  IlKueon Kang (kangilkueon@snu.ac.kr)
#  Wookje Han (gksdnrwp@snu.ac.kr)
#  Jinsol Park (jinsolpark@snu.ac.kr)
#  Systems Software & Architecture Laboratory
#  Dept. of Computer Science and Engineering
#  Seoul National University
#
#----------------------------------------------------------------

  .data
  .align  2

  .globl  test
test:
  .word test1
  .word test2
  .word test3
  .word test4
  .word test5
  .word 0

  .globl  ans
ans:
  .word ans1
  .word ans2
  .word ans3
  .word ans4
  .word ans5
  .word ans_END


test1:
  # k, width, height
  .word 1
  .word 4
  .word 4
  # bitmap
  .word 0x01020304
  .word 0x01020304
  .word 0x01020304
  
  .word 0x01020304
  .word 0x01020304
  .word 0x01020304
  
  .word 0x01020304
  .word 0x01020304
  .word 0x01020304
  
  .word 0x01020304
  .word 0x01020304
  .word 0x01020304
  
  .word 0x01020304
  .word 0x01020304
  .word 0x01020304
test2:
  # k, width, height
  .word 1
  .word 2
  .word 2
  # bitmap
  .word 0x04030201
  .word 0x00000605
  .word 0x04030201
  .word 0x00000605
test3:
  # k, width, height
  .word 1
  .word 4
  .word 4
  # bitmap
  .word 0x913e3d3c
  .word 0x33328e90
  .word 0x3e3d3c34
  .word 0x808e3d4d
  .word 0x9e8e3e92
  .word 0x001ebcaa
  .word 0x988234d3
  .word 0x11443122
  .word 0xeed32200
  .word 0x913e3d3c
  .word 0x33328e90
  .word 0x3e3d3c34

test4:
  # k, width, height
  .word 2
  .word 8
  .word 8
  # bitmap
  .word 0x913e3d3c
  .word 0x33328e90
  .word 0x3e3d3c34
  .word 0x808e3d4d
  .word 0x9e8e3e92
  .word 0x001ebcaa
  .word 0x988234d3
  .word 0x11443122
  .word 0xeed32200
  .word 0x913e3d3c
  .word 0x33328e90
  .word 0x3e3d3c34
  .word 0x913e3d3c
  .word 0x33328e90
  .word 0x3e3d3c34
  .word 0x913c3c3c
  .word 0x32329191
  .word 0x3c3c3c32
  .word 0x92241175
  .word 0x33125512
  .word 0x00000011
  .word 0x65646464
  .word 0x66666565
  .word 0x67676766
  .word 0x33422212
  .word 0xcaaca4d3
  .word 0x000000dd
  .word 0x65646464
  .word 0x66666565
  .word 0x67676766
  .word 0x41332212
  .word 0x15114212
  .word 0x00000099
  .word 0x913c3c3c
  .word 0x32329191
  .word 0x3c3c3c32
  .word 0x19934431
  .word 0x1900ac12
  .word 0x00000001
  .word 0x913c3c3c
  .word 0x32329191
  .word 0x3c3c3c32
  .word 0x15194412
  .word 0xfe191116
  .word 0x000000af
  .word 0x65646464
  .word 0x66666565
  .word 0x67676766
  
test5:
# k, width, height
  .word 1
  .word 6
  .word 4
# bitmap
  .word 0x35000000
  .word 0xf235fbf2
  .word 0x000000fb
  .word 0x00000000
  .word 0x00000000

  .word 0x35fbf235
  .word 0xf235fbf2
  .word 0xfbf235fb
  .word 0x00000000
  .word 0x00000000

  .word 0x00000000
  .word 0xf2350000
  .word 0xfa6a0afb
  .word 0x00fa6a0a
  .word 0x00000000

  .word 0x00000000
  .word 0xf2350000
  .word 0xfbf235fb
  .word 0x00000000
  .word 0x00000000


ans1:
  .word 0x02020302
  .word 0x00000201
  .word 0x02020302
  .word 0x00000201

ans2:
  .word 0x00040302

ans3:
  .word 0x6e666766
  .word 0x0000474b
  .word 0x355f488e
  .word 0x00005855

ans4: 
  .word 0x624a4056
  .word 0x00005859
  .word 0x5a4e3c1d
  .word 0x00005a5a

ans5:
  .word 0x27bcb527
  .word 0x0000bcb5
  .word 0x00000000
  .word 0x2a000000
  .word 0x1a02fad0
  .word 0x0000003e

ans_END:
  .word 0xdeadbeef
