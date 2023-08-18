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

####################
# void bmpresize(unsigned char *imgptr, int h, int w, int k, unsigned char *outptr)
####################

	.globl bmpresize
bmpresize:

	# TODO
	
	# a0:	*imgptr
	# a1:	h
	# a2:	w
	# a3:	k
	# a4:	*outptr

	slli	t0, a2, 1
	add		t0, a2, t0				# t0: w * 3
	andi	t0, t0, 3
	addi	t0, t0, -4
	sub 	t0, x0, t0
	andi	t0, t0, 3				# t0: number of input padding p
	addi	sp, sp, -128
	sw 		t0, 0(sp)				# save t0 to stack: 0(sp) = input padding p
	sw		x0, 28(sp)				# save output remaining bits to sp+28
	srl		a1, a1, a3				# a1: h/2^k
	srl		a2, a2, a3				# a2: w/2^k
	slli	t0, a2, 1
	add		t0, a2, t0				# t0: w/2^k * 3
	addi	t0, t0, -4
	sub 	t0, x0, t0
	andi	t0, t0, 3 				# t0: number of output padding p
	sw		t0, 32(sp)				# save output padding p to sp+32
	sw		x0, 4(sp)				# save to stack: means check how many bytes read in previous load data
	sw		x0, 8(sp)				# save to stack: means i
	sw 		x0, 12(sp)				# save to stack: means j
	sw		x0, 16(sp)				# save to stack: means k
	sw		x0, 20(sp)				# save to stack: means l
	sw 		x0, 24(sp)				# save to stack: means m
	sw		x0, 36(sp)				# save to stack: output remaing bits
	sw		x0, 40(sp)				# save to stack: save output address
	jal 	x0, outeri  			# jump to outeri
	
outeri:
	lw		t1, 8(sp) 				# load t1(i) from stack (sp+8)
	sw		x0, 36(sp)				# set output remaining bits to 0
	bge 	t1, a1, end				# t1: i, if i >= h/2^k then move to end
	jal 	x0, outerj				# jump to outerj
	
outerilast:
	lw		t1, 8(sp)
	addi 	t1, t1, 1				# meaning i++
	sw 		t1, 8(sp)
	sw		x0, 12(sp)
	lw		t1, 40(sp)
	lw 		t2, 32(sp)
	add		t1, t2, t1
	sw 		t1, 40(sp)
	bge 	x0, x0, outeri			# jump to outeri

outerj:
	lw		t1, 12(sp)				# load t1(j) from stack (sp+12)
	bge		t1, a2, outerilast		# t1: j, if j >= w/2^k then move to outerilast
	jal		x0, color				# jump to innerk

outerjlast:
	lw		t1, 12(sp)
	addi 	t1, t1, 1				# meaning j++
	sw 		t1, 12(sp)
	add 	t1, x0, x0
	sw		t1, 24(sp)
	bge 	x0, x0, outerj			# jump to outerj

color:
	lw 		t1, 24(sp)				# load t1(m) from stack (sp+24)
	addi	t2, x0, 3
	bge 	t1, t2, outerjlast 		# if t1 >= t2 then move to outerjlast
	jal		x0, innerk

colorlast:
	lw		t0, 24(sp)				# load t0 m value
	slli	t0, t0, 2				# m' = 0, 4, 8
	addi	t0, t0, 44
	add		t0, sp, t0				
	lw 		t1, 0(t0)				# load color sum to t1
	sw		x0, 0(t0)
	srl		t1, t1, a3
	srl		t1, t1, a3				# sum >> 2*k

	# Now, we want to save this to outptr, so we have to calculate address using outp
	# words number = i*(3*a2+outp)+3j+m

	add		t2, a2, a2
	add		t2, t2, a2
	lw		t3, 32(sp)				
	add		t2, t2, t3				# t2 = 3*a2+outp
	lw		t3, 8(sp)
	add 	t4, x0, x0
	add		t0, x0, x0

calc:
	bge		t4, t3, colorkeep		# if t4 >= i then move to colorkeep, want to calculate multiply
	addi	t4, t4, 1
	add		t0, t0, t2
	jal		x0, calc

colorkeep:
	lw		t4, 24(sp)
	lw		t2, 12(sp)
	add		t3, t2, t2
	add		t2, t2, t3
	add		t4, t2, t4				# t0 = 3j + m
	add		t0, t4, t0
	andi	t2, t0, 3
	srli	t0, t0, 2
	slli	t0, t0, 2
	add		t0, t0, a4				# real output address
	slli	t2, t2, 3				# shift write value
	bne 	t2, x0, nonreset; 		# if t2 == 0 then reset output to 0
	sw 		x0, 0(t0)				# reset output to 0

nonreset:	
	sll		t1, t1, t2				
	lw		t4, 0(t0)				# load outptr data
	add		t4, t4, t1
	sw 		t4, 0(t0)				# save in outptr
	lw		t0, 24(sp)
	addi 	t0, t0, 1
	sw 		t0, 24(sp)				# m++
	sw		x0, 16(sp)
	jal 	x0, color				# jump to color
	
innerk:
	lw		t1, 16(sp)				# load t1(k) from stack (sp+16)
	addi	t2, x0, 1
	sll		t2, t2, a3
	bge		t1, t2, colorlast		# t1: k, if k >= 2^k (this k is input k) then move to colorlast
	jal		x0, innerl				# jump to innerl

innerklast:
	lw		t1, 16(sp)
	addi 	t1, t1, 1				# meaning k++
	sw 		t1, 16(sp)
	sw		x0, 20(sp)				# reset l to 0
	bge 	x0, x0, innerk			# jump to innerk

innerl:
	lw		t1, 20(sp)				# load t1(l) from stack (sp+16)
	addi	t2, x0, 1
	sll		t2, t2, a3
	bge		t1, t2, innerklast		# t1: l, if l >= 2^k then move to innerklast
	jal		x0, superinner			# jump to color

innerllast:
	lw		t1, 20(sp)
	addi 	t1, t1, 1				# meaning l++
	sw 		t1, 20(sp)
	jal		x0, innerl				# jump to innerl

superinner:
	lw		t0, 12(sp)				# want to make words number = 3*2^k*j+3l+m
	add		t1, t0, t0
	add		t0, t1, t0
	sll		t0, t0, a3
	lw		t1, 20(sp)
	add		t2, t1, t1
	add		t1, t2, t1
	add		t0, t0, t1
	lw		t1, 24(sp)
	add		t0, t0, t1				# t0: words number = 3*2^k*j+3l+m -> words number / 4 = addr, words number % 4 = remaining bit 	
	andi	t2, t0, 3				# t2: remaining bits after calculating
	sw		t2, 4(sp)
	srli	t0, t0, 2		
	slli	t0, t0, 2				# t0: addr of inptr, but we have update horizontal addr. so, have to add (2^k*i+k) * (3*w+inp)/4
	lw		t1, 8(sp)
	sll		t1, t1, a3
	lw 		t2, 16(sp)
	add		t1, t1, t2
	sll		t2, a2, a3
	add		t3, t2, t2
	add		t2, t2, t3
	lw		t3, 0(sp)
	add		t2, t2, t3
	# srli	t2, t2, 2
	addi	t3, x0, 1
	add		t4, x0, t1
	jal 	x0, calculate			# calulate multiply using for loop

calculate:
	bge 	t3, t2, superlast 		# if n >= (3*w+inp)/4 then move to superlast
	add		t1, t1, t4
	addi	t3, t3, 1
	jal		x0, calculate

superlast:
	add		t0, t0, t1				# Now, we have real input address
	add		t0, t0, a0
	lw 		t1, 0(t0) 				# load t1 value of color from a0: *imgptr + 4*(words number)
	lw		t2, 4(sp)				# load input remaining
	addi 	t4, x0, 255
	slli	t2, t2, 3
	sll		t4, t4, t2
	and		t1, t1, t4
	srl		t1, t1, t2				# select want color position and value and save into t1
	
	# m = 0: blue, save 44(sp)
	# m = 1: green, save 48(sp)
	# m = 2: red, save 52(sp)		

	lw		t0, 24(sp)				# load t0 m value
	slli	t0, t0, 2				# m' = 0, 4, 8
	addi	t0, t0, 44
	add		t0, sp, t0
	lw		t3, 0(t0)				# load t3 previous updeate value which is sum of color
	add 	t3, t3, t1				# sum of now value and previous value
	sw		t3, 0(t0)				# save to stack
	jal 	x0, innerllast			# move to innerl-last

end:
	addi 	sp, sp, 128
	ret
