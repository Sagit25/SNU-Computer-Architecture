# 4190.308 Computer Architecture (Fall 2022)

# Project #2: FP Addition

### Due: 11:59PM, October 16 (Sunday)

## Introduction

The goal of this project is to get familiar with the IEEE 754 floating-point standard by implementing the addition of two floating-point numbers using integer arithmetic. Specificially, this project focuses on a variation of the 16-bit floating-point format, called SnuFloat16 (or `SFP16` for short).

## SnuFloat16 (or `SFP16`) Representation

`SFP16` is a 16-bit floating-point representation that follows the IEEE 754 standard for floating-point arithmetic. The overall structure of the `SFP16` representation is shown below. The MSB (Most Significant Bit) is used as a sign bit (`S`). The next seven bits are used for the exponent (`E`) with a bias value of 63. The last eight bits are used for the fraction (`F`). The rules for normalized / denormalized values and the representation of zero, infinity, and NaN are all the same as in the IEEE standard. For rounding, we use the **round-to-even** mode.
```
bit 15 14 13 12 11 10 9  8  7  6  5  4  3  2  1  0
    S  E  E  E  E  E  E  E  F  F  F  F  F  F  F  F
    ^  +--------+--------+  +----------+---------+
    |           |                      |                       
Sign bit        |              Fraction (8 bits)
         Exponent (7 bits)                   
```

Note that the smallest positive number in the `SFP16` format is 0.00000001 x 2<sup>-62</sup> and the largest positive number is 1.11111111 x 2<sup>63</sup>.

In C, we use a 16-bit unsigned short integer to store the `SFP16` representation. Hence, the new type `SFP16` is defined as follows.

```
typedef unsigned short SFP16;
```

## FP Addition 

This section briefly overviews the required steps for adding two floating-point numbers. Let `x` and `y` be the two `SFP16`-type numbers to be added and `p` be the number of bits allocated for the fraction (i.e., `p` = 8 for the `SFP16` format). The notations E<sub>x</sub> and M<sub>x</sub> are used for the exponent and significand of `x` where M<sub>x</sub> has an explicit leading bit (0 or 1). Likewise, E<sub>y</sub> and M<sub>y</sub> are used for the exponent and significand of `y`. We also use the notation S<sub>x</sub> to represent the sign bit of `x` which is either 0 or 1. For example, when `x` has the bit pattern of `0b0100100111010010` (= 1.11010010 x 2<sup>10</sup>), M<sub>x</sub> = 1.11010010, E<sub>x</sub> = 10, and S<sub>x</sub> = 0. Therefore, the value of `x` can be represented as `x` = (-1)<sup>S<sub>x</sub></sup> * M<sub>x</sub> * 2<sup>E<sub>x</sub></sup>. 

1. If `|x|` < `|y|`, swap the operands. This ensures that the difference of the exponents satisfies `d` = E<sub>x</sub> - E<sub>y</sub> >= 0. In case `d` == 0 (i.e. E<sub>x</sub> == E<sub>y</sub>), this also ensures M<sub>x</sub> >= M<sub>y</sub>. 

2. Shift right M<sub>y</sub> by `d` bits and increment E<sub>y</sub> by `d` so that E<sub>y</sub> == E<sub>x</sub>.

3. If the signs of `x` and `y` are same, compute a preliminary significand `M` by adding M<sub>y</sub> to M<sub>x</sub> using the integer arithmetic. If the signs of `x` and `y` are different, subtract M<sub>y</sub> from M<sub>x</sub>. Also, set the exponent `E` and the sign bit `S` of the result such that `E` = E<sub>x</sub> and `S` = S<sub>x<sub>.

4. Normalize the preliminary significand `M` by shifting left or right until there is only one non-zero bit before the binary point. As you shift `M` to the right by one bit, you need to increment `E` by one. When you shift left `M` to the left by one bit, `E` should be decremented by one. Note that sometimes `M` should be represented as a denormalized form in which case there will be 0 before the binary point.

5. After the normalization, if `M` has any non-zero bits beyond `p` bits after the binary point, it should be rounded according to the __rounding-to-even__ mode.

6. As a result of the rounding, it is possible that `M` is no longer in a normalized form, in which case you need to perform the normalization and rounding again.

7. Now the result value will be (-1)<sup>S</sup> * M * 2<sup>E</sup> and encode this value into binary numbers according to the `SFP16` format.

### Example 1

Let `x` = 0x17f2 and `y` = 0x154f be the two floating-point numbers in the `SFP16` representation.
```
         S EEE EEEE FFFF FFFF                          
      x: 0 001 0111 1111 0010   Sx = 0, Mx = 1.11110010, Ex = 23 - bias = -40
      y: 0 001 0101 0100 1111   Sy = 0, My = 1.01001111, Ey = 21 - bias = -42
```

Since E<sub>y</sub> is smaller than E<sub>x</sub>, M<sub>y</sub> is shifted to the right by E<sub>x</sub> - E<sub>y</sub> = 2 bits. And then, M<sub>x</sub> and M<sub>y</sub> are added together. 

```                               
     Mx:  1.1111001000      ; Ex = -40
   + My:  0.0101001111      ; Ey = -40
      M: 10.0100010111      ; E  = -40,  S = 0
```

The result `M` is not in a normalized form. So, `M` is shifted right and the exponent is incremented by 1, such that `M` = 1.00100010111 and `E` = -39.

As `M` has 11 bits after the binary point, we need to truncate the trailing 3 bits (`111`) beyond the `p` (= 8) bits. In this case, we need to round up by adding 1 into the `p`-th bit. 

```
      M: 10.0100010111      ; E  = -40 
      M:  1.00100010111     ; E  = -39  (after normalization)
      M:  1.00100011        ; E  = -39  (after rounding)
```

Now we encode the result `M` and `E` according to the `SFP16` representation. (`f` indicates the fraction bits and `e` is the exponent value after adding the bias.)

```
         S EEE EEEE FFFF FFFF                          
    x+y: 0 001 1000 0010 0011    ; S = 0, f = 00100011, e = -39 + bias = 24
         = 0x1823
````
    
### Example 2 

Let's look at another example where `x` = 0x8b00 and `y` = 0x0100. 

```
         S EEE EEEE FFFF FFFF                          
      x: 1 000 1011 0000 0000   Sx = 1, Mx = 1.00000000, Ex = 11 - bias = -52
      y: 0 000 0001 0000 0000   Sy = 0, My = 1.00000000, Ey = 1 - bias = -62
```

Similar to the previous example, M<sub>y</sub> is shifted to the right by E<sub>x</sub> - E<sub>y</sub> = 10 bits. Since the signs of two input values differ, we need to subtract M<sub>y</sub> from M<sub>x</sub>. The sign of the result `S` will follow that of `x`, i.e., `S` = S<sub>x</sub> = 1. 

```                              
     Mx:  1.0000000000        ; Ex = -52
   - My:  0.0000000001        ; Ey = -52
      M:  0.1111111111        ; E  = -52, S = 1
```

Since the result `M` is not in a normalized form, `M` is shifted left by one bit and `E` is decremented by one, such that `M` = 1.111111111 and `E` = -53.

Now we perform the rounding. In this case, 1.111111111 is located in the middle of 1.11111111 and (1.11111111 + 0.00000001 or 10.00000000) and we need to round up to the even number 10.00000000 according to the round-to-even mode.

As a result of this rounding, `M` is no longer in a normalized form and we have to normalize `M` again as follows.

```
      M:  0.1111111111        ; E  = -52
      M:  1.111111111         ; E  = -53  (after normalization)
      M: 10.00000000          ; E  = -53  (after rounding)
      M:  1.00000000          ; E  = -52  (after re-normalization)
```

You can see that the final result of the addition is actually same as `x`.

```
         S EEE EEEE FFFF FFFF                          
    x+y: 1 000 1011 0000 0000    ; S = 1, f = 11011110, e = -52 + bias = 11
         = 0x8b00
````

## FP Addition using Guard, Round, and Sticky Bits

Typically, a floating-point operation takes two inputs `x` and `y` with `p` bits of fraction and returns a `p`-bit result. The ideal algorithm would compute this by first performing the operation exactly, and then rounding the result to `p` bits. The floating-point addition algorithm presented in the previous section follows this strategy. In this case, however, we need a very wide adder and also very long registers to compute and keep the intermediate result. This is true especially when the difference (`d`) in the exponents of the two inputs is very large because the smaller value should be shifted to the right by `d` bits.

Fortunately, we can produce the same result by maintaining only three extra bits, namely guard (G), round (R), and sticky (S) bits. These bits are added (or subtracted) together along with the other `p` bits, and they are also used for normalization and rounding. The floating-point addition with the guard, round, and sticky bits is proceeded as follows.

1. If `|x|` < `|y|`, swap the operands. 

2. Now we extend M<sub>x</sub> and M<sub>y</sub> to include the extra three bits (G, R, and S bits) after the `p`-bit fraction. Those bits are initialized to zeroes. And then we shift right M<sub>y</sub> by `d` = E<sub>x</sub> - E<sub>y</sub> bits. From the bits shifted out beyond the `p` bits, we set the guard bit to the most significant bit (i.e., (`p`+1)-th bit), the round bit to the next most significant bit (i.e., (`p`+2)-th bit), and sticky bit to the OR of the rest.

3. If the signs of `x` and `y` are same, compute a preliminary significand `M` by adding M<sub>y</sub> to M<sub>x</sub> using the integer arithmetic. If the signs of `x` and `y` are different, subtract M<sub>y</sub> from M<sub>x</sub>. Also, set the exponent `E` and the sign bit `S` of the result such that `E` = E<sub>x</sub> and `S` = S<sub>x<sub>.

4. Normalize the preliminary significand `M`. Whenever `M` is shifted left, those G, R, and S bits are shifted together as if they were part of the normal fraction bits, filling the sticky bit with zero. Likewise, whenever `M` is shifted right during the normalization step, G, R, and S bits are updated as follows (assuming `b` is the new bit shifted out beyond the `p` bits):
   ```
      S_bit <- S_bit | R_bit
      R_bit <- G_bit
      G_bit <- b
   ```
   In any case, the exponent of the result, `E`, should be adjusted accordingly.

5. The next step is to perform rounding. The main role of the guard bit is to fill in the least-significant bit (`p`-th bit) of the result when necessary. Depending on the situation, the original guard bit in the step 3 may have moved to the left or right direction during normalization. But in any case, the role of the guard bit is over after the normalization step. Hence, we adjust the round bit and the sticky bit as follows:
   ```
      S_bit <- S_bit | R_bit
      R_bit <- G_bit
   ```

   This is because, when it comes to the rounding, what really matters is the discarded bits. We set the round bit to the previous guard bit because it is the first bit among the discarded bits. We also update the sticky bit to include the status of the previous round bit. 

   Using the updated round bit and the sticky bit, we perform rounding according to the round-to-even mode: if (R_bit &and; S_bit) &or; (L_bit &and; R_bit), round up, otherwise round down. (Here, L_bit means the last bit of the fraction, i.e., `p`-th bit.)

6. Perform the normalization and rounding again if necessary.

7. Encode the result into binary numbers according to the `SFP16` format.


### Example 1 (revisited)

Let `x` = 0x17f2 and `y` = 0x154f are the two floating-point numbers in the `SFP16` representation.
```
         S EEE EEEE FFFF FFFF                          
      x: 0 001 0111 1111 0010   Sx = 0, Mx = 1.11110010, Ex = 23 - bias = -40
      y: 0 001 0101 0100 1111   Sy = 0, My = 1.01001111, Ey = 21 - bias = -42
```

M<sub>y</sub> is shifted to the right by E<sub>x</sub> - E<sub>y</sub> = 2 bits. And then, M<sub>x</sub> and M<sub>y</sub> are added together. We set the G, R, and S bits as follows:

```                         
                     GRS      
     Mx:  1.11110010 000    ; Ex = -40
   + My:  0.01010011 110    ; Ey = -40
      M: 10.01000101 110    ; E  = -40,  S = 0
```

The result `M` is not in a normalized form. So, `M` is shifted right and the exponent is incremented by 1, such that `M` = 1.00100010 (with GRS=111) and `E` = -39.

Now, we perform the rounding by setting the R_bit to 1 (the previous G bit), and the S_bit to 1 (previous R_bit | S_bit). This is a round-up condition, so we add 1 into the `p`-th bit.  

```
                     GRS
      M: 10.01000101 110    ; E  = -40 
      M:  1.00100010 111    ; E  = -39  (after normalization)
                     RS
      M:  1.00100010 11     ; E  = -39  (update RS for rounding)
      M:  1.00100011        ; E  = -39  (after rounding)
```

Now we encode the result `M` and `E` according to the `SFP16` representation. (`f` indicates the fraction bits and `e` is the exponent value after adding the bias.)

```
         S EEE EEEE FFFF FFFF                          
    x+y: 0 001 1000 0010 0011    ; S = 0, f = 00100011, e = -39 + bias = 24
         = 0x1823
````

   
### Example 2 (revisited)

Let's look at the previous example again where `x` = 0x8b00 and `y` = 0x0100. 

```
         S EEE EEEE FFFF FFFF                          
      x: 1 000 1011 0000 0000   Sx = 1, Mx = 1.00000000, Ex = 11 - bias = -52
      y: 0 000 0001 0000 0000   Sy = 0, My = 1.00000000, Ey = 1 - bias = -62
```

M<sub>y</sub> is shifted to the right by E<sub>x</sub> - E<sub>y</sub> = 10 bits, setting the G, R, and S bits accordingly.  Since the signs of two input values differ, we need to subtract M<sub>y</sub> from M<sub>x</sub> in this case. The sign of the result `S` will follow that of `x`, i.e., `S` = S<sub>x</sub> = 1. 

```                   
                     GRS           
     Mx:  1.00000000 000      ; Ex = -52
   - My:  0.00000000 010      ; Ey = -52
      M:  0.11111111 110      ; E  = -52, S = 1
```

Since the result `M` is not in a normalized form, the guard bit is moved into the fraction, decrementing `E` by one, such that `M` = 1.11111111 (with GRS=100) and `E` = -53.

For rounding, we update the R_bit to 1 and the S_bit to 0. Because (L_bit &and; R_bit) is true, we round up making `M` = 10.00000000 with `E` = -53. After renormalization, we get `M` = 1.00000000 and `E` = -52. 

```
                     GRS
      M:  0.11111111 110      ; E  = -52
      M:  1.11111111 100      ; E  = -53  (after normalization)
                     RS
      M:  1.11111111 10       ; E  = -53  (update RS for rounding)
      M: 10.00000000          ; E  = -53  (after rounding)
      M:  1.00000000          ; E  = -52  (after re-normalization)
```

The final result is encoded as follows:

```
         S EEE EEEE FFFF FFFF                          
    x+y: 1 000 1011 0000 0000    ; S = 1, f = 11011110, e = -52 + bias = 11
         = 0x8b00
````

## Problem Specification

Your task is to implement the following C function `fpadd()` that returns the sum (`x` + `y`) of the two floating-point numbers, `x` and `y`, represented in the `SFP16` format. The return value should be also represented in the `SFP16` format. Any of the input can be a negative value.

```
typedef unsigned short SFP16;
SFP16 fpadd(SPF16 x, SPF16 y);
```

* Your implementation should make use of the three extra bits (guard, round, and sticky bits) without retaining the unnecessary fraction bits.

* We do not distinguish between +0 and -0. When the result is zero, you can return any bit pattern corresponding to +0 (0x0000) or -0 (0x8000).

* For NaN (Not-a-Number), we only allow the bit pattern where the fractional part is 0b00000001. Hence, +NaN and -NAN are encoded as 0x7f01 and 0xff01, respectively. When one of the inputs is NaN, you can assume that it has the bit pattern of 0x7f01 or 0xff01, and nothing else. 

* We do not distinguish between +NaN and -NaN. When the result is NaN, you can return any of them. 

* We DO however distinguish between +inf (0x7f00) and -inf (0xff00). When the result becomes too large to be represented, it should be converted to either +inf or -inf depending on its sign. 

* For special cases where +inf/-inf, +0/-0, and +NaN/-NAN are involved, the return value should be the same as the one shown in the following table.

   ```
        y:    +inf   -inf    nan    zero   other
    x:     
     +inf     +inf    nan    nan    +inf   +inf
     -inf      nan   -inf    nan    -inf   -inf
      nan      nan    nan    nan     nan    nan
      zero    +inf   -inf    nan     zero
      other   +inf   -inf    nan
   ```

## Skeleton code

We provide you with the skeleton code for this project. It can be download from Github at https://github.com/snu-csl/ca-pa2/. To download and build the skeleton code, please follow these steps:

```
$ git clone https://github.com/snu-csl/ca-pa2.git
$ cd ca-pa2
$ make
gcc -g -O2 -Wall   -c -o pa2.o pa2.c
gcc -g -O2 -Wall   -c -o pa2-test.o pa2-test.c
gcc -o pa2 pa2.o pa2-test.o
```

The result of a sample run looks like this:

```
$ ./pa2
-------- NORM + NORM --------
test 0: Wrong
test 1: Wrong
test 2: Wrong
test 3: Wrong
-------- NORM + DENORM ------
test 0: Wrong
test 1: Wrong
test 2: Wrong
test 3: Wrong
------ DENORM + DENORM ------
test 0: Wrong
test 1: Wrong
test 2: Wrong
test 3: Wrong
--------- INF / NAN ---------
test 0: Wrong
test 1: Wrong
test 2: Wrong
test 3: Wrong
```

You are required to complete the `fpadd()` function in the `pa2.c` file.

## Grading Guideline

The following shows the types of test cases and their relative points during grading. 

* Normal + Normal values
  * Addition (15 points)
  * Subtraction (15 points)
* Denormal + Denormal values
  * Addition (15 points)
  * Subtraction (15 points) 
* Normal + Denormal values
  * Addition (15 points)
  * Subtraction (15 points)
* Handling of special values (10 points)


## Restrictions

- You are not allowed to use any array.
- You are not allowed to use floating-point data types such as `float` and `double`.
- You are not allowed to use any integer data type whose bit width is greater than 16 bits (e.g., `int`, `long`, `long long` etc.).
- If your implementation violates the intention of this project assignment, you will get penalty (e.g., simulating an array using a bunch of local or global variables, etc.).
- The following is the list of symbols and keywords that are not allowed. Any source file that contains one or more of them (even in the comment lines) will be rejected by the server.
   ```
   [, ], int, long, float, double, struct, union, static
   ``` 
- Do not include any header file in the `pa2.c` file.
- Your `pa2.c` file should not contain any external library functions including `printf()`. Please remove them before you submit your code to the server.
- Your code should finish within a reasonable time. We will measure the time to perform a certain number of operations. If your code does not finish within a predefined threshold (e.g., 5 sec), it will be killed.


## Hand in instructions

- Submit only the `pa2.c` file to the submission server.

- The submitted code will NOT be graded instantly. Instead, it will be graded twice a day at noon and midnight. You may submit multiple versions, but only the last version submitted before 12:00pm or 12:00am will be graded.

## Logistics

- You will work on this project alone.
- Only the upload submitted before the deadline will receive the full credit. 25% of the credit will be deducted for every single day delay.
- **You can use up to 4 *slip days* during this semester**. If your submission is delayed by 1 day and if you decided to use 1 slip day, there will be no penalty. In this case, you should explicitly declare the number of slip days you want to use in the QnA board of the submission server after each submission. Saving the slip days for later projects is highly recommended!
- Any attempt to copy others’ work will result in heavy penalty (for both the copier and the originator). Don’t take a risk.

## Reference

[1] David Goldberg, "Appendix J: Computer Arithmetic," _Computer Architecture: A Quantitative Approach_, 6th Edition, Morgan Kaufmann, 2019.

Have fun!

[Jin-Soo Kim](mailto:jinsoo.kim_AT_snu.ac.kr)  
[Systems Software and Architecture Laboratory](http://csl.snu.ac.kr)  
[Dept. of Computer Science and Engineering](http://cse.snu.ac.kr)  
[Seoul National University](http://www.snu.ac.kr)
