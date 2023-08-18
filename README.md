# 4190.308 Computer Architecture (Fall 2022)
# Project #4: Extending a 5-stage Pipelined RISC-V Processor
### Due: 11:59PM, December 18 (Sunday)


## Introduction

The goal of this project is to understand how a pipelined processor works. In this project, you need to extend the existing 5-stage pipelined RISC-V simulator to support new instructions and a branch prediction scheme using the branch target buffer (BTB). 

## Background: A 5-stage pipelined RISC-V processor (`snurisc5`)

The target RISC-V processor `snurisc5` consists of five pipeline stages: IF, ID, EX, MM, and WB. The following briefly summarizes the tasks performed in each stage:

* IF: Fetches an instruction from imem (instruction memory)
* ID: Decodes the instruction, reads register files, and prepares immediate values
* EX: Performs arithmetic/logical computation and determines the branch outcome
* MM: Accesses dmem (data memory), if necessary
* WB: Writes back the result to the register file and updates `pc`

Please note that `snurisc5` is slightly different from the 5-stage pipeline described in the textbook. For more information, please refer to [this file](https://github.com/snu-csl/pyrisc/blob/master/pipe5/README.md).


## Problem specification

This project assignment consists of the following three parts.

## Part 1: Supporting `push` and `pop` instructions (40 points)

The RISC-V ISA is designed to support custom instructions. System architects can add any instruction for the application that they want to accelerate. This is a powerful feature as it allows for a new invention without breaking any software compatibility.

In this project, we want to add two new instructions called `push` and `pop`. Traditionally, these instructions are used to access data in the stack memory. The operations of the `push` and `pop` instructions are defined as follows (`sp` denotes the stack pointer register and `reg` can be any register):

```
push reg:  R[sp] <- R[sp] - 4
           M[R[sp]] <- R[reg]

pop  reg:  R[reg] <- M[R[sp]]
           R[sp] <- R[sp] + 4
```

Please note that executing the `pop sp` instruction leads to an undefined result and should be avoided, while `push sp` is a valid instruction.

Using the `push` and `pop` instructions, function prologue and epilogue needed to save and restore register values can be simplified as shown in the following example. 

```
func:  addi  sp, sp, -8           func:  push    ra
       sw    ra, 4(sp)                   push    s0
       sw    s0, 0(sp)
       
       ...                  =>           ...

       lw    s0, 0(sp)                   pop     s0
       lw    ra, 4(sp)                   pop     ra
       addi  sp, sp, 8                   ret
       ret
```

We encode `push` and `pop` instructions as R-type instructions, but with a new opcode `0b1101011`. Both instructions have only one register argument. For `push`, the register number is encoded in the `rs2` field as its value will be written into the memory just like in the `sw` instruction. For `pop`, the register number is encoded in the `rd` field. The below shows how `push x31` and `pop x30` instructions are encoded.

```
# R-type instruction format 
bits        31...25  24...20  19...15  14...12  11...7    6...0
             funct7      rs2      rs1   funct3      rd   opcode

push x31:   0000001    11111    00000      000   00000  1101011
pop  x30:   0000010    00000    00000      000   11110  1101011
```

Your task is to modify the existing 5-stage pipelined processor to support the `push` and `pop` instructions with fully implementing data forwarding wherever necessary. For `push` and `pop` instructions, use the ALU to compute the next `sp` value (`sp + 4` or `sp - 4`). 

There are several things you need to consider.

1. Both `push` and `pop` instructions may also have an implicit dependency on `sp` with the preceding instructions that modify `sp`. In these cases, you have to forward its value as well. 

* Example 1: Implicit dependency on `sp` 
  ```  
                         C0  C1  C2  C3  C4  C5  C6  C7  (cycle)
    lui   sp, 0x80020    IF  ID  EX  MM  WB
    push  t0                 IF  ID  EX  MM  WB          ; sp forwarded from lui
    pop   t1                     IF  ID  EX  MM  WB      ; sp forwarded from push
    ebreak                           IF  ID  EX  MM  WB
  ```
      
2. The `pop` instruction can cause another load-use hazard, where the pipeline should be stalled for one cycle. 

* Example 2: Load-use hazard 
  ``` 
                         C0  C1  C2  C3  C4  C5  C6  C7  C8  C9  (cycle)
    lui   sp, 0x80020    IF  ID  EX  MM  WB
    push  zero               IF  ID  EX  MM  WB
    pop   t0                     IF  ID  EX  MM  WB          
    push  t0                         IF  ID  ID  EX  MM  WB        ; 1-cycle stall       
    ebreak                               IF  IF  ID  EX  MM  WB
  ```

3. For `pop`, you need to write back *two* register values, namely `sp` and `rd`, in the WB stage. For this, we provide you with the new register file (`Pipe.cpu.rf`) that has two write ports: `rd` and `rd2` for two register numbers to be written, and `wbdata` and `wbdata2` for their data, respectively. If you specify any non-zero register numbers in both `rd` and `rd2`, they will be updated together at the end of the WB stage. If you don't have anything to write in `rd2`, set it to zero. Again, setting both `rd` and `rd2` to the same register number (except zero) results in an undefined behavior. You can assume that our test cases will not include such an instruction as `pop sp`.  


## Part 2: Branch prediction with Branch Target Buffer (40 points)

The current pipelined processor uses an __always-not-taken__ branch prediction scheme that naively fetches the next instructions when it encounters a branch instruction. In case the branch is determined to be taken at the EX stage, the wrong instructions in the IF and ID stages should be set to BUBBLEs, wasting two cycles. 

The role of the branch predictor is to increase the chances of fetching the right instruction after a branch or jump instruction. Based on the past history of whether the current branch instruction was previously taken or not, the branch predictor may be able to make a better decision. Your second task is to implement a branch predictor using the branch target buffer (BTB).

Basically, the BTB is a small table inside the processor that caches the recent information on __taken__ branches. Because the BTB predicts the next instruction address and will send it out _before_ decoding the instruction, we must know whether the fetched instruction is predicted as a taken branch. The hardware for BTB is very similar to the hardware for a cache. It has a fixed number of entries and each entry consists of valid bit (V), tag bits (T), and the target address (A) as shown below.

```
BTB structure:
     +---+---------+----------------------+ 
  0  | V | tag (T) |  target address (A)  |
     +---+---------+----------------------+ 
  1  |   |         |                      |
     +---+---------+----------------------+ 
     |   |   ...   |        ...           |
     +---+---------+----------------------+ 
 N-1 |   |         |                      |
     +---+---------+----------------------+ 
```

The valid bit indicates whether the corresponding entry has a valid information. Initially, all the entries in the BTB are set to be invalid. The tag bits represent the address of a branch instruction. And the target address has the address to jump when the branch is taken. 

In the IF stage, the instruction memory (imem) and the BTB are accessed simultaneously with the same `pc`. If the `pc` of the fetched instruction matches a tag in the BTB, then it indicates that the instruction is a branch and it was taken previously. In this case, the current branch instruction is also predicted as taken, and the corresponding target address is used as the next `pc`. If the matching entry is not found in the BTB, the current instruction is either (1) a branch instruction which were not taken previously, or (2) not a branch instruction at all. In both cases, the next instruction at `pc` + 4  will be fetched.

The branch outcome is known at the end of the EX stage. If the prediction using the BTB was right, we have nothing to do. When the prediction was wrong, we need to cancel two instructions in the IF and ID stages, and fetch the right instruction. Also, when the branch is predicted to be taken but it was not taken, we should make the corresponding entry in the BTB invalid. Likewise, when the branch is predicted to be not-taken but it was taken, we should add an entry for the current branch instruction to the BTB.

Consider the following things when you implement the BTB.

1. We assume that the BTB always has the power-of-2 (i.e. N = 2<sup>k</sup>, 0 <= k <= 8) entries.

2. The BTB has a direct-mapped organization, where `(pc >> 2) % N` is used as an index. We omit the last 2 bits in the `pc` because they will be always `0b00` (all the instruction addresses are aligned to the 4-byte boundary in `pyrisc`). The remaining bits of the `pc` (i.e. `pc >> (k + 2)`) are used as the tag bits. If the valid bit (`V`) is 1, it indicates a valid entry.

3. We have added a new class named `BTB` in the skeleton code. You need to implement the initialization code as well as `add()` and `remove()` functions. You can refer to the BTB object using the name `Pipe.cpu.btb`. Because of the direct-mapped organization, two or more branch instructions can be mapped to the same entry in the BTB. If the corresponding entry is already occupied by another branch instruction at the time you want to add a new entry, it is simply overwritten with the information of the current branch instruction.

4. You may need to add/remove an entry to/from the BTB in the EX stage when the prediction was wrong. Please make sure you update the BTB inside of the `update(self)` function of the EX stage so that it can be applied to the BTB at the end of the current cycle.

5. It is very difficult to predict the target address of the `jalr` instruction as it depends on a register value. To make the problem simpler, the `jalr` instruction is handled in the same way as in `snurisc5`; the instructions next to the `jalr` instruction are fetched until we have the target address in the EX stage and then the two instructions being executed in the IF and ID stages are converted into BUBBLEs while the target address is forwarded to the next `pc` value immediately. On the other hand, the `jal` instruction can be treated in the same way as the other branch instructions.

The following shows some example scenarios.

* Example 3: For the first `bne` instruction at line 7, the BTB entry is empty. Hence, it is predicted as not-taken and the following `ebreak` instruction is fetched. At `C4`, we know that it is mispredicted, so two instructions at line 11 and 13 are flushed and the `addi` instruction is fetched at `C5` (line 4). Next time we meet the `bne` instruction at line 8, the previous history is available in the BTB and it is predicted as taken, fetching the `addi` instruction immediately at the next cycle at `C7`. On the third execution of the `bne` instruction at `C8`, it will be still predicted as taken, but this is wrong. Again, two instructions at line 6 and 10 are flushed and the `ebreak` instruction is fetched at `C11`.   
  ```  
  1                        C0  C1  C2  C3  C4  C5  C6  C7  C8  C9 C10 C11 C12 C13 C14 C15 (cycle)
  2       li   t0, 3       IF  ID  EX  MM  WB
  3   L0: addi t0, t0, -1      IF  ID  EX  MM  WB          
  4                                            IF  ID  EX  MM  WB
  5                                                    IF  ID  EX  MM  WB
  6                                                            IF  ID  -   -   -
  7       bne  t0, x0, L0          IF  ID  EX  MM  WB
  8                                                IF  ID  EX  MM  WB   
  9                                                        IF  ID  EX  MM  WB
  10                                                               IF  -   -   -   - 
  11      ebreak                       IF  ID  -   -   -
  12                                                                   IF  ID  EX  MM  WB
  13      <illegal>                        IF  -   -   -   - 
   ```

* Example 4: In this example, the `beq` instruction will be mispredicted. But you can see that the instruction at the branch target is actually the same as the next instruction. In this case, you may feel tempted to keep them going instead of making them BUBBLEs. In order to do that, however, you would need another comparator at the EX stage to compare the branch target address with the address of the next instruction. To make the design simpler, we do not consider this optimization. So, whenever a branch is mispredicted, you must cancel two instructions being executed in the IF and ID stages.

  ```  
  1                        C0  C1  C2  C3  C4  C5  C6  C7  C8  (cycle)
  2       beq  x0, x0, L0  IF  ID  EX  MM  WB
  3   L0: li   t0, 1           IF  ID  -   -   -          
  4                                    IF  ID  EX  MM  WB        
  5       ebreak                   IF  -   -   -   -
  6                                        IF  ID  EX  MM  WB
  ```   


## Part 3: Design document (20 points)

You need to prepare and submit the design document (in PDF format) for the modified `snurisc5` processor. If you design the pipeline correctly with satisfying all the above requirements, you will get 20 points even if your implementation does not work. Your design document should answer the following questions.

1. About Part 1: When do the new data hazards occur due to the `push` and `pop` instructions and how do you deal with them? 

 * Show all the possible cases when data hazards can occur and your solutions to them
 * Explain the changes in the datapath 
 * Explain the changes in the control signals. You should present how each control signal is generated in detail similar to the descriptions in pp.319-320 in the textbook.

 2. About Part 2: How do you implement the branch prediction using the BTB?

 * Explain your implementation of the BTB
 * Explain how you find out that the current branch is mispredicted
 * Explain how you handle the mispredicted branch
 * Explain the changes in the datapath
 * Explain the changes in the control signals. Again, you should present how each control signal is generated in detail.


## Adding `push`/`pop` instructions to RISC-V toolchain

You need to rebuild the GNU RISC-V toolchain so that it understands the `push` and `pop` instructions. If you have deleted the source code of the GNU RISC-V toolchain, please download it again from the Github as follows.

```
$ git clone --recursive https://github.com/riscv/riscv-gnu-toolchain
```

There is a patch file named `pushpop.patch` in the `patch` directory of the skeleton code. Move the file into the `riscv-gnu-toolchain` directory and perform the following command. This will update `./binutils/include/opcode/riscv-opc.h` and `./binutils/opcodes/riscv-opc.c` files. (The following assumes that `ca-pa4` and `riscv-gnu-toolchain` directories exist in your home directory.)
```
$ cp ~/ca-pa4/patch/pushpop.patch ~/riscv-gnu-toolchain
$ cd ~/riscv-gnu-toolchain
$ patch -p1 < ./pushpop.patch
```

Now you can build the toolchain as before.

```
$ mkdir build
$ cd build
$ ../configure --prefix=/opt/riscv --with-arch=rv32i --disable-gdb
$ sudo make
```

## Skeleton code

We provide you with the skeleton code that can be downloaded from https://github.com/snu-csl/ca-pa4. To download the skeleton code, please take the following step:

```
$ git clone https://github.com/snu-csl/ca-pa4.git
```

It is basically the same as the 5-stage pipelined simulator (`snurisc5`) available in the [PyRISC project](https://github.com/snu-csl/pyrisc). Please refer to the [snurisc5.pdf](https://github.com/snu-csl/pyrisc/blob/master/pipe5/snurisc5.pdf) file for the current pipeline structure of the `snurisc5` simulator. We have slightly changed the simulator structure so that you only need to modify the `stages.py` file. Currently, the instruction encodings and masks for `push` and `pop` instructions are already added to the ISA table in `isa.py` as shown below. However, the datapath and the control logic cannot handle those instructions yet. 

You may find the [GUIDE.md](https://github.com/snu-csl/pyrisc/blob/master/pipe5/GUIDE.md) in the PyRISC project useful, which describes the overall architecture and implementation details of the `snurisc5` simulator.

```
# Instruction Encodings
PUSH        = WORD(0b00000010000000000000000001101011)  
POP         = WORD(0b00000100000000000000000001101011)  

# Instruction Masks
PUSH_MASK   = WORD(0b11111110000000000111000001111111)
POP_MASK    = WORD(0b11111110000000000111000001111111)

# ISA table
PUSH    : [ "push",     PUSH_MASK,  R_TYPE,   CL_MEM,   ]
POP     : [ "pop",      POP_MASK,   R_TYPE,   CL_MEM,   ]

```

Several RISC-V executable files, such as `fib`, `sum100 `, `forward`, `branch`, `loaduse`, `ex1`, `ex2`, `ex3`, and `ex4`, are available in the `./asm` directory of the skeleton code. In particular, `ex1`, `ex2`, `ex3`, and `ex4` programs are the ones explained in Example 1-4 of this document. You can test your simulator with these programs. Also, it is highly recommended to write your own test programs to see how your simulator works. Use the log level 4 as follows if you want to examine what's happening in the pipeline in each cycle. Also, you can change the size of the BTB using the `-b` option. For example, `-b 7` sets the size of the BTB to 2<sup>7</sup> = 128 entries (The default BTB size is set to `k` = 4, i.e. 16 entries).

```
$ ./snurisc5.py -l 4 -b 7 asm/ex4
```

## Restrictions

* You should not change any files other than `stages.py`.

* Your `stages.py` file should not contain any `print()` function even in comment lines. Please remove them before you submit your code to the server.

* You should not introduce unnecessary pipeline stalls.

* Your code should finish within a reasonable number of of cycles. If your simulator runs beyond the predefined threshold, you will get the `TIMEOUT` error.


## Hand in instructions

* Submit only the `stages.py` file to the submission server.

* Also, submit the design document (in PDF file only) to the submission server.

* The submitted code will NOT be graded instantly. Instead, it will be graded every four hours (12:00am, 4:00am, 8:00am, 12:00pm, 4:00pm, 8:00pm). You may submit multiple versions, but only the last version will be graded.

* The `sys` server will be closed at 11:59PM on December 22nd. This is the firm deadline.


## Logistics

* You will work on this project alone.

* Only the upload submitted before the deadline will receive the full credit. 25% of the credit will be deducted for every single day delay.

* You can use up to 4 slip days during this semester. If your submission is delayed by 1 day and if you decided to use 1 slip day, there will be no penalty. In this case, you should explicitly declare the number of slip days you want to use in the QnA board of the submission server after each submission.

* Any attempt to copy others' work will result in heavy penalty (for both the copier and the originator). Don't take a risk.

This is the final project. I hope it was fun!


[Jin-Soo Kim](mailto:jinsoo.kim_AT_snu.ac.kr)  
[Systems Software and Architecture Laboratory](http://csl.snu.ac.kr)  
[Dept. of Computer Science and Engineering](http://cse.snu.ac.kr)  
[Seoul National University](http://www.snu.ac.kr)
