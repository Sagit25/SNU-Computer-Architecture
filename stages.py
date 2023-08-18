#==========================================================================
#
#   Project #4: Extending a 5-stage Pipelined RISC-V Processor
#
#   November 28, 2022
# 
#   Seongyeop Jeong (seongyeop.jeong@snu.ac.kr)
#   Jaehoon Shim (mattjs@snu.ac.kr)
#   IlKueon Kang (kangilkueon@snu.ac.kr)
#   Wookje Han (gksdnrwp@snu.ac.kr)
#   Jinsol Park (jinsolpark@snu.ac.kr)
#
#   Systems Software & Architecture Laboratory
#   Dept. of Computer Science and Engineering
#   Seoul National University
#
#==========================================================================

import sys

from consts import *
from isa import *
from program import *
from pipe import *


#--------------------------------------------------------------------------
#   BTB: For Project #4
#--------------------------------------------------------------------------

class BTB(object):

    def __init__(self, k):
        self.k = k
        # initialize your BTB here
        self.N = 1 << self.k
        self.table = [[0 for col in range(3)] for row in range(self.N)]

    # Lookup the entry corresponding to the pc
    # It will return the target address if there is a matching entry
    def lookup(self, pc):
        index = (pc >> 2) % self.N
        tag = pc >> (self.k + 2)
        if (self.table[index][0] == 1) and (self.table[index][1] == tag):
            return self.table[index][2] 
        else:
            return 0

    # Add an entry 
    def add(self, pc, target):
        index = (pc >> 2) % self.N
        tag = pc >> (self.k + 2)
        self.table[index][0] = 1
        self.table[index][1] = tag
        self.table[index][2] = target
        return

    # Make the corresponding entry invalid
    def remove(self, pc):
        index = (pc >> 2) % self.N
        self.table[index][0] = 0
        return



#--------------------------------------------------------------------------
#   Control signal table
#--------------------------------------------------------------------------

csignals = {
    LW     : [ Y, BR_N  , OP1_RS1, OP2_IMI, OEN_1, OEN_0, ALU_ADD  , WB_MEM, REN_1, MEN_1, M_XRD, MT_W, ],
    SW     : [ Y, BR_N  , OP1_RS1, OP2_IMS, OEN_1, OEN_1, ALU_ADD  , WB_X  , REN_0, MEN_1, M_XWR, MT_W, ],
    AUIPC  : [ Y, BR_N  , OP1_PC,  OP2_IMU, OEN_0, OEN_0, ALU_ADD  , WB_ALU, REN_1, MEN_0, M_X  , MT_X, ],
    LUI    : [ Y, BR_N  , OP1_X,   OP2_IMU, OEN_0, OEN_0, ALU_COPY2, WB_ALU, REN_1, MEN_0, M_X  , MT_X, ],
    ADDI   : [ Y, BR_N  , OP1_RS1, OP2_IMI, OEN_1, OEN_0, ALU_ADD  , WB_ALU, REN_1, MEN_0, M_X  , MT_X, ],

    SLLI   : [ Y, BR_N  , OP1_RS1, OP2_IMI, OEN_1, OEN_0, ALU_SLL  , WB_ALU, REN_1, MEN_0, M_X  , MT_X, ],
    SLTI   : [ Y, BR_N  , OP1_RS1, OP2_IMI, OEN_1, OEN_0, ALU_SLT  , WB_ALU, REN_1, MEN_0, M_X  , MT_X, ],
    SLTIU  : [ Y, BR_N  , OP1_RS1, OP2_IMI, OEN_1, OEN_0, ALU_SLTU , WB_ALU, REN_1, MEN_0, M_X  , MT_X, ],
    XORI   : [ Y, BR_N  , OP1_RS1, OP2_IMI, OEN_1, OEN_0, ALU_XOR  , WB_ALU, REN_1, MEN_0, M_X  , MT_X, ],
    SRLI   : [ Y, BR_N  , OP1_RS1, OP2_IMI, OEN_1, OEN_0, ALU_SRL  , WB_ALU, REN_1, MEN_0, M_X  , MT_X, ],

    SRAI   : [ Y, BR_N  , OP1_RS1, OP2_IMI, OEN_1, OEN_0, ALU_SRA  , WB_ALU, REN_1, MEN_0, M_X  , MT_X, ],
    ORI    : [ Y, BR_N  , OP1_RS1, OP2_IMI, OEN_1, OEN_0, ALU_OR   , WB_ALU, REN_1, MEN_0, M_X  , MT_X, ],
    ANDI   : [ Y, BR_N  , OP1_RS1, OP2_IMI, OEN_1, OEN_0, ALU_AND  , WB_ALU, REN_1, MEN_0, M_X  , MT_X, ],
    ADD    : [ Y, BR_N  , OP1_RS1, OP2_RS2, OEN_1, OEN_1, ALU_ADD  , WB_ALU, REN_1, MEN_0, M_X  , MT_X, ],
    SUB    : [ Y, BR_N  , OP1_RS1, OP2_RS2, OEN_1, OEN_1, ALU_SUB  , WB_ALU, REN_1, MEN_0, M_X  , MT_X, ],

    SLL    : [ Y, BR_N  , OP1_RS1, OP2_RS2, OEN_1, OEN_1, ALU_SLL  , WB_ALU, REN_1, MEN_0, M_X  , MT_X, ],
    SLT    : [ Y, BR_N  , OP1_RS1, OP2_RS2, OEN_1, OEN_1, ALU_SLT  , WB_ALU, REN_1, MEN_0, M_X  , MT_X, ],
    SLTU   : [ Y, BR_N  , OP1_RS1, OP2_RS2, OEN_1, OEN_1, ALU_SLTU , WB_ALU, REN_1, MEN_0, M_X  , MT_X, ],
    XOR    : [ Y, BR_N  , OP1_RS1, OP2_RS2, OEN_1, OEN_1, ALU_XOR  , WB_ALU, REN_1, MEN_0, M_X  , MT_X, ],
    SRL    : [ Y, BR_N  , OP1_RS1, OP2_RS2, OEN_1, OEN_1, ALU_SRL  , WB_ALU, REN_1, MEN_0, M_X  , MT_X, ],

    SRA    : [ Y, BR_N  , OP1_RS1, OP2_RS2, OEN_1, OEN_1, ALU_SRA  , WB_ALU, REN_1, MEN_0, M_X  , MT_X, ],
    OR     : [ Y, BR_N  , OP1_RS1, OP2_RS2, OEN_1, OEN_1, ALU_OR   , WB_ALU, REN_1, MEN_0, M_X  , MT_X, ],
    AND    : [ Y, BR_N  , OP1_RS1, OP2_RS2, OEN_1, OEN_1, ALU_AND  , WB_ALU, REN_1, MEN_0, M_X  , MT_X, ],
    JALR   : [ Y, BR_JR , OP1_RS1, OP2_IMI, OEN_1, OEN_0, ALU_ADD  , WB_PC4, REN_1, MEN_0, M_X  , MT_X, ],   
    JAL    : [ Y, BR_J  , OP1_X  , OP2_IMJ, OEN_0, OEN_0, ALU_X    , WB_PC4, REN_1, MEN_0, M_X  , MT_X, ],

    BEQ    : [ Y, BR_EQ , OP1_RS1, OP2_IMB, OEN_1, OEN_1, ALU_SEQ  , WB_X  , REN_0, MEN_0, M_X  , MT_X, ],
    BNE    : [ Y, BR_NE , OP1_RS1, OP2_IMB, OEN_1, OEN_1, ALU_SEQ  , WB_X  , REN_0, MEN_0, M_X  , MT_X, ],
    BLT    : [ Y, BR_LT , OP1_RS1, OP2_IMB, OEN_1, OEN_1, ALU_SLT  , WB_X  , REN_0, MEN_0, M_X  , MT_X, ],
    BGE    : [ Y, BR_GE , OP1_RS1, OP2_IMB, OEN_1, OEN_1, ALU_SLT  , WB_X  , REN_0, MEN_0, M_X  , MT_X, ],
    BLTU   : [ Y, BR_LTU, OP1_RS1, OP2_IMB, OEN_1, OEN_1, ALU_SLTU , WB_X  , REN_0, MEN_0, M_X  , MT_X, ],

    BGEU   : [ Y, BR_GEU, OP1_RS1, OP2_IMB, OEN_1, OEN_1, ALU_SLTU , WB_X  , REN_0, MEN_0, M_X  , MT_X, ],
    ECALL  : [ Y, BR_N  , OP1_X  , OP2_X  , OEN_0, OEN_0, ALU_X    , WB_X  , REN_0, MEN_0, M_X  , MT_X, ],
    EBREAK : [ Y, BR_N  , OP1_X  , OP2_X  , OEN_0, OEN_0, ALU_X    , WB_X  , REN_0, MEN_0, M_X  , MT_X, ],

    # Add entries for PUSH and POP instructions

    PUSH   : [ Y, BR_N  , OP1_RS1, OP2_IMI, OEN_1, OEN_1, ALU_SUB  , WB_MEM, REN_1, MEN_1, M_XWR, MT_W, ],
    POP    : [ Y, BR_N  , OP1_RS1, OP2_IMI, OEN_1, OEN_0, ALU_ADD  , WB_MEM, REN_1, MEN_1, M_XRD, MT_W, ],

}


#--------------------------------------------------------------------------
#   IF: Instruction fetch stage
#--------------------------------------------------------------------------

class IF(Pipe):

    # Pipeline registers ------------------------------

    reg_pc          = WORD(0)       # IF.reg_pc

    #--------------------------------------------------


    def __init__(self):
        super().__init__()

        # Internal signals:----------------------------
        #
        #   self.pc                 # Pipe.IF.pc
        #   self.inst               # Pipe.IF.inst
        #   self.exception          # Pipe.IF.exception
        #   self.pc_next            # Pipe.IF.pc_next
        #   self.pcplus4            # Pipe.IF.pcplus4
        #
        #----------------------------------------------

        self.taken = 0

    def compute(self):

        # Readout pipeline register values 
        self.pc     = IF.reg_pc

        # Fetch an instruction from instruction memory (imem)
        self.inst, status = Pipe.cpu.imem.access(Pipe.CTL.imem_en, self.pc, 0, Pipe.CTL.imem_rw)

        # Handle exception during imem access
        if not status:
            self.exception = EXC_IMEM_ERROR
            self.inst = BUBBLE
        else:
            self.exception = EXC_NONE

        # Compute PC + 4 using an adder
        self.pcplus4 = Pipe.cpu.adder_pcplus4.op(self.pc, 4)

        """
        # Select next PC
        self.pc_next =  self.pcplus4            if Pipe.CTL.pc_sel == PC_4      else \
                        Pipe.EX.brjmp_target    if Pipe.CTL.pc_sel == PC_BRJMP  else \
                        Pipe.EX.jump_reg_target if Pipe.CTL.pc_sel == PC_JALR   else \
                        WORD(0)
        """
         
                        
        # CHANGE: BTB
        self.pc_next = self.pcplus4
        opcode = RISCV.opcode(self.inst)
        if opcode in [ JAL, BEQ, BNE, BLT, BGE, BLTU, BGEU ]:
            if (Pipe.EX.btb.lookup(self.pc) == 0):
                self.pc_next = self.pcplus4
                self.taken = 0
            else:
                self.pc_next = Pipe.EX.btb.lookup(self.pc)
                self.taken = 1     
        if opcode in [ JALR ]:
            self.pc_next = self.pcplus4     


    def update(self):

        if not Pipe.CTL.IF_stall:
            IF.reg_pc           = self.pc_next

        if (Pipe.CTL.ID_bubble and Pipe.CTL.ID_stall):
            sys.exit(1)
        
        if Pipe.CTL.ID_bubble:
            ID.reg_pc           = self.pc
            ID.reg_inst         = WORD(BUBBLE)
            ID.reg_exception    = WORD(EXC_NONE)
            ID.reg_pcplus4      = WORD(0)
        elif not Pipe.CTL.ID_stall:
            ID.reg_pc           = self.pc
            ID.reg_inst         = self.inst
            ID.reg_exception    = self.exception
            ID.reg_pcplus4      = self.pcplus4
        else:               # Pipe.CTL.ID_stall
            pass            # Do not update
        
        ID.reg_taken = self.taken

        Pipe.log(S_IF, self.pc, self.inst, self.log())

    def log(self):
        return ("# inst=0x%08x, pc_next=0x%08x" % (self.inst, self.pc_next))


#--------------------------------------------------------------------------
#   ID: Instruction decode stage
#--------------------------------------------------------------------------

class ID(Pipe):


    # Pipeline registers ------------------------------

    reg_pc          = WORD(0)           # ID.reg_pc
    reg_inst        = WORD(BUBBLE)      # ID.reg_inst
    reg_exception   = WORD(EXC_NONE)    # ID.reg_exception
    reg_pcplus4     = WORD(0)           # ID.reg_pcplus4
    reg_taken       = WORD(0)

    #--------------------------------------------------


    def __init__(self):
        super().__init__()

        # Internal signals:----------------------------
        #
        #   self.pc                 # Pipe.ID.pc
        #   self.inst               # Pipe.ID.inst
        #   self.exception          # Pipe.ID.exception
        #   self.pcplus4            # Pipe.ID.pcplus4
        #
        #   self.rs1                # Pipe.ID.rs1
        #   self.rs2                # Pipe.ID.rs2
        #   self.rd                 # Pipe.ID.rd
        #   self.op1_data           # Pipe.ID.op1_data
        #   self.op2_data           # Pipe.ID.op2_data
        #   self.rs2_data           # Pipe.ID.rs2_data
        #
        #----------------------------------------------
        self.saving_pop_rd = 0
        self.taken = 0

    def compute(self):

        # Readout pipeline register values
        self.pc         = ID.reg_pc
        self.inst       = ID.reg_inst
        self.exception  = ID.reg_exception
        self.pcplus4    = ID.reg_pcplus4

        self.rs1        = RISCV.rs1(self.inst)          # for CTL (forwarding check)
        self.rs2        = RISCV.rs2(self.inst)          # for CTL (forwarding check)
        self.rd         = RISCV.rd(self.inst)
        
        self.taken = ID.reg_taken
        
        
        # CHANGE: for POP, PUSH instruction, rs1 shoud be sp(=2)
        if ((self.reg_inst & PUSH_MASK) == PUSH): 
            self.rs1 = Pipe.ID.rs1 = 2
            self.rd = Pipe.ID.rd = 2
        if ((self.reg_inst & POP_MASK) == POP): 
            self.rs1 = Pipe.ID.rs1 = 2
            self.saving_pop_rd = self.rd
            self.rd = 2
            

        rf_rs1_data, rf_rs2_data = Pipe.cpu.rf.read(self.rs1, self.rs2)

        imm_i           = RISCV.imm_i(self.inst)
        imm_s           = RISCV.imm_s(self.inst)
        imm_b           = RISCV.imm_b(self.inst)
        imm_u           = RISCV.imm_u(self.inst)
        imm_j           = RISCV.imm_j(self.inst)

        # Generate control signals
        # CTL.gen() should be called after getting register numbers to detect forwarding condition

        if not Pipe.CTL.gen(self.inst):
            self.inst = BUBBLE             

        # Determine ALU operand 2: R[rs2] or immediate values
        alu_op2 =       rf_rs2_data     if Pipe.CTL.op2_sel == OP2_RS2      else \
                        imm_i           if Pipe.CTL.op2_sel == OP2_IMI      else \
                        imm_s           if Pipe.CTL.op2_sel == OP2_IMS      else \
                        imm_b           if Pipe.CTL.op2_sel == OP2_IMB      else \
                        imm_u           if Pipe.CTL.op2_sel == OP2_IMU      else \
                        imm_j           if Pipe.CTL.op2_sel == OP2_IMJ      else \
                        WORD(0)
                        
                        
        # CHANGE: want to make 4
        tmp = alu_op2
        alu_op2 =   4       if (self.reg_inst & PUSH_MASK) == PUSH  else \
                    4       if (self.reg_inst & POP_MASK) == POP    else \
                    tmp


        # Determine ALU operand 1: PC or R[rs1]
        # Get forwarded value for rs1 if necessary
        # The order matters: EX -> MM -> WB (forwarding from the closest stage)
        self.op1_data = self.pc         if Pipe.CTL.op1_sel == OP1_PC       else \
                        Pipe.EX.alu_out if Pipe.CTL.fwd_op1 == FWD_EX       else \
                        Pipe.MM.wbdata  if Pipe.CTL.fwd_op1 == FWD_MM       else \
                        Pipe.WB.wbdata  if Pipe.CTL.fwd_op1 == FWD_WB       else \
                        rf_rs1_data

        # Get forwarded value for rs2 if necessary
        # The order matters: EX -> MM -> WB (forwarding from the closest stage)
        self.op2_data = Pipe.EX.alu_out if Pipe.CTL.fwd_op2 == FWD_EX       else \
                        Pipe.MM.wbdata  if Pipe.CTL.fwd_op2 == FWD_MM       else \
                        Pipe.WB.wbdata  if Pipe.CTL.fwd_op2 == FWD_WB       else \
                        alu_op2

        # Get forwarded value for rs2 if necessary
        # The order matters: EX -> MM -> WB (forwarding from the closest stage)
        # For sw and branch instructions, we need to carry R[rs2] as well
        # -- in these instructions, op2_data will hold an immediate value
        self.rs2_data = Pipe.EX.alu_out if Pipe.CTL.fwd_rs2 == FWD_EX       else \
                        Pipe.MM.wbdata  if Pipe.CTL.fwd_rs2 == FWD_MM       else \
                        Pipe.WB.wbdata  if Pipe.CTL.fwd_rs2 == FWD_WB       else \
                        rf_rs2_data
                        
        
        # CHANGE: load-use for POP
        if ((Pipe.EX.reg_inst == BUBBLE) or (Pipe.EX.reg_rd != 2)) and ((Pipe.MM.reg_inst == BUBBLE) or \
            (Pipe.MM.reg_rd != 2)) and (Pipe.WB.reg_inst & POP_MASK == POP):
            if (self.rs1 == 2) and Pipe.CTL.rs1_oen and (Pipe.CTL.fwd_op1 != FWD_EX) and (Pipe.CTL.fwd_op1 != FWD_MM):
                self.op1_data = Pipe.WB.alu_out
            if (self.rs2 == 2) and Pipe.CTL.rs2_oen and (Pipe.CTL.fwd_rs2 != FWD_EX) and (Pipe.CTL.fwd_rs2 != FWD_MM):
                self.rs2_data = Pipe.WB.alu_out
            if (self.rs2 == 2) and Pipe.CTL.rs2_oen and (Pipe.CTL.fwd_op2 != FWD_EX) and (Pipe.CTL.fwd_op2 != FWD_MM) and (self.inst & PUSH_MASK != PUSH):
                self.op2_data = Pipe.WB.alu_out 
        if ((Pipe.EX.reg_inst == BUBBLE) or (Pipe.EX.reg_rd != 2)) and (Pipe.MM.reg_inst & POP_MASK == POP):
            if (self.rs1 == 2) and Pipe.CTL.rs1_oen and (Pipe.CTL.fwd_op1 != FWD_EX):
                self.op1_data = Pipe.MM.alu_out
            if (self.rs2 == 2) and Pipe.CTL.rs2_oen and (Pipe.CTL.fwd_rs2 != FWD_EX):
                self.rs2_data = Pipe.MM.alu_out    
            if (self.rs2 == 2) and Pipe.CTL.rs2_oen and (Pipe.CTL.fwd_op2 != FWD_EX) and (self.inst & PUSH_MASK != PUSH):
                self.op2_data = Pipe.MM.alu_out 
                

    def update(self):

        EX.reg_pc                   = self.pc

        if Pipe.CTL.EX_bubble:
            EX.reg_inst             = WORD(BUBBLE)
            EX.reg_exception        = WORD(EXC_NONE)
            EX.reg_c_br_type        = WORD(BR_N)
            EX.reg_c_rf_wen         = False
            EX.reg_c_dmem_en        = False
        else:
            EX.reg_inst             = self.inst
            EX.reg_exception        = self.exception
            EX.reg_rd               = self.rd
            EX.reg_op1_data         = self.op1_data
            EX.reg_op2_data         = self.op2_data
            EX.reg_rs2_data         = self.rs2_data
            EX.reg_c_br_type        = Pipe.CTL.br_type
            EX.reg_c_alu_fun        = Pipe.CTL.alu_fun
            EX.reg_c_wb_sel         = Pipe.CTL.wb_sel
            EX.reg_c_rf_wen         = Pipe.CTL.rf_wen
            EX.reg_c_dmem_en        = Pipe.CTL.dmem_en
            EX.reg_c_dmem_rw        = Pipe.CTL.dmem_rw
            EX.reg_pcplus4          = self.pcplus4
            EX.reg_saving_pop_rd    = self.saving_pop_rd
            EX.reg_rs2              = self.rs2
            
        EX.reg_taken = self.taken

        Pipe.log(S_ID, self.pc, self.inst, self.log())     

    def log(self):
        if self.inst in [ BUBBLE, ILLEGAL ]:
            return('# -')
        else:
            # return("# rd=%d rs1=%d rs2=%d op1=0x%08x op2=0x%08x" % (self.rd, self.rs1, self.rs2, self.op1_data, self.op2_data))
            return("# rd=%d rs1=%d rs2=%d op1=0x%08x op2=0x%08x FWD_op1=%d FWD_op2=%d" % (self.rd, self.rs1, self.rs2, self.op1_data, self.op2_data,Pipe.CTL.fwd_op1,Pipe.CTL.fwd_op2))


#--------------------------------------------------------------------------
#   EX: Execution stage
#--------------------------------------------------------------------------

class EX(Pipe):

    # Pipeline registers ------------------------------

    reg_pc              = WORD(0)           # EX.reg_pc
    reg_inst            = WORD(BUBBLE)      # EX.reg_inst
    reg_exception       = WORD(EXC_NONE)    # EX.exception
    reg_rd              = WORD(0)           # EX.reg_rd
    reg_c_rf_wen        = False             # EX.reg_c_rf_wen
    reg_c_wb_sel        = WORD(WB_X)        # EX.reg_c_wb_sel
    reg_c_dmem_en       = False             # EX.reg_c_dmem_en
    reg_c_dmem_rw       = WORD(M_X)         # EX.reg_c_dmem_rw
    reg_c_br_type       = WORD(BR_N)        # EX.reg_c_br_type
    reg_c_alu_fun       = WORD(ALU_X)       # EX.reg_c_alu_fun
    reg_op1_data        = WORD(0)           # EX.reg_op1_data
    reg_op2_data        = WORD(0)           # EX.reg_op2_data
    reg_rs2_data        = WORD(0)           # EX.reg_rs2_data
    reg_pcplus4         = WORD(0)           # EX.reg_pcplus4
    reg_saving_pop_rd   = WORD(0) 
    reg_taken           = WORD(0)
    reg_rs2             = WORD(0)

    #--------------------------------------------------


    def __init__(self):
        super().__init__()

        # Internal signals:----------------------------
        #
        #   self.pc                 # Pipe.EX.pc
        #   self.inst               # Pipe.EX.inst
        #   self.exception          # Pipe.EX.exception
        #   self.rd                 # Pipe.EX.rd
        #   self.c_rf_wen           # Pipe.EX.c_rf_wen
        #   self.c_wb_sel           # Pipe.EX.c_wb_sel
        #   self.c_dmem_en          # Pipe.EX.c_dmem_en
        #   self.c_dmem_rw          # Pipe.EX.c_dmem_fcn
        #   self.c_br_type          # Pipe.EX.c_br_type
        #   self.c_alu_fun          # Pipe.EX.c_alu_fun
        #   self.op1_data           # Pipe.EX.op1_data
        #   self.op2_data           # Pipe.EX.op2_data
        #   self.rs2_data           # Pipe.EX.rs2_data
        #   self.pcplus4            # Pipe.EX.pcplus4
        #
        #   self.alu2_data          # Pipe.EX.alu2_data
        #   self.alu_out            # Pipe.EX.alu_out
        #   self.brjmp_target       # Pipe.EX.brjmp_target
        #   self.jump_reg_target    # Pipe.EX.jump_reg_target
        #
        #----------------------------------------------

        self.btb = BTB(Log.btb_k)
        self.taken = 0

    def compute(self):

        # Readout pipeline register values
        self.pc                 = EX.reg_pc
        self.inst               = EX.reg_inst
        self.exception          = EX.reg_exception
        self.rd                 = EX.reg_rd
        self.c_rf_wen           = EX.reg_c_rf_wen
        self.c_wb_sel           = EX.reg_c_wb_sel
        self.c_dmem_en          = EX.reg_c_dmem_en
        self.c_dmem_rw          = EX.reg_c_dmem_rw
        self.c_br_type          = EX.reg_c_br_type
        self.c_alu_fun          = EX.reg_c_alu_fun
        self.op1_data           = EX.reg_op1_data
        self.op2_data           = EX.reg_op2_data
        self.rs2_data           = EX.reg_rs2_data
        self.pcplus4            = EX.reg_pcplus4
        self.saving_pop_rd      = EX.reg_saving_pop_rd 
        self.taken              = EX.reg_taken
        self.rs2                = EX.reg_rs2


        # For branch instructions, we use ALU to make comparisons between rs1 and rs2.
        # Since op2_data has an immediate value (offset) for branch instructions,
        # we change the input of ALU to rs2_data.
        self.alu2_data  = self.rs2_data     if self.c_br_type in [ BR_NE, BR_EQ, BR_GE, BR_GEU, BR_LT, BR_LTU ] else \
                          self.op2_data
        
        # Perform ALU operation
        self.alu_out = Pipe.cpu.alu.op(self.c_alu_fun, self.op1_data, self.alu2_data)

        # Adjust the output for jalr instruction (forwarded to IF)
        self.jump_reg_target    = self.alu_out & WORD(0xfffffffe) 

        # Calculate the branch/jump target address using an adder (forwarded to IF)
        self.brjmp_target       = Pipe.cpu.adder_brtarget.op(self.pc, self.op2_data) 

        # For jal and jalr instructions, pc+4 should be written to the rd
        if self.c_wb_sel == WB_PC4:                   
            self.alu_out        = self.pcplus4


    def update(self):

        MM.reg_pc                   = self.pc
        # Exception should not be cleared in MM even if MM_bubble is enabled.
        # Otherwise we will lose any exception status.
        # For cancelled instructions, exception has been cleared already
        # as they enter ID or EX stage.
        MM.reg_exception            = self.exception

        if Pipe.CTL.MM_bubble:
            MM.reg_inst             = WORD(BUBBLE)
            MM.reg_c_rf_wen         = False
            MM.reg_c_dmem_en        = False
        else:
            MM.reg_inst             = self.inst
            MM.reg_rd               = self.rd
            MM.reg_c_rf_wen         = self.c_rf_wen
            MM.reg_c_wb_sel         = self.c_wb_sel
            MM.reg_c_dmem_en        = self.c_dmem_en
            MM.reg_c_dmem_rw        = self.c_dmem_rw
            MM.reg_alu_out          = self.alu_out
            MM.reg_rs2_data         = self.rs2_data
            MM.reg_rs2              = self.rs2
            
            
            # CHANGE: control rd value for POP
            if ((self.inst & POP_MASK) == POP):
                MM.reg_rd = self.saving_pop_rd
        
        
        # CHANGE: BTB
        opcode = RISCV.opcode(self.inst)
        flush = False
        if opcode in [ BEQ, BLT, BLTU ]:
            if (self.alu_out == 1) and (self.taken == 0):
                flush = True
                IF.reg_pc = self.brjmp_target
                self.btb.add(self.pc, self.brjmp_target)
            elif (self.alu_out == 0) and (self.taken == 1):
                flush = True
                IF.reg_pc = self.pcplus4
                self.btb.add(self.pc, self.pcplus4)
        if opcode in [ BNE, BGE, BGEU ]:
            if (self.alu_out == 0) and (self.taken == 0):
                flush = True
                IF.reg_pc = self.brjmp_target
                self.btb.add(self.pc, self.brjmp_target)
            elif (self.alu_out == 1) and (self.taken == 1):
                flush = True
                IF.reg_pc = self.pcplus4
                self.btb.add(self.pc, self.pcplus4)
        if opcode in [ JAL ]:
            if (self.taken == 0):
                flush = True
                IF.reg_pc = self.brjmp_target
                self.btb.add(self.pc, self.brjmp_target)
        if opcode in [ JALR ]:
            flush = True
            IF.reg_pc = self.jump_reg_target
        if flush:     
            ID.reg_inst        = WORD(BUBBLE)     
            ID.reg_exception   = WORD(EXC_NONE)   
            ID.reg_pcplus4     = WORD(0)        
            EX.reg_inst        = WORD(BUBBLE)
            EX.reg_exception   = WORD(EXC_NONE)
            EX.reg_c_br_type   = WORD(BR_N)
            EX.reg_c_rf_wen    = False
            EX.reg_c_dmem_en   = False
            
        Pipe.log(S_EX, self.pc, self.inst, self.log())
        

    def log(self):

        ALU_OPS = {
            ALU_X       : f'# -',
            ALU_ADD     : f'# {self.alu_out:#010x} <- {self.op1_data:#010x} + {self.alu2_data:#010x}',
            ALU_SUB     : f'# {self.alu_out:#010x} <- {self.op1_data:#010x} - {self.alu2_data:#010x}',
            ALU_AND     : f'# {self.alu_out:#010x} <- {self.op1_data:#010x} & {self.alu2_data:#010x}',
            ALU_OR      : f'# {self.alu_out:#010x} <- {self.op1_data:#010x} | {self.alu2_data:#010x}',
            ALU_XOR     : f'# {self.alu_out:#010x} <- {self.op1_data:#010x} ^ {self.alu2_data:#010x}',
            ALU_SLT     : f'# {self.alu_out:#010x} <- {self.op1_data:#010x} < {self.alu2_data:#010x} (signed)',
            ALU_SLTU    : f'# {self.alu_out:#010x} <- {self.op1_data:#010x} < {self.alu2_data:#010x} (unsigned)',
            ALU_SLL     : f'# {self.alu_out:#010x} <- {self.op1_data:#010x} << {self.alu2_data & 0x1f}',
            ALU_SRL     : f'# {self.alu_out:#010x} <- {self.op1_data:#010x} >> {self.alu2_data & 0x1f} (logical)',
            ALU_SRA     : f'# {self.alu_out:#010x} <- {self.op1_data:#010x} >> {self.alu2_data & 0x1f} (arithmetic)',
            ALU_COPY1   : f'# {self.alu_out:#010x} <- {self.op1_data:#010x} (pass 1)',
            ALU_COPY2   : f'# {self.alu_out:#010x} <- {self.alu2_data:#010x} (pass 2)',
            ALU_SEQ     : f'# {self.alu_out:#010x} <- {self.op1_data:#010x} == {self.alu2_data:#010x}',
        }
        return('# -' if self.inst == BUBBLE else ALU_OPS[self.c_alu_fun]);


#--------------------------------------------------------------------------
#   MM: Memory access stage
#--------------------------------------------------------------------------

class MM(Pipe):

    # Pipeline registers ------------------------------

    reg_pc              = WORD(0)           # MM.reg_pc
    reg_inst            = WORD(BUBBLE)      # MM.reg_inst
    reg_exception       = WORD(EXC_NONE)    # MM.reg_exception
    reg_rd              = WORD(0)           # MM.reg_rd
    reg_c_rf_wen        = False             # MM.reg_c_rf_wen
    reg_c_wb_sel        = WORD(WB_X)        # MM.reg_c_wb_sel
    reg_c_dmem_en       = False             # MM.reg_c_dmem_en
    reg_c_dmem_rw       = WORD(M_X)         # MM.reg_c_dmem_rw
    reg_alu_out         = WORD(0)           # MM.reg_alu_out
    reg_rs2_data        = WORD(0)           # MM.reg_rs2_data
    reg_rs2             = WORD(0)

    #--------------------------------------------------

    def __init__(self):
        super().__init__()

        # Internal signals:----------------------------
        #
        #   self.pc                 # Pipe.MM.pc
        #   self.inst               # Pipe.MM.inst
        #   self.exception          # Pipe.MM.exception
        #   self.rd                 # Pipe.MM.rd
        #   self.c_rf_wen           # Pipe.MM.c_rf_wen
        #   self.c_wb_sel           # Pipe.MM.c_rf_wen
        #   self.c_dmem_en          # Pipe.MM.c_dmem_en
        #   self.c_dmem_rw          # Pipe.MM.c_dmem_rw
        #   self.alu_out            # Pipe.MM.alu_out
        #   self.rs2_data           # Pipe.MM.rs2_data
        #
        #   self.wbdata             # Pipe.MM.wbdata
        #
        #----------------------------------------------

    def compute(self):

        self.pc             = MM.reg_pc
        self.inst           = MM.reg_inst
        self.exception      = MM.reg_exception
        self.rd             = MM.reg_rd
        self.c_rf_wen       = MM.reg_c_rf_wen
        self.c_wb_sel       = MM.reg_c_wb_sel
        self.c_dmem_en      = MM.reg_c_dmem_en
        self.c_dmem_rw      = MM.reg_c_dmem_rw
        self.alu_out        = MM.reg_alu_out  
        self.rs2_data       = MM.reg_rs2_data   
        self.rs2            = MM.reg_rs2      
        
        
        # CHANGE: write back sp for PUSH instruction
        if (self.inst & PUSH_MASK == PUSH):
            if (self.rs2 == 2):
                self.rs2_data = self.alu_out


        # Access data memory (dmem) if needed
        mem_data, status = Pipe.cpu.dmem.access(self.c_dmem_en, self.alu_out, self.rs2_data, self.c_dmem_rw)
        
        
        # CHANGE: POP instruction need sp-4
        if ((self.reg_inst & POP_MASK) == POP): 
            mem_data, status = Pipe.cpu.dmem.access(self.c_dmem_en, self.alu_out-4, self.rs2_data, self.c_dmem_rw)
            

        # Handle exception during dmem access
        if not status:
            self.exception |= EXC_DMEM_ERROR
            self.c_rf_wen   = False

        # For load instruction, we need to store the value read from dmem
        self.wbdata         = mem_data          if self.c_wb_sel == WB_MEM  else \
                              self.alu_out              
                              

        # CHANGE: write back sp for PUSH instruction
        if (self.inst & PUSH_MASK == PUSH):
            self.wbdata = self.alu_out
            if (self.rs2 == 2):
                self.rs2_data = self.alu_out
        
    
    def update(self):
    
        WB.reg_pc           = self.pc
        WB.reg_inst         = self.inst
        WB.reg_exception    = self.exception
        WB.reg_rd           = self.rd
        WB.reg_c_rf_wen     = self.c_rf_wen
        WB.reg_wbdata       = self.wbdata
        WB.reg_alu_out      = self.alu_out

        Pipe.log(S_MM, self.pc, self.inst, self.log())
        
        
        # CHANGE: additional write back for POP instruction
        WB.reg_wbdata2 = self.alu_out


    def log(self):
        if not self.c_dmem_en:
            return('# -')
        elif self.c_dmem_rw == M_XRD:
            # CHANGE: additional write back for POP instruction
            if ((self.inst & POP_MASK) == POP): 
                return('# 0x%08x <- M[0x%08x]' % (self.wbdata, self.alu_out-4))
            return('# 0x%08x <- M[0x%08x]' % (self.wbdata, self.alu_out))
        else:
            return('# M[0x%08x] <- 0x%08x' % (self.alu_out, self.rs2_data))


#--------------------------------------------------------------------------
#   WB: Write back stage
#--------------------------------------------------------------------------

class WB(Pipe):

    # Pipeline registers ------------------------------

    reg_pc              = WORD(0)           # WB.reg_pc
    reg_inst            = WORD(BUBBLE)      # WB.reg_inst
    reg_exception       = WORD(EXC_NONE)    # WB.reg_exception
    reg_rd              = WORD(0)           # WB.reg_rd
    reg_c_rf_wen        = False             # WB.reg_c_rf_wen
    reg_wbdata          = WORD(0)           # WB.reg_wbdata
    reg_wbdata2         = WORD(0)
    reg_alu_out         = WORD(0)

    #--------------------------------------------------


    def __init__(self):
        super().__init__()

    def compute(self):

        # Readout pipeline register values
        self.pc                 = WB.reg_pc    
        self.inst               = WB.reg_inst  
        self.exception          = WB.reg_exception      
        self.rd                 = WB.reg_rd    
        self.c_rf_wen           = WB.reg_c_rf_wen 
        self.wbdata             = WB.reg_wbdata
        self.wbdata2            = WB.reg_wbdata2
        self.alu_out            = WB.reg_alu_out

    def update(self):        

        if self.c_rf_wen:
            Pipe.cpu.rf.write(self.rd, self.wbdata) 
            
        # CHANGE: additional write back for POP instruction
        if ((self.inst & POP_MASK) == POP): 
            Pipe.cpu.rf.write(2, self.wbdata2)

        Pipe.log(S_WB, self.pc, self.inst, self.log())

        if (self.exception):
            return False
        else:
            return True

    def log(self):
        if self.inst == BUBBLE or (not self.c_rf_wen):
            return('# -')
        else:
            # CHANGE: additional write back for POP instruction
            s = ""
            if ((self.inst & POP_MASK) == POP): 
                s = ('# R[%d] <- 0x%08x    ' % (2, self.wbdata2))
            return(s + '# R[%d] <- 0x%08x' % (self.rd, self.wbdata))


#--------------------------------------------------------------------------
#   Control: Control logic (executed in ID stage)
#--------------------------------------------------------------------------

class Control(object):

    def __init__(self):
        super().__init__()

        # Internal signals:----------------------------
        #
        #   self.pc_sel             # Pipe.CTL.pc_sel
        #   self.br_type            # Pipe.CTL.br_type
        #   self.op1_sel            # Pipe.CTL.op1_sel
        #   self.op2_sel            # Pipe.CTL.op2_sel
        #   self.alu_fun            # Pipe.CTL.alu_fun
        #   self.wb_sel             # Pipe.CTL.wb_sel
        #   self.rf_wen             # Pipe.CTL.rf_wen
        #   self.fwd_op1            # Pipe.CTL.fwd_op1
        #   self.fwd_op2            # Pipe.CTL.fwd_op2
        #   self.imem_en            # Pipe.CTL.imem_en
        #   self.imem_rw            # Pipe.CTL.imem_rw
        #   self.dmem_en            # Pipe.CTL.dmem_en
        #   self.dmem_rw            # Pipe.CTL.dmem_rw
        #   self.IF_stall           # Pipe.CTL.IF_stall
        #   self.ID_stall           # Pipe.CTL.ID_stall
        #   self.ID_bubble          # Pipe.CTL.ID_bubble
        #   self.EX_bubble          # Pipe.CTL.EX_bubble
        #   self.MM_bubble          # Pipe.CTL.MM_bubble
        #
        #----------------------------------------------


        # These signals are used before gen() is called
        self.imem_en        = True
        self.imem_rw        = M_XRD


    def gen(self, inst):

        opcode = RISCV.opcode(inst)
        if opcode in [ EBREAK, ECALL ]:
            Pipe.ID.exception |= EXC_EBREAK
        elif opcode == ILLEGAL:
            Pipe.ID.exception |= EXC_ILLEGAL_INST
            inst = BUBBLE
            opcode = RISCV.opcode(inst)

        self.IF_stall       = False
        self.ID_stall       = False
        self.ID_bubble      = False
        self.EX_bubble      = False
        self.MM_bubble      = False     

        cs = csignals[opcode]

        self.br_type        = cs[CS_BR_TYPE]
        self.op1_sel        = cs[CS_OP1_SEL]
        self.op2_sel        = cs[CS_OP2_SEL]
        self.alu_fun        = cs[CS_ALU_FUN]
        self.wb_sel         = cs[CS_WB_SEL]
        self.rf_wen         = cs[CS_RF_WEN]

        rs1_oen             = cs[CS_RS1_OEN]
        rs2_oen             = cs[CS_RS2_OEN]

        self.dmem_en        = cs[CS_MEM_EN]
        self.dmem_rw        = cs[CS_MEM_FCN]
        
        self.rs1_oen = rs1_oen
        self.rs2_oen = rs2_oen

        # Control signal to select the next PC
        self.pc_sel         =   PC_BRJMP    if (EX.reg_c_br_type == BR_NE  and (not Pipe.EX.alu_out)) or    \
                                               (EX.reg_c_br_type == BR_EQ  and Pipe.EX.alu_out) or          \
                                               (EX.reg_c_br_type == BR_GE  and (not Pipe.EX.alu_out)) or    \
                                               (EX.reg_c_br_type == BR_GEU and (not Pipe.EX.alu_out)) or    \
                                               (EX.reg_c_br_type == BR_LT  and Pipe.EX.alu_out) or          \
                                               (EX.reg_c_br_type == BR_LTU and Pipe.EX.alu_out) or          \
                                               (EX.reg_c_br_type == BR_J) else                              \
                                PC_JALR     if  EX.reg_c_br_type == BR_JR else                              \
                                PC_4

        # Control signal for forwarding rs1 value to op1_data
        # The c_rf_wen signal can be disabled when we have an exception during dmem access,
        # so Pipe.MM.c_rf_wen should be used instead of MM.reg_c_rf_wen.
        self.fwd_op1        =   FWD_EX      if (EX.reg_rd == Pipe.ID.rs1) and rs1_oen and   \
                                               (EX.reg_rd != 0) and EX.reg_c_rf_wen else    \
                                FWD_MM      if (MM.reg_rd == Pipe.ID.rs1) and rs1_oen and   \
                                               (MM.reg_rd != 0) and Pipe.MM.c_rf_wen else   \
                                FWD_WB      if (WB.reg_rd == Pipe.ID.rs1) and rs1_oen and   \
                                               (WB.reg_rd != 0) and WB.reg_c_rf_wen else    \
                                FWD_NONE

        # Control signal for forwarding rs2 value to op2_data
        self.fwd_op2        =   FWD_EX      if (EX.reg_rd == Pipe.ID.rs2) and               \
                                               (EX.reg_rd != 0) and EX.reg_c_rf_wen and     \
                                               self.op2_sel == OP2_RS2 else                 \
                                FWD_MM      if (MM.reg_rd == Pipe.ID.rs2) and               \
                                               (MM.reg_rd != 0) and Pipe.MM.c_rf_wen and    \
                                               self.op2_sel == OP2_RS2 else                 \
                                FWD_WB      if (WB.reg_rd == Pipe.ID.rs2) and               \
                                               (WB.reg_rd != 0) and WB.reg_c_rf_wen and     \
                                               self.op2_sel == OP2_RS2 else                 \
                                FWD_NONE
                                
                                
        # CHANGE: forwarding logic for PUSH, POP instruction
        if opcode in [ PUSH, POP ]:
            self.fwd_op1    =   FWD_EX      if (EX.reg_rd == 2) and EX.reg_c_rf_wen and rs1_oen else        \
                                FWD_MM      if (MM.reg_rd == 2) and Pipe.MM.reg_c_rf_wen and rs1_oen else   \
                                FWD_WB      if (WB.reg_rd == 2) and WB.reg_c_rf_wen and rs1_oen else        \
                                FWD_NONE


        # Control signal for forwarding rs2 value to rs2_data
        self.fwd_rs2        =   FWD_EX      if (EX.reg_rd == Pipe.ID.rs2) and rs2_oen and   \
                                               (EX.reg_rd != 0) and EX.reg_c_rf_wen  else   \
                                FWD_MM      if (MM.reg_rd == Pipe.ID.rs2) and rs2_oen and   \
                                               (MM.reg_rd != 0) and Pipe.MM.c_rf_wen else   \
                                FWD_WB      if (WB.reg_rd == Pipe.ID.rs2) and rs2_oen and   \
                                               (WB.reg_rd != 0) and WB.reg_c_rf_wen  else   \
                                FWD_NONE

        # Check for load-use data hazard
        EX_load_inst = EX.reg_c_dmem_en and EX.reg_c_dmem_rw == M_XRD
        load_use_hazard     = (EX_load_inst and EX.reg_rd != 0) and             \
                              ((EX.reg_rd == Pipe.ID.rs1 and rs1_oen) or        \
                               (EX.reg_rd == Pipe.ID.rs2 and rs2_oen))
                              
                              
        # CHANGE: load-use for POP instruction                      
        if (not load_use_hazard):
            load_use_hazard     = (EX_load_inst and EX.reg_saving_pop_rd != 0) and          \
                                  (EX.reg_inst & POP_MASK == POP) and                       \
                                  ((EX.reg_saving_pop_rd == Pipe.ID.rs1 and rs1_oen) or     \
                                   (EX.reg_saving_pop_rd == Pipe.ID.rs2 and rs2_oen))
                                  
        
        '''
        # Check for mispredicted branch/jump
        EX_brjmp            = self.pc_sel != PC_4
        self.ID_bubble      = EX_brjmp 
        self.EX_bubble      = load_use_hazard or EX_brjmp
        '''

        # For load-use hazard, ID and IF are stalled for one cycle (and EX bubbled)
        # For mispredicted branches, instructions in ID and IF should be cancelled (become BUBBLE)
        self.IF_stall       = load_use_hazard 
        self.ID_stall       = load_use_hazard
        self.ID_bubble      = False
        self.EX_bubble      = False
        self.EX_bubble      = load_use_hazard

        # Any instruction with an exception becomes BUBBLE as it enters the MM stage. 
        # This is because the instruction can be cancelled while it is in IF and ID due to mispredicted 
        # branch/jump, in which case it should not cause any exception. We just keep track of the exception 
        # state with the instruction along the pipeline until EX. If the instruction survives EX, it is 
        # safe to make the instruction and any following instructions bubble (except for EBREAK)
        self.MM_bubble = (Pipe.EX.exception and (Pipe.EX.exception != EXC_EBREAK)) or (Pipe.MM.exception)
       
        if inst == BUBBLE:
            return False
        else:
            return True


