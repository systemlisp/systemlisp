(in-package :system-lisp-examples)

;;; FemtoRV32 processor 
(defclass femtorv32 (sl-component)
  (;; Ports
   ;; - Clock and reset
   (clk
    :accessor clk
    :initform (make-instance 'sl-signal-binary :bit-width 1 :dir 'input))
   (reset
    :accessor reset
    :initform (make-instance 'sl-signal-binary :bit-width 1 :dir 'input))
   ;; - Memory interface
   (mem-addr
    :accessor mem-addr
    :initform (make-instance 'sl-signal-binary :bit-width 32 :dir 'output))
   (mem-wdata
    :accessor mem-wdata
    :initform (make-instance 'sl-signal-binary :bit-width 32 :dir 'output))
   (mem-wmask
    :accessor mem-wmask
    :initform (make-instance 'sl-signal-binary :bit-width 4 :dir 'output))
   (mem-rdata
    :accessor mem-rdata
    :initform (make-instance 'sl-signal-binary :bit-width 32 :dir 'input))
   (mem-rstrb
    :accessor mem-rstrb
    :initform (make-instance 'sl-signal-binary :bit-width 1 :dir 'output))
   (mem-rbusy
    :accessor mem-rbusy
    :initform (make-instance 'sl-signal-binary :bit-width 1 :dir 'input))
   (mem-wbusy
    :accessor mem-wbusy
    :initform (make-instance 'sl-signal-binary :bit-width 1 :dir 'input))
   ;; Wires
   ;; - Wires for output ports
   (mem-addr-lambda
    :accessor mem-addr-lambda
    :initform (make-instance 'sl-signal-binary-lambda :bit-width 32))
   (mem-wdata-lambda
    :accessor mem-wdata-lambda
    :initform (make-instance 'sl-signal-binary-lambda :bit-width 32))
   (mem-wmask-lambda
    :accessor mem-wmask-lambda
    :initform (make-instance 'sl-signal-binary-lambda :bit-width 4))
   (mem-rstrb-lambda
    :accessor mem-rstrb-lambda
    :initform (make-instance 'sl-signal-binary-lambda :bit-width 1))
   ;; - Instruction decoding
   (rdId
    :accessor rdId
    :initform (make-instance 'sl-signal-binary-lambda :bit-width 5)
    :documentation "The destination register")
   (funct3-is
    :accessor funct3-is
    :initform (make-instance 'sl-signal-binary-lambda :bit-width 8)
    :documentation "The ALU function decoded in 1-hot form")
   (Uimm
    :accessor Uimm
    :initform (make-instance 'sl-signal-binary-lambda :bit-width 32))
   (Iimm
    :accessor Iimm
    :initform (make-instance 'sl-signal-binary-lambda :bit-width 32))
   (Simm
    :accessor Simm
    :initform (make-instance 'sl-signal-binary-lambda :bit-width 32))
   (Bimm
    :accessor Bimm
    :initform (make-instance 'sl-signal-binary-lambda :bit-width 32))
   (Jimm
    :accessor Jimm
    :initform (make-instance 'sl-signal-binary-lambda :bit-width 32))
   ;;  - RV32I has 10 different instructions
   (is-load
    :accessor is-load
    :initform (make-instance 'sl-signal-binary-lambda :bit-width 1))
   (is-ALU-imm
    :accessor is-ALU-imm
    :initform (make-instance 'sl-signal-binary-lambda :bit-width 1))
   (is-store
    :accessor is-store
    :initform (make-instance 'sl-signal-binary-lambda :bit-width 1))
   (is-ALU-reg
    :accessor is-ALU-reg
    :initform (make-instance 'sl-signal-binary-lambda :bit-width 1))
   (is-SYSTEM
    :accessor is-SYSTEM
    :initform (make-instance 'sl-signal-binary-lambda :bit-width 1))
   (is-JAL
    :accessor is-JAL
    :initform (make-instance 'sl-signal-binary-lambda :bit-width 1))
   (is-JALR
    :accessor is-JALR
    :initform (make-instance 'sl-signal-binary-lambda :bit-width 1))
   (is-LUI
    :accessor is-LUI
    :initform (make-instance 'sl-signal-binary-lambda :bit-width 1))
   (is-AUIPC
    :accessor is-AUIPC
    :initform (make-instance 'sl-signal-binary-lambda :bit-width 1))
   (is-branch
    :accessor is-branch
    :initform (make-instance 'sl-signal-binary-lambda :bit-width 1))
   (is-ALU
    :accessor is-ALU
    :initform (make-instance 'sl-signal-binary-lambda :bit-width 1))
   ;; - The register file
   (rs1
    :accessor rs1
    :initform (make-instance 'sl-signal-binary :bit-width 32))
   (rs2
    :accessor rs2
    :initform (make-instance 'sl-signal-binary :bit-width 32))
   (register-file
    :accessor register-file
    :initform (make-array 32 :initial-element nil)
    :type (vector sl-signal-binary))
   ;; - ALU signals
   (alu-in1
    :accessor alu-in1
    :initform (make-instance 'sl-signal-binary-lambda :bit-width 32))
   (alu-in2
    :accessor alu-in2
    :initform (make-instance 'sl-signal-binary-lambda :bit-width 32))
   (alu-reg
    :accessor alu-reg
    :initform (make-instance 'sl-signal-binary :bit-width 32))
   (alu-shamt
    :accessor alu-shamt
    :initform (make-instance 'sl-signal-binary :bit-width 5))
   (alu-busy
    :accessor alu-busy
    :initform (make-instance 'sl-signal-binary-lambda :bit-width 1))
   (alu-wr
    :accessor alu-wr
    :initform (make-instance 'sl-signal-binary-lambda :bit-width 1))
   (alu-plus
    :accessor alu-plus
    :initform (make-instance 'sl-signal-binary-lambda :bit-width 32))
   (alu-minus
    :accessor alu-minus
    :initform (make-instance 'sl-signal-binary-lambda :bit-width 33))
   (alu-lt
    :accessor alu-lt
    :initform (make-instance 'sl-signal-binary-lambda :bit-width 1))
   (alu-ltu
    :accessor alu-ltu
    :initform (make-instance 'sl-signal-binary-lambda :bit-width 1))
   (alu-eq
    :accessor alu-eq
    :initform (make-instance 'sl-signal-binary-lambda :bit-width 1))
   (alu-out
    :accessor alu-out
    :initform (make-instance 'sl-signal-binary-lambda :bit-width 32))
   (funct3-is-shift
    :accessor funct3-is-shift
    :initform (make-instance 'sl-signal-binary-lambda :bit-width 1))
   ;; - Predicate for conditional branches
   (predicate
    :accessor predicate
    :initform (make-instance 'sl-signal-binary-lambda :bit-width 1))
   ;; - Program counter and branch target computation
   (pc
    :accessor pc
    :initform (make-instance 'sl-signal-binary :bit-width 24))
   (instr
    :accessor instr
    :initform (make-instance 'sl-signal-binary :bit-width 32))
   (pc-plus4
    :accessor pc-plus4
    :initform (make-instance 'sl-signal-binary-lambda :bit-width 24))
   (pc-plus-imm
    :accessor pc-plus-imm
    :initform (make-instance 'sl-signal-binary-lambda :bit-width 24))
   (loadstore-addr
    :accessor loadstore-addr
    :initform (make-instance 'sl-signal-binary-lambda :bit-width 24))
   ;; - The value written back to the register file
   (write-back-data
    :accessor write-back-data
    :initform (make-instance 'sl-signal-binary-lambda :bit-width 32))
   ;; - Load/store
   (mem-byte-access
    :accessor mem-byte-access
    :initform (make-instance 'sl-signal-binary-lambda :bit-width 1))
   (mem-halfword-access
    :accessor mem-halfword-access
    :initform (make-instance 'sl-signal-binary-lambda :bit-width 1))
   (load-sign
    :accessor load-sign
    :initform (make-instance 'sl-signal-binary-lambda :bit-width 1))
   (load-data
    :accessor load-data
    :initform (make-instance 'sl-signal-binary-lambda :bit-width 32))
   (load-halfword
    :accessor load-halfword
    :initform (make-instance 'sl-signal-binary-lambda :bit-width 16))
   (load-byte
    :accessor load-byte
    :initform (make-instance 'sl-signal-binary-lambda :bit-width 8))
   ;; - Memory write mask
   (store-wmask
    :accessor store-wmask
    :initform (make-instance 'sl-signal-binary-lambda :bit-width 4))
   ;; - The state machine
   (state
    :accessor state
    :initform (make-instance 'sl-signal-binary :bit-width 4))
   (write-back
    :accessor write-back
    :initform (make-instance 'sl-signal-binary-lambda :bit-width 1))
   (jump-to-pc-plus-imm
    :accessor jump-to-pc-plus-imm
    :initform (make-instance 'sl-signal-binary-lambda :bit-width 1))
   (need-to-wait
    :accessor need-to-wait
    :initform (make-instance 'sl-signal-binary-lambda :bit-width 1))
   ;; - Cycle counter
   (cycles
    :accessor cycles
    :initform (make-instance 'sl-signal-binary :bit-width 32))))

;;; Build phase
(defmethod build ((self femtorv32))
  ;; Create register file
  (loop for i from 0 to 31 do
    (setf (aref (-> self register-file) i)
	  (make-instance 'sl-signal-binary :bit-width 32))))

;;; Combinatorial logic
(defmethod connect ((self femtorv32))
  ;; wire rdId = instr[11:7]
  (connect-comb (-> self rdId)
		(lambda () (bits (-> self instr value) 11 7))
		(vector (-> self instr)))
  ;; funct3Is = 8'b00000001 << instr[14:12]
  (connect-comb (-> self funct3-is)
		(lambda () (lsh #u8b_00000001 (sl-uint-value (bits (-> self instr value) 14 12))))
		(vector (-> self instr)))
  ;;  Uimm = {    instr[31],   instr[30:12], {12{1'b0}}};
  (connect-comb (-> self Uimm)
		(lambda ()
		  (concat (bit (-> self instr value) 31)
			  (bits (-> self instr value) 30 12)
			  (uint 0 :bits 12)))
		(vector (-> self instr)))
  ;;  Iimm = {{21{instr[31]}}, instr[30:20]};
  (connect-comb (-> self Iimm)
		(lambda ()
		  (concat
		   (repeat-bit-uint (bit (-> self instr value) 31) 21)
		   (bits (-> self instr value) 30 20)))
		(vector (-> self instr)))
  ;; Simm = {{21{instr[31]}}, instr[30:25],instr[11:7]};
  (connect-comb (-> self Simm)
		(lambda ()
		  (concat
		   (repeat-bit-uint (bit (-> self instr value) 31) 21)
		   (bits (-> self instr value) 30 25)
		   (bits (-> self instr value) 11 7)))
		(vector (-> self instr)))
  ;; Bimm = {{20{instr[31]}}, instr[7],instr[30:25],instr[11:8],1'b0};
  (connect-comb (-> self Bimm)
		(lambda ()
		  (concat
		   (repeat-bit-uint (bit (-> self instr value) 31) 20)
		   (bit (-> self instr value) 7)
		   (bits (-> self instr value) 30 25)
		   (bits (-> self instr value) 11 8)
		   (uint 0 :bits 1)))
		(vector (-> self instr)))
  ;; Jimm = {{12{instr[31]}}, instr[19:12],instr[20],instr[30:21],1'b0};
  (connect-comb (-> self Jimm)
		(lambda ()
		  (concat
		   (repeat-bit-uint (bit (-> self instr value) 31) 12)
		   (bits (-> self instr value) 19 12)
		   (bit (-> self instr value) 20)
		   (bits (-> self instr value) 30 21)
		   (uint 0 :bits 1)))
		(vector (-> self instr)))
  ;; isLoad    =  (instr[6:2] == 5'b00000); // rd <- mem[rs1+Iimm]
  (connect-comb (-> self is-load)
		(lambda ()
		  (if (sl= (bits (-> self instr value) 6 2) #u5b_00000) 1 0))
		(vector (-> self instr)))
  ;; isALUimm  =  (instr[6:2] == 5'b00100); // rd <- rs1 OP Iimm
  (connect-comb (-> self is-alu-imm)
		(lambda ()
		  (if (sl= (bits (-> self instr value) 6 2) #u5b_00100) 1 0))
		(vector (-> self instr)))
  ;; isStore   =  (instr[6:2] == 5'b01000); // mem[rs1+Simm] <- rs2
  (connect-comb (-> self is-store)
		(lambda ()
		  (if (sl= (bits (-> self instr value) 6 2) #u5b_01000) 1 0))
		(vector (-> self instr)))
  ;; isALUreg  =  (instr[6:2] == 5'b01100); // rd <- rs1 OP rs2
  (connect-comb (-> self is-alu-reg)
		(lambda ()
		  (if (sl= (bits (-> self instr value) 6 2) #u5b_01100) 1 0))
		(vector (-> self instr)))
  ;; isSYSTEM  =  (instr[6:2] == 5'b11100); // rd <- cycles
  (connect-comb (-> self is-system)
		(lambda ()
		  (if (sl= (bits (-> self instr value) 6 2) #u5b_11100) 1 0))
		(vector (-> self instr)))
  ;; isJAL     =  instr[3]; // (instr[6:2] == 5'b11011); // rd <- PC+4; PC<-PC+Jimm
  (connect-comb (-> self is-jal)
		(lambda ()
		  (if (sl= (bits (-> self instr value) 6 2) #u5b_11011) 1 0))
		(vector (-> self instr)))
  ;; isJALR    =  (instr[6:2] == 5'b11001); // rd <- PC+4; PC<-rs1+Iimm
  (connect-comb (-> self is-jalr)
		(lambda ()
		  (if (sl= (bits (-> self instr value) 6 2) #u5b_11001) 1 0))
		(vector (-> self instr)))
  ;; isLUI     =  (instr[6:2] == 5'b01101); // rd <- Uimm
  (connect-comb (-> self is-lui)
		(lambda ()
		  (if (sl= (bits (-> self instr value) 6 2) #u5b_01101) 1 0))
		(vector (-> self instr)))
  ;; isAUIPC   =  (instr[6:2] == 5'b00101); // rd <- PC + Uimm
  (connect-comb (-> self is-auipc)
		(lambda ()
		  (if (sl= (bits (-> self instr value) 6 2) #u5b_00101) 1 0))
		(vector (-> self instr)))
  ;; isBranch  =  (instr[6:2] == 5'b11000); // if(rs1 OP rs2) PC<-PC+Bimm
  (connect-comb (-> self is-branch)
		(lambda ()
		  (if (sl= (bits (-> self instr value) 6 2) #u5b_11000) 1 0))
		(vector (-> self instr)))
  ;; isALU = isALUimm | isALUreg;
  (connect-comb (-> self is-alu)
		(lambda ()
		  (sl-logior (-> self is-alu-imm value)
			     (-> self is-alu-reg value)))
		(vector (-> self is-alu-imm)
			(-> self is-alu-reg)))
  ;; aluIn1 = rs1;
  (connect-comb (-> self alu-in1)
		(lambda () (-> self rs1 value))
	        (vector (-> self rs1)))
  ;; aluIn2 = isALUreg | isBranch ? rs2 : Iimm;
  (connect-comb (-> self alu-in2)
		(lambda ()
		  (if (zerop (-> (sl-logior (-> self is-alu-reg value) (-> self is-branch value)) sl-uint-value))
		      (-> self Iimm value)
		      ;; else
		      (-> self rs2 value)))
		(vector (-> self is-alu-reg)
			(-> self is-branch)
			(-> self rs2)
			(-> self Iimm)))
  ;; aluBusy = |aluShamt;
  (connect-comb (-> self alu-busy)
		(lambda ()
		  (or-reduce (-> self alu-shamt value)))
		(vector (-> self alu-shamt)))
  ;; aluPlus = aluIn1 + aluIn2;
  (connect-comb (-> self alu-plus)
		(lambda () (sl+ (-> self alu-in1 value)
				(-> self alu-in2 value)))
		(vector (-> self alu-in1) (-> self alu-in2)))
  ;; aluMinus = {1'b1, ~aluIn2} + {1'b0,aluIn1} + 33'b1;
  (connect-comb (-> self alu-minus)
		(lambda () (sl+ (concat #u1b_1 (sl-lognot (-> self alu-in2 value)))
				(concat #u1b_0 (-> self alu-in1 value))
				(uint 1 :bits 33)))
		(vector (-> self alu-in1) (-> self alu-in2)))
  ;; LT  = (aluIn1[31] ^ aluIn2[31]) ? aluIn1[31] : aluMinus[32];
  (connect-comb (-> self alu-lt)
		(lambda ()
		  (if (zerop (sl-logxor (bit (-> self alu-in1 value) 31)
					(bit (-> self alu-in2 value) 31)))
		      (bit (-> self alu-minus value) 32)
		      ;; else
		      (bit (-> self alu-in1 value) 31)))
		(vector (-> self alu-in1)
			(-> self alu-in2)
			(-> self alu-minus)))
  ;; LTU = aluMinus[32];
  (connect-comb (-> self alu-ltu)
		(lambda () (bit (-> self alu-minus value) 32))
		(vector (-> self alu-minus)))
  ;; EQ  = (aluMinus[31:0] == 0);
  (connect-comb (-> self alu-eq)
		(lambda () (if (sl= 0 (bits (-> self alu-minus value) 31 0)) 1 0))
		(vector (-> self alu-minus)))
  ;; alu-out =
  (connect-comb (-> self alu-out)
		(lambda ()
		  (sl-logior
		   ;; (funct3Is[0]  ? instr[30] & instr[5] ? aluMinus[31:0] : aluPlus : 32'b0)
		   (if (not (zerop (bit (-> self funct3-is value) 0)))
		       (if (not (zerop (sl-logand (bit (-> self instr value) 30)
						  (bit (-> self instr value) 5))))
			   (bits (-> self alu-minus value) 31 0)
			   ;; else
			   (-> self alu-plus value))
		       ;; else
		       0)
		   ;; (funct3Is[2]  ? {31'b0, LT}                                     : 32'b0)
		   (if (not (zerop (bit (-> self funct3-is value) 2)))
		       (concat (uint 0 :bits 31) (-> self alu-lt value))
		       ;; else
		       0)
		   ;; (funct3Is[3]  ? {31'b0, LTU}                                    : 32'b0)
		   (if (not (zerop (bit (-> self funct3-is value) 3)))
		       (concat (uint 0 :bits 31) (-> self alu-ltu value))
		       ;; else
		       0)
		   ;; (funct3Is[4]  ? aluIn1 ^ aluIn2                                 : 32'b0)
		   (if (not (zerop (bit (-> self funct3-is value) 4)))
		       (sl-logxor (-> self alu-in1 value)
				  (-> self alu-in2 value))
		       ;; else
		       0)
		   ;; (funct3Is[6]  ? aluIn1 | aluIn2                                 : 32'b0)
		   (if (not (zerop (bit (-> self funct3-is value) 6)))
		       (sl-logior (-> self alu-in1 value)
				  (-> self alu-in2 value))
		       ;; else
		       0)
		   ;; (funct3Is[7]  ? aluIn1 & aluIn2                                 : 32'b0)
		   (if (not (zerop (bit (-> self funct3-is value) 7)))
		       (sl-logand (-> self alu-in1 value)
				  (-> self alu-in2 value))
		       ;; else
		       0)
		   ;; (funct3IsShift ? aluReg                                         : 32'b0)
		   (if (not (sl= 0 (-> self funct3-is-shift value)))
		       (-> self alu-reg value)
		       ;; else
		       0)))
		(vector (-> self funct3-is)
			(-> self instr)
			(-> self alu-minus)
			(-> self alu-plus)
			(-> self alu-lt)
			(-> self alu-ltu)
			(-> self alu-in1)
			(-> self alu-in2)
			(-> self funct3-is-shift)
			(-> self alu-reg)))
  ;; funct3IsShift = funct3Is[1] | funct3Is[5];
  (connect-comb (-> self funct3-is-shift)
		(lambda ()
		  (sl-logior (bit (-> self funct3-is value) 1)
			     (bit (-> self funct3-is value) 5)))
		(vector (-> self funct3-is)))
  ;; predicate =
  (connect-comb (-> self predicate)
		(lambda ()
		  ;; |
		  (sl-logior
                   ;; funct3Is[0] &  EQ  | // BEQ
		   (sl-logand (bit (-> self funct3-is value) 0) (-> self alu-eq value))
		   ;; funct3Is[1] & !EQ  | // BNE
		   (sl-logand (bit (-> self funct3-is value) 1) (sl-lognot (-> self alu-eq value)))
		   ;; funct3Is[4] &  LT  | // BLT
		   (sl-logand (bit (-> self funct3-is value) 4) (-> self alu-lt value))
		   ;; funct3Is[5] & !LT  | // BGE
		   (sl-logand (bit (-> self funct3-is value) 5) (sl-lognot (-> self alu-lt value)))
		   ;; funct3Is[6] &  LTU | // BLTU
		   (sl-logand (bit (-> self funct3-is value) 6) (-> self alu-ltu value))
		   ;; funct3Is[7] & !LTU ; // BGEU
		   (sl-logand (bit (-> self funct3-is value) 7) (sl-lognot (-> self alu-ltu value)))))
		(vector (-> self funct3-is)
			(-> self alu-eq)
			(-> self alu-lt)
			(-> self alu-ltu)))
  ;; PCplus4 = PC + 4;
  (connect-comb (-> self pc-plus4)
		(lambda () (sl+ 4 (-> self pc)))
		(vector (-> self pc)))
  ;; PCplusImm = 
  (connect-comb (-> self pc-plus-imm)
		(lambda ()
		  (sl+ (-> self pc)
		       (if (sl= 1 (bit (-> self instr value) 3))
			   (bits (-> self Jimm value) 23 0)
			   ;; else
			   (if (sl= 1 (bit (-> self instr value) 4))
			       (bits (-> self Uimm value) 23 0)
			       ;; else
			       (bits (-> self Bimm value) 23 0)))))
		(vector (-> self pc)
			(-> self instr)
			(-> self Jimm)
			(-> self Uimm)
			(-> self Bimm)))
  ;; loadstore_addr =
  (connect-comb (-> self loadstore-addr)
		(lambda ()
		  (sl+ (bits (-> self rs1 value) 23 0)
		       (if (sl= 1 (bit (-> self instr value) 5))
			   (bits (-> self Simm value) 23 0)
			   ;; else
			   (bits (-> self Iimm value) 23 0))))
		(vector (-> self rs1)
			(-> self Simm)
			(-> self Iimm)))
  ;; mem_addr = state[WAIT_INSTR_bit] | state[FETCH_INSTR_bit] ? PC : loadstore_addr ;
  (connect-comb (-> self mem-addr-lambda)
		(lambda ()
		  (if (sl= 1 (sl-logior (bit (-> self state value) 1)
					(bit (-> self state value) 0)))
		      (-> self pc value)
		      ;; else
		      (-> self loadstore-addr value)))
		(vector (-> self state)
			(-> self pc)
			(-> self loadstore-addr)))
  (connect-driver-load (-> self mem-addr-lambda) (-> self mem-addr))
  ;; writeBackData  =
  (connect-comb (-> self write-back-data)
		(lambda ()
		  (sl-logior
		   ;;  (isSYSTEM            ? cycles     : 32'b0) |  // SYSTEM
		   (if (sl= 1 (-> self is-system))
		       (-> self cycles value)
		       ;;else
		       0)
		   ;; (isLUI               ? Uimm       : 32'b0) |  // LUI
		   (if (sl= 1 (-> self is-lui))
		       (-> self Uimm value)
		       ;; else
		       0)
		   ;; (isALU               ? aluOut     : 32'b0) |  // ALUreg, ALUimm
		   (if (sl= 1 (-> self is-alu))
		       (-> self alu-out)
		       ;; else
		       0)
		   ;; (isAUIPC             ? PCplusImm  : 32'b0) |  // AUIPC
		   (if (sl= 1 (-> self is-auipc))
		       (-> self pc-plus-imm)
		       ;; else
		       0)
		   ;; (isJALR   | isJAL    ? PCplus4    : 32'b0) |  // JAL, JALR
		   (if (sl= 1 (sl-logior (-> self is-jalr value)
					 (-> self is-jal value)))
		       (-> self pc-plus4)
		       ;;else
		       0)
		   ;; (isLoad              ? LOAD_data  : 32'b0) ;  // Load
		   (if (sl= 1 (-> self is-load))
		       (-> self load-data)
		       ;; else
		       0)))
		(vector (-> self is-system)
			(-> self is-lui)
			(-> self Uimm)
			(-> self is-alu)
			(-> self alu-out)
			(-> self is-auipc)
			(-> self pc-plus-imm)
			(-> self is-jal)
			(-> self is-jalr)
			(-> self pc-plus4)
			(-> self is-load)
			(-> self load-data)))
  ;; mem_byteAccess     = instr[13:12] == 2'b00; // funct3[1:0] == 2'b00;
  (connect-comb (-> self mem-byte-access)
		(lambda ()
		  (if (sl= #u2b_00 (bits (-> self instr value) 13 12)) 1 0))
		(vector (-> self instr)))
  ;; mem_halfwordAccess = instr[13:12] == 2'b01; // funct3[1:0] == 2'b01;
  (connect-comb (-> self mem-halfword-access)
		(lambda ()
		  (if (sl= #u2b_01 (bits (-> self instr value) 13 12)) 1 0))
		(vector (-> self instr)))
  ;; LOAD_sign = !instr[14] & (mem_byteAccess ? LOAD_byte[7] : LOAD_halfword[15]);
  (connect-comb (-> self load-sign)
		(lambda ()
		  (sl-logand
		   (sl-lognot (bit (-> self instr value) 14))
		   (if (sl= 1 (-> self mem-byte-access))
		       (bit (-> self load-byte value) 7)
		       ;; else
		       (bit (-> self load-halfword value) 15))))
		(vector (-> self instr)
			(-> self mem-byte-access)
			(-> self load-byte)
			(-> self load-halfword)))
  ;; LOAD_data =
  (connect-comb (-> self load-data)
		(lambda ()
		  ;; mem_byteAccess ? {{24{LOAD_sign}},     LOAD_byte} :
		  (if (sl= 1 (-> self mem-byte-access value))
		      (concat (repeat-bit-uint (-> self load-sign value sl-uint-value) 24)
			      (-> self load-byte value))
		      ;; else
		      ;; mem_halfwordAccess ? {{16{LOAD_sign}}, LOAD_halfword} :
		      (if (sl= 1 (-> self mem-halfword-access))
			  (concat (repeat-bit-uint (-> self load-sign value sl-uint-value) 16)
				  (-> self load-halfword value))
			  ;; else
			  ;; mem_rdata ;
			  (-> self mem-rdata value))))
		(vector (-> self mem-byte-access)
			(-> self load-sign)
			(-> self load-byte)
			(-> self mem-halfword-access)
			(-> self load-halfword)
			(-> self mem-rdata)))
  ;; LOAD_halfword = loadstore_addr[1] ? mem_rdata[31:16] : mem_rdata[15:0];
  (connect-comb (-> self load-halfword)
		(lambda ()
		  (if (sl= 1 (bit (-> self loadstore-addr value) 1))
		      (bits (-> self mem-rdata value) 31 16)
		      ;; else
		      (bits (-> self mem-rdata value) 15 0)))
		(vector (-> self loadstore-addr)
			(-> self mem-rdata)))
  ;; LOAD_byte = loadstore_addr[0] ? LOAD_halfword[15:8] : LOAD_halfword[7:0];
  (connect-comb (-> self load-byte)
		(lambda ()
		  (if (sl= 1 (bit (-> self loadstore-addr value) 0))
		      (bits (-> self load-halfword value) 15 8)
		      ;; else
		      (bits (-> self load-halfword value) 7 0)))
		(vector (-> self loadstore-addr)
			(-> self load-halfword)))
  ;; assign mem_wdata[ 7: 0] = rs2[7:0];
  (connect-comb (slice (-> self mem-wdata) 7 0)
		(lambda () (bits (-> self rs2 value) 7 0))
		(vector (-> self rs2)))
  ;; assign mem_wdata[15: 8] = loadstore_addr[0] ? rs2[7:0]  : rs2[15: 8];
  (connect-comb (slice (-> self mem-wdata) 15 8)
		(lambda ()
		  (if (sl= 1 (bit (-> self loadstore-addr value) 0))
		      (bits (-> self rs2 value) 7 0)
		      ;; else
		      (bits (-> self rs2 value) 15 8)))
		(vector (-> self rs2)
			(-> self loadstore-addr)))
  ;; assign mem_wdata[23:16] = loadstore_addr[1] ? rs2[7:0]  : rs2[23:16];
  (connect-comb (slice (-> self mem-wdata) 23 16)
		(lambda ()
		  (if (sl= 1 (bit (-> self loadstore-addr value) 1))
		      (bits (-> self rs2 value) 7 0)
		      ;; else
		      (bits (-> self rs2 value) 23 16)))
		(vector (-> self rs2)
			(-> self loadstore-addr)))
  ;;  assign mem_wdata[31:24] = loadstore_addr[0] ? rs2[7:0]  : loadstore_addr[1] ? rs2[15:8] : rs2[31:24];
  (connect-comb (slice (-> self mem-wdata) 31 24)
		(lambda ()
		  (if (sl= 1 (bit (-> self loadstore-addr value) 0))
		      (bits (-> self rs2 value) 7 0)
		      ;; else
		      (if (sl= 1 (bit (-> self loadstore-addr value) 1))
			  (bits (-> self rs2 value) 15 8)
			  ;; else
			  (bits (-> self rs2 value) 31 24))))
		(vector (-> self rs2)
			(-> self loadstore-addr)))
  ;; STORE_wmask =
  (connect-comb (-> self store-wmask)
		(lambda ()
		  (if (sl= 1 (-> self mem-byte-access value))
		      (if (sl= 1 (bit (-> self loadstore-addr value) 1))
			  (if (sl= 1 (bit (-> self loadstore-addr value) 0))
			      #u4b_1000
			      #u4b_0100)
			  ;; else
			  (if (sl= 1 (bit (-> self loadstore-addr value) 0))
			      #u4b_0010
			      #u4b_0001))
		      ;; else
		      (if (sl= 1 (-> self mem-halfword-access value))
			  (if (sl= 1 (bit (-> self loadstore-addr value) 0))
			      #u4b_1100
			      #u4b_0011)
			  ;; else
			  #u4b_1111)))
		(vector (-> self mem-byte-access)
			(-> self loadstore-addr)
			(-> self mem-halfword-access)))
  ;; writeBack = ~(isBranch | isStore ) & (state[EXECUTE_bit] | state[WAIT_ALU_OR_MEM_bit]);
  (connect-comb (-> self write-back)
		(lambda ()
		  (sl-logand
		   (sl-lognot (sl-logior (-> self is-branch value)
					 (-> self is-store value)))
		   (sl-logior (bit (-> self state value) 2)
			      (bit (-> self state value) 3))))
		(vector (-> self is-branch)
			(-> self is-store)
			(-> self state)))
  ;; assign mem_rstrb = state[EXECUTE_bit] & isLoad | state[FETCH_INSTR_bit];
  (connect-comb (-> self mem-rstrb-lambda)
		(lambda ()
		  (sl-logior
		   (sl-logand (bit (-> self state value) 2)
			      (-> self is-load value))
		   (bit (-> self state value) 0)))
		(vector (-> self state)
			(-> self is-load)))
  (connect-driver-load (-> self mem-rstrb-lambda) (-> self mem-rstrb))
  ;;  assign mem_wmask = {4{state[EXECUTE_bit] & isStore}} & STORE_wmask;
  (connect-comb (-> self mem-wmask-lambda)
		(lambda ()
		  (sl-logand (-> self store-wmask value)
			     (repeat-bit-uint
			      (sl-logand (-> self is-store value)
					 (bit (-> self state value) 2))
			      4)))
		(vector (-> self state)
			(-> self is-store)
			(-> self store-wmask)))
  (connect-driver-load (-> self mem-wmask-lambda) (-> self mem-wmask))
  ;;assign aluWr = state[EXECUTE_bit] & isALU;
  (connect-comb (-> self alu-wr)
		(lambda ()
		  (sl-logand (-> self is-alu value)
			     (bit (-> self state value) 2)))
		(vector (-> self state)
			(-> self is-alu)))
  ;; jumpToPCplusImm = isJAL | (isBranch & predicate);
  (connect-comb (-> self jump-to-pc-plus-imm)
		(lambda ()
		  (sl-logior (-> self is-jal value)
			     (sl-logand (-> self is-branch value)
					(-> self predicate value))))
		(vector (-> self is-jal)
			(-> self is-branch)
			(-> self predicate)))
  ;; needToWait = isLoad | isStore | isALU & funct3IsShift;
  (connect-comb (-> self need-to-wait)
		(lambda ()
		  (sl-logior
		   (-> self is-load value)
		   (-> self is-store value)
		   (sl-logand (-> self is-alu value)
			      (-> self funct3-is-shift value))))
		(vector (-> self is-load)
			(-> self is-store)
			(-> self is-alu)
			(-> self funct3-is-shift))))

;;; Sequential logic
(defmethod-res regfile-logic ((self femtorv32))
  (forever
    (sim-wait (posedge (-> self clk)))
    (when (sl= (-> self write-back value) 1)
      (when (not (sl= (-> self rdId) 0))
	(setnb (aref (-> self register-file) (-> self rdId value sl-uint-value))
	       (-> self write-back-data value))))))

(defmethod-res shift-logic ((self femtorv32))
    (forever
      (sim-wait (posedge (-> self clk)))
      
      (when (sl= (-> self alu-wr) 1)
	(when (sl= (-> self funct3-is-shift) 1)
	  (progn
	    (setnb (-> self alu-reg) (-> self alu-in1 value))
	    (setnb (-> self alu-shamt) (bits (-> self alu-in2 value) 4 0)))))

      (when (sl= 1 (or-reduce (-> self alu-shamt value)))
	(progn
	  (setnb (-> self alu-shamt) (sl1- (-> self alu-shamt value)))
	  (setnb (-> self alu-reg)
		 (if (sl= 1 (bit (-> self funct3-is value) 1))
		     (lsh (-> self alu-reg value) 1)
		     ;; else
		     (concat (sl-logand (bit (-> self instr value) 30)
					(bit (-> self alu-reg value) 31))
			     (bits (-> self alu-reg value) 31 1))))))))

(defmethod-res fsm-logic ((self femtorv32))
  (forever
    (sim-wait (posedge (-> self clk)))
    (let ((is-default t))
      (if (zerop (-> self reset value sl-uint-value))
	  (progn
	    (sl-message 'none "reset active")
	    (setnb (-> self state) 8)
	    (setnb (-> self pc) 0))
	  ;; else
	  (progn
	    ;; state[WAIT_INSTR_bit] == 1
	    (when (sl= (-> self state value) 2)
	      (sl-message 'none "state = WAIT_INSTR")
	      (when (zerop (-> self mem-rbusy value sl-uint-value))
		(setnb (-> self rs1) (aref (-> self register-file)
					   (-> (bits (-> self mem-rdata value) 19 15) sl-uint-value)))
		(setnb (-> self rs2) (aref (-> self register-file)
					   (-> (bits (-> self mem-rdata value) 24 20) sl-uint-value)))
		(setnb (-> self instr) (-> self mem-rdata value))
		(setnb (-> self state) 4) ;; state <= EXECUTE; 
		(setf is-default nil)))
	    ;; state[EXECUTE_bit] == 1
	    (when (sl= (-> self state value) 4)
	      (sl-message 'none "state = EXECUTE")
	      (setnb (-> self pc) (if (sl= 1 (-> self is-jalr value))
				      (concat (bits (-> self alu-plus value) 23 1) #u1b_0)
				      ;; else
				      (if (sl= 1 (-> self jump-to-pc-plus-imm value))
					  (-> self pc-plus-imm)
					  ;; else
					  (-> self pc-plus4))))
	      (setnb (-> self state) (if (sl= 1 (-> self need-to-wait value))
					 (ash 1 3)
					 ;;else
					 1))
	      (setf is-default nil))
	    ;; state[WAIT_ALU_OR_MEM_bit] == 1
	    (when (sl= (-> self state value) 8)
	      (sl-message 'none "state = WAIT_ALU_OR_MEM")
	      (when (sl= 1 (sl-logand (sl-lognot (-> self alu-busy value))
				      (sl-lognot (-> self mem-rbusy value))
				      (sl-lognot (-> self mem-wbusy value))))
		(setnb (-> self state) 1))
	      (setf is-default nil))
	    ;; default // FETCH_INSTR
	    (when is-default
	      (setnb (-> self state) 2)))))))

(defmethod-res cycle-counter-logic ((self femtorv32))
  (forever
    (sim-wait (posedge (-> self clk)))
    (setnb (-> self cycles) (sl1+ (-> self cycles value)))))

(defmethod run ((self femtorv32))
  (spawn #'regfile-logic self)
  (spawn #'shift-logic self)
  (spawn #'fsm-logic self)
  (spawn #'cycle-counter-logic self))


