(in-package :sl-examples)

;;; Encoding tests
(fiveam:def-suite* asm-encoding-tests)

;;; LUI rd <- Uimm
(fiveam:test encode-lui
  (let* ((state (make-instance 'rv32i-asm-state))
	 (encoding nil))
    (reset-asm-symtab state)
    ;; LUI encoding
    (encode-instr state '(lui x5 #u20x_12345))
    (setf encoding (aref (prog-mem state) 0))
    ;; Checks
    (fiveam:is (sl= (bits encoding 6 0) #b0110111)) ;; check opcode
    (fiveam:is (sl= (bits encoding 11 7) 5)) ;; check rd
    (fiveam:is (sl= (bits encoding 31 12) #x12345)))) ;; check imm


;;; AUIPC rd <- PC + Uimm
(fiveam:test encode-auipc
  (let* ((state (make-instance 'rv32i-asm-state))
	 (encoding nil))
    (reset-asm-symtab state)
    ;; AUIPC encoding
    (encode-instr state '(auipc x13 #x34567))
    (setf encoding (aref (prog-mem state) 0))
    ;; Checks
    (fiveam:is (sl= (bits encoding 6 0) #b0010111)) ;; check opcode
    (fiveam:is (sl= (bits encoding 11 7) 13)) ;; check rd
    (fiveam:is (sl= (bits encoding 31 12) #x34567)))) ;; check imm

;;; JAL rd <- PC+4; PC<-PC+Jimm
;;; imm[20|10:1|11|19:12] rd opcode
(fiveam:test encode-jal
  (let* ((state (make-instance 'rv32i-asm-state))
	 (encoding nil))
    (reset-asm-symtab state)
    ;; JAL encoding
    (encode-instr state '(jal x3 #u21b_1_10000001_0_1111111111_0))
    (setf encoding (aref (prog-mem state) 0))
    ;; Checks
    (fiveam:is (sl= 1 (bit encoding 31)))
    (fiveam:is (sl= #b1111111111 (bits encoding 30 21)))
    (fiveam:is (sl= 0 (bit encoding 20)))
    (fiveam:is (sl= #b10000001 (bits encoding 19 12)))

    ;; JAL encoding 2
    (setf state (make-instance 'rv32i-asm-state))
    (reset-asm-symtab state)
    (encode-instr state '(jal x3 #u21b_1_10010001_1_1111011111_0))
    (setf encoding (aref (prog-mem state) 0))
    ;; Checks
    (fiveam:is (sl= 1 (bit encoding 31)))
    (fiveam:is (sl= #b1111011111 (bits encoding 30 21)))
    (fiveam:is (sl= 1 (bit encoding 20)))
    (fiveam:is (sl= #b10010001 (bits encoding 19 12)))

    (fiveam:is (sl= #b1101111 (bits encoding 6 0))) ;; check opcode
    (fiveam:is (sl= 3 (bits encoding 11 7))))) ;; check rd

;;; JALR rd <- PC+4; PC<-rs1+Iimm
(fiveam:test encode-jalr
  (let* ((state (make-instance 'rv32i-asm-state))
	 (encoding nil))
    (reset-asm-symtab state)
    ;; JALR encoding
    (encode-instr state '(jalr x13 x8 #b1000))
    (setf encoding (aref (prog-mem state) 0))
    ;; Checks
    (fiveam:is (sl= #b1100111 (bits encoding 6 0))) ;; check opcode
    (fiveam:is (sl= 13 (bits encoding 11 7))) ;; check rd
    (fiveam:is (sl= #b1000 (bits encoding 31 20))))) ;; check imm

;;; BEQ if(rs1 == rs2) PC<-PC+Bimm
;;; imm[12|10:5] rs2 rs1 funct3 imm[4:1|11] opcode
(fiveam:test encode-beq
  (let* ((state (make-instance 'rv32i-asm-state))
	 (encoding nil))
    (reset-asm-symtab state)
    ;; BEQ encoding
    (encode-instr state '(beq x7 x9 #u13b_1_0_100001_1111_0))
    (setf encoding (aref (prog-mem state) 0))
    ;; Checks
    (fiveam:is (sl= #b1100011 (bits encoding 6 0)))  ;; check opcode
    (fiveam:is (sl= 7 (bits encoding 19 15)))	     ;; check rs1
    (fiveam:is (sl= 9 (bits encoding 24 20)))        ;; check rs2
    (fiveam:is (sl= 0 (bits encoding 14 12)))        ;; check funct3
    (fiveam:is (sl= 1 (bit encoding 31)))            ;; check imm[12]   
    (fiveam:is (sl= #b100001 (bits encoding 30 25))) ;; check imm[10:5]
    (fiveam:is (sl= #b1111 (bits encoding 11 8)))    ;; check imm[4:1]
    (fiveam:is (sl= 0 (bit encoding 7)))             ;; check imm[11]
    ))

;;; BNE if(rs1 != rs2) PC<-PC+Bimm
(fiveam:test encode-bne
  (let* ((state (make-instance 'rv32i-asm-state))
	 (encoding nil))
    (reset-asm-symtab state)
    ;; Bne encoding
    (encode-instr state '(bne x7 x9 #u13b_1_0_100001_1111_0))
    (setf encoding (aref (prog-mem state) 0))
    ;; Checks
    (fiveam:is (sl= #b1100011 (bits encoding 6 0)))  ;; check opcode
    (fiveam:is (sl= 7 (bits encoding 19 15)))	     ;; check rs1
    (fiveam:is (sl= 9 (bits encoding 24 20)))        ;; check rs2
    (fiveam:is (sl= 1 (bits encoding 14 12)))        ;; check funct3
    (fiveam:is (sl= 1 (bit encoding 31)))            ;; check imm[12]   
    (fiveam:is (sl= #b100001 (bits encoding 30 25))) ;; check imm[10:5]
    (fiveam:is (sl= #b1111 (bits encoding 11 8)))    ;; check imm[4:1]
    (fiveam:is (sl= 0 (bit encoding 7)))             ;; check imm[11]
    ))

;;; BLT if(rs1 < rs2) PC<-PC+Bimm
(fiveam:test encode-blt
  (let* ((state (make-instance 'rv32i-asm-state))
	 (encoding nil))
    (reset-asm-symtab state)
    ;; BLT encoding
    (encode-instr state '(blt x7 x9 #u13b_1_0_100101_1111_0))
    (setf encoding (aref (prog-mem state) 0))
    ;; Checks
    (fiveam:is (sl= #b1100011 (bits encoding 6 0)))  ;; check opcode
    (fiveam:is (sl= 7 (bits encoding 19 15)))	     ;; check rs1
    (fiveam:is (sl= 9 (bits encoding 24 20)))        ;; check rs2
    (fiveam:is (sl= 4 (bits encoding 14 12)))        ;; check funct3
    (fiveam:is (sl= 1 (bit encoding 31)))            ;; check imm[12]   
    (fiveam:is (sl= #b100101 (bits encoding 30 25))) ;; check imm[10:5]
    (fiveam:is (sl= #b1111 (bits encoding 11 8)))    ;; check imm[4:1]
    (fiveam:is (sl= 0 (bit encoding 7)))             ;; check imm[11]
    ))

;;; BGE if(rs1 >= rs2) PC<-PC+Bimm
(fiveam:test encode-bge
  (let* ((state (make-instance 'rv32i-asm-state))
	 (encoding nil))
    (reset-asm-symtab state)
    ;; BGE encoding
    (encode-instr state '(bge x7 x9 #u13b_1_0_100101_1111_0))
    (setf encoding (aref (prog-mem state) 0))
    ;; Checks
    (fiveam:is (sl= #b1100011 (bits encoding 6 0)))  ;; check opcode
    (fiveam:is (sl= 7 (bits encoding 19 15)))	     ;; check rs1
    (fiveam:is (sl= 9 (bits encoding 24 20)))        ;; check rs2
    (fiveam:is (sl= 5 (bits encoding 14 12)))        ;; check funct3
    (fiveam:is (sl= 1 (bit encoding 31)))            ;; check imm[12]   
    (fiveam:is (sl= #b100101 (bits encoding 30 25))) ;; check imm[10:5]
    (fiveam:is (sl= #b1111 (bits encoding 11 8)))    ;; check imm[4:1]
    (fiveam:is (sl= 0 (bit encoding 7)))             ;; check imm[11]
    ))

;;; BLTU if($unsigned(rs1) < $unsigned(rs2)) PC<-PC+Bimm
(fiveam:test encode-bltu
  (let* ((state (make-instance 'rv32i-asm-state))
	 (encoding nil))
    (reset-asm-symtab state)
    ;; BLTU encoding
    (encode-instr state '(bltu x7 x9 #u13b_1_0_100101_1111_0))
    (setf encoding (aref (prog-mem state) 0))
    ;; Checks
    (fiveam:is (sl= #b1100011 (bits encoding 6 0)))  ;; check opcode
    (fiveam:is (sl= 7 (bits encoding 19 15)))	     ;; check rs1
    (fiveam:is (sl= 9 (bits encoding 24 20)))        ;; check rs2
    (fiveam:is (sl= 6 (bits encoding 14 12)))        ;; check funct3
    (fiveam:is (sl= 1 (bit encoding 31)))            ;; check imm[12]   
    (fiveam:is (sl= #b100101 (bits encoding 30 25))) ;; check imm[10:5]
    (fiveam:is (sl= #b1111 (bits encoding 11 8)))    ;; check imm[4:1]
    (fiveam:is (sl= 0 (bit encoding 7)))             ;; check imm[11]
    ))

;;; BGEU if($unsigned(rs1) >= $unsigned(rs2)) PC<-PC+Bimm
(fiveam:test encode-bgeu
  (let* ((state (make-instance 'rv32i-asm-state))
	 (encoding nil))
    (reset-asm-symtab state)
    ;; BGEU encoding
    (encode-instr state '(bgeu x7 x9 #u13b_1_0_100101_1111_0))
    (setf encoding (aref (prog-mem state) 0))
    ;; Checks
    (fiveam:is (sl= #b1100011 (bits encoding 6 0)))  ;; check opcode
    (fiveam:is (sl= 7 (bits encoding 19 15)))	     ;; check rs1
    (fiveam:is (sl= 9 (bits encoding 24 20)))        ;; check rs2
    (fiveam:is (sl= 7 (bits encoding 14 12)))        ;; check funct3
    (fiveam:is (sl= 1 (bit encoding 31)))            ;; check imm[12]   
    (fiveam:is (sl= #b100101 (bits encoding 30 25))) ;; check imm[10:5]
    (fiveam:is (sl= #b1111 (bits encoding 11 8)))    ;; check imm[4:1]
    (fiveam:is (sl= 0 (bit encoding 7)))             ;; check imm[11]
    ))

;;; rd <- mem[rs1+Iimm]
;;; LB
(fiveam:test encode-lb
  (let* ((state (make-instance 'rv32i-asm-state))
	 (encoding nil))
    (reset-asm-symtab state)
    ;; LB encoding
    (encode-instr state '(lb x13 x8 #b1000))
    (setf encoding (aref (prog-mem state) 0))
    ;; Checks
    (fiveam:is (sl= #b0000011 (bits encoding 6 0))) ;; check opcode
    (fiveam:is (sl= 13 (bits encoding 11 7))) ;; check rd
    (fiveam:is (sl= 0 (bits encoding 14 12))) ;; check funct3
    (fiveam:is (sl= #b1000 (bits encoding 31 20)))))
;;; LH 
;;; LW 
;;; LBU 
;;; LHU 

;;; mem[rs1+Simm] <- rs2
;;; SB
(fiveam:test encode-sb
  (let* ((state (make-instance 'rv32i-asm-state))
	 (encoding nil))
    (reset-asm-symtab state)
    ;; SB encoding
    (encode-instr state '(sb x13 #u12b_1000001_11111 x8))
    (setf encoding (aref (prog-mem state) 0))
    ;; Checks
    (fiveam:is (sl= #b0100011 (bits encoding 6 0))) ;; check opcode
    (fiveam:is (sl= 13 (bits encoding 24 20))) ;; check rs2
    (fiveam:is (sl= 8 (bits encoding 19 15))) ;; check rs1
    (fiveam:is (sl= 0 (bits encoding 14 12))) ;; check funct3
    (fiveam:is (sl= #b1000001 (bits encoding 31 25)))
    (fiveam:is (sl= #b11111 (bits encoding 11 7)))))
;;; SH
;;; SW

;;; rd <- rs1 OP imm
;;; ADDI
(fiveam:test encode-addi
  (let* ((state (make-instance 'rv32i-asm-state))
	 (encoding nil))
    (reset-asm-symtab state)
    ;; ADDI encoding
    (encode-instr state '(addi x13 x8 #b1000))
    (setf encoding (aref (prog-mem state) 0))
    ;; Checks
    (fiveam:is (sl= #b0010011 (bits encoding 6 0))) ;; check opcode
    (fiveam:is (sl= 13 (bits encoding 11 7))) ;; check rd
    (fiveam:is (sl= #b1000 (bits encoding 31 20)))
    (fiveam:is (sl= 8 (bits encoding 19 15)))))
;;; SLTI
;;; SLTIU
;;; XORI
;;; ORI
;;; ANDI

;;; rd = rs1 << shamt
;;; SLLI
(fiveam:test encode-slli
  (let* ((state (make-instance 'rv32i-asm-state))
	 (encoding nil))
    (reset-asm-symtab state)
    ;; SLLI encoding
    (encode-instr state '(slli x13 x4 10))
    (setf encoding (aref (prog-mem state) 0))
    ;; Checks
    (fiveam:is (sl= #b0010011 (bits encoding 6 0))) ;; check opcode
    (fiveam:is (sl= 0 (bits encoding 31 25))) ;; check funct7
    (fiveam:is (sl= 10 (bits encoding 24 20))) ;; check shamt
    (fiveam:is (sl= 13 (bits encoding 11 7))) ;; check rd
    (fiveam:is (sl= 4 (bits encoding 19 15))) ;; check rs1
    (fiveam:is (sl= #b001 (bits encoding 14 12))) ;; check funct3
    ))
;;; SRLI
;;; SRAI

;;; rd <- rs1 OP rs2
;;; ADD
(fiveam:test encode-add
  (let* ((state (make-instance 'rv32i-asm-state))
	 (encoding nil))
    (reset-asm-symtab state)
    ;; ADD encoding
    (encode-instr state '(add x13 x4 x10))
    (setf encoding (aref (prog-mem state) 0))
    ;; Checks
    (fiveam:is (sl= #b0110011 (bits encoding 6 0))) ;; check opcode
    (fiveam:is (sl= 0 (bits encoding 31 25))) ;; check funct7
    (fiveam:is (sl= 13 (bits encoding 11 7))) ;; check rd
    (fiveam:is (sl= 4 (bits encoding 19 15))) ;; check rs1
    (fiveam:is (sl= 10 (bits encoding 24 20))) ;; check rs2
    (fiveam:is (sl= #b000 (bits encoding 14 12))) ;; check funct3
    ))
;;; SUB
(fiveam:test encode-sub
  (let* ((state (make-instance 'rv32i-asm-state))
	 (encoding nil))
    (reset-asm-symtab state)
    ;; SUB encoding
    (encode-instr state '(sub x13 x4 x10))
    (setf encoding (aref (prog-mem state) 0))
    ;; Checks
    (fiveam:is (sl= #b0110011 (bits encoding 6 0))) ;; check opcode
    (fiveam:is (sl= #b0100000 (bits encoding 31 25))) ;; check funct7
    (fiveam:is (sl= 13 (bits encoding 11 7))) ;; check rd
    (fiveam:is (sl= 4 (bits encoding 19 15))) ;; check rs1
    (fiveam:is (sl= 10 (bits encoding 24 20))) ;; check rs2
    (fiveam:is (sl= #b000 (bits encoding 14 12))) ;; check funct3
    ))
;;; SLL
;;; SLT
;;; SLTU
;;; XOR
;;; SRL
;;; SRA
;;; OR
;;; AND

;;; FENCE
;;; ECALL
;;; EBREAK

;;; ============================================================
;;; Pseudoinstruction tests
;;; ============================================================

;;; NOP => addi x0, x0, 0
(fiveam:test encode-nop
  (let* ((state (make-instance 'rv32i-asm-state))
	 (encoding nil))
    (reset-asm-symtab state)
    (encode-instr state '(nop))
    (setf encoding (aref (prog-mem state) 0))
    ;; Should be addi x0, 0, x0
    (fiveam:is (sl= #b0010011 (bits encoding 6 0)))  ;; addi opcode
    (fiveam:is (sl= 0 (bits encoding 11 7)))          ;; rd = x0
    (fiveam:is (sl= 0 (bits encoding 14 12)))          ;; funct3 = 000
    (fiveam:is (sl= 0 (bits encoding 19 15)))          ;; rs1 = x0
    (fiveam:is (sl= 0 (bits encoding 31 20)))))        ;; imm = 0

;;; MV rd, rs => addi rd, 0, rs
(fiveam:test encode-mv
  (let* ((state (make-instance 'rv32i-asm-state))
	 (encoding nil))
    (reset-asm-symtab state)
    (encode-instr state '(mv x5 x10))
    (setf encoding (aref (prog-mem state) 0))
    ;; Should be addi x5, 0, x10
    (fiveam:is (sl= #b0010011 (bits encoding 6 0)))  ;; addi opcode
    (fiveam:is (sl= 5 (bits encoding 11 7)))          ;; rd = x5
    (fiveam:is (sl= 0 (bits encoding 14 12)))          ;; funct3 = 000
    (fiveam:is (sl= 10 (bits encoding 19 15)))         ;; rs1 = x10
    (fiveam:is (sl= 0 (bits encoding 31 20)))))        ;; imm = 0

;;; LI rd, small_imm => addi rd, imm, x0
(fiveam:test encode-li-small
  (let* ((state (make-instance 'rv32i-asm-state))
	 (encoding nil))
    (reset-asm-symtab state)
    ;; Positive small immediate
    (encode-instr state '(li x7 42))
    (setf encoding (aref (prog-mem state) 0))
    (fiveam:is (sl= #b0010011 (bits encoding 6 0)))  ;; addi opcode
    (fiveam:is (sl= 7 (bits encoding 11 7)))          ;; rd = x7
    (fiveam:is (sl= 0 (bits encoding 19 15)))          ;; rs1 = x0
    (fiveam:is (sl= 42 (bits encoding 31 20)))))       ;; imm = 42

;;; LI rd, small_negative => addi rd, imm, x0
(fiveam:test encode-li-small-negative
  (let* ((state (make-instance 'rv32i-asm-state))
	 (encoding nil))
    (reset-asm-symtab state)
    ;; Negative small immediate (-1)
    (encode-instr state '(li x3 -1))
    (setf encoding (aref (prog-mem state) 0))
    (fiveam:is (sl= #b0010011 (bits encoding 6 0)))  ;; addi opcode
    (fiveam:is (sl= 3 (bits encoding 11 7)))          ;; rd = x3
    (fiveam:is (sl= 0 (bits encoding 19 15)))          ;; rs1 = x0
    (fiveam:is (sl= #xFFF (bits encoding 31 20)))))   ;; imm = sign-extended -1

;;; LI rd, large_imm => lui rd, upper + addi rd, lower, rd
(fiveam:test encode-li-large
  (let* ((state (make-instance 'rv32i-asm-state))
	 (encoding-lui nil)
	 (encoding-addi nil))
    (reset-asm-symtab state)
    ;; Large immediate: 0x12345 (no sign compensation needed, lower bits < 0x800)
    (encode-instr state (list 'li 'x9 #x12345))
    (setf encoding-lui (aref (prog-mem state) 0))
    (setf encoding-addi (aref (prog-mem state) 1))
    ;; LUI x9, 0x12
    (fiveam:is (sl= #b0110111 (bits encoding-lui 6 0)))  ;; lui opcode
    (fiveam:is (sl= 9 (bits encoding-lui 11 7)))          ;; rd = x9
    (fiveam:is (sl= #x12 (bits encoding-lui 31 12)))      ;; upper = 0x12
    ;; ADDI x9, 0x345, x9
    (fiveam:is (sl= #b0010011 (bits encoding-addi 6 0)))  ;; addi opcode
    (fiveam:is (sl= 9 (bits encoding-addi 11 7)))         ;; rd = x9
    (fiveam:is (sl= 9 (bits encoding-addi 19 15)))        ;; rs1 = x9
    (fiveam:is (sl= #x345 (bits encoding-addi 31 20))))) ;; lower = 0x345

;;; LI rd, large_imm with sign compensation
(fiveam:test encode-li-large-sign-compensate
  (let* ((state (make-instance 'rv32i-asm-state))
	 (encoding-lui nil)
	 (encoding-addi nil))
    (reset-asm-symtab state)
    ;; 0x12FFF => lower = 0xFFF (>= 0x800), needs +1 on upper
    (encode-instr state (list 'li 'x9 #x12FFF))
    (setf encoding-lui (aref (prog-mem state) 0))
    (setf encoding-addi (aref (prog-mem state) 1))
    ;; LUI x9, 0x13 (0x12 + 1 compensation)
    (fiveam:is (sl= #b0110111 (bits encoding-lui 6 0)))
    (fiveam:is (sl= 9 (bits encoding-lui 11 7)))
    (fiveam:is (sl= #x13 (bits encoding-lui 31 12)))
    ;; ADDI x9, -1, x9  (0xFFF sign-extended = -1)
    (fiveam:is (sl= #b0010011 (bits encoding-addi 6 0)))
    (fiveam:is (sl= 9 (bits encoding-addi 11 7)))
    (fiveam:is (sl= 9 (bits encoding-addi 19 15)))
    (fiveam:is (sl= #xFFF (bits encoding-addi 31 20)))))

;;; J offset => jal x0, offset
(fiveam:test encode-j
  (let* ((state (make-instance 'rv32i-asm-state))
	 (encoding nil))
    (reset-asm-symtab state)
    (encode-instr state '(j #u21b_0_00000000_0_0000001000_0))
    (setf encoding (aref (prog-mem state) 0))
    ;; Should be jal x0, offset
    (fiveam:is (sl= #b1101111 (bits encoding 6 0)))  ;; jal opcode
    (fiveam:is (sl= 0 (bits encoding 11 7)))))        ;; rd = x0

;;; RET => jalr x0, 0, ra
(fiveam:test encode-ret
  (let* ((state (make-instance 'rv32i-asm-state))
	 (encoding nil))
    (reset-asm-symtab state)
    (encode-instr state '(ret))
    (setf encoding (aref (prog-mem state) 0))
    ;; Should be jalr x0, 0, ra (x1)
    (fiveam:is (sl= #b1100111 (bits encoding 6 0)))  ;; jalr opcode
    (fiveam:is (sl= 0 (bits encoding 11 7)))          ;; rd = x0
    (fiveam:is (sl= 0 (bits encoding 14 12)))          ;; funct3 = 000
    (fiveam:is (sl= 1 (bits encoding 19 15)))          ;; rs1 = ra (x1)
    (fiveam:is (sl= 0 (bits encoding 31 20)))))        ;; imm = 0
