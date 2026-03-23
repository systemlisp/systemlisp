(in-package :sl-examples)

;;; FemtoRV32 program-level tests
;;; Each test assembles a small program, runs the simulation,
;;; and checks register values after execution.
(fiveam:def-suite* femtorv32-prog-tests)

(defun get-cpu-regval (reg-name)
  (let* ((cpu (-> *sim* sim-tops first cpu))
	 (reg-file (-> cpu register-file))
	 (asm-state (make-instance 'rv32i-asm-state))
	 (result 0))
    (reset-asm-state asm-state)
    (setf result (value (aref reg-file (gethash reg-name (symtab asm-state)))))
    (or result
	(error "Invalid register name ~a" reg-name))))

;;; ========== U-type instructions ==========

(fiveam:test femtorv32-test-instr-lui
  (let ((test-prog '((lui x5 #u20x_12345))))
    (run-femtorv32-dram-tb
     test-prog
     500
     (lambda ()
       (let ((x5-val (get-cpu-regval 'x5)))
	 (fiveam:is (sl= #u32x_12345_000 x5-val)))))))

(fiveam:test femtorv32-test-instr-auipc
  ;; PC is 24-bit, so AUIPC result is limited to 24 bits.
  ;; Use a small immediate whose Uimm (imm << 12) fits in 24 bits.
  (let ((test-prog '((auipc x5 #u20x_00042)
		     (nop) (nop))))
    (run-femtorv32-dram-tb
     test-prog 500
     (lambda ()
       ;; AUIPC at PC=0: x5 = 0 + 0x42000
       (fiveam:is (sl= #x42000 (get-cpu-regval 'x5)))))))

;;; ========== I-type ALU instructions ==========

(fiveam:test femtorv32-test-instr-addi
  (let ((test-prog '((addi x5 x0 42)
		     (addi x6 x0 100)
		     (addi x7 x6 -30)	; 100 + (-30) = 70
		     (nop) (nop))))
    (run-femtorv32-dram-tb
     test-prog 500
     (lambda ()
       (fiveam:is (sl= 42 (get-cpu-regval 'x5)))
       (fiveam:is (sl= 70 (get-cpu-regval 'x7)))))))

(fiveam:test femtorv32-test-instr-slti
  (let ((test-prog '((addi x5 x0 5)
		     (slti x6 x5 10)	; 5 < 10 (signed) => 1
		     (slti x7 x5 3)	; 5 < 3 (signed) => 0
		     (nop) (nop))))
    (run-femtorv32-dram-tb
     test-prog 500
     (lambda ()
       (fiveam:is (sl= 1 (get-cpu-regval 'x6)))
       (fiveam:is (sl= 0 (get-cpu-regval 'x7)))))))

(fiveam:test femtorv32-test-instr-sltiu
  (let ((test-prog '((addi x5 x0 5)
		     (sltiu x6 x5 10)	; 5 < 10 (unsigned) => 1
		     (sltiu x7 x5 3)	; 5 < 3 (unsigned) => 0
		     (nop) (nop))))
    (run-femtorv32-dram-tb
     test-prog 500
     (lambda ()
       (fiveam:is (sl= 1 (get-cpu-regval 'x6)))
       (fiveam:is (sl= 0 (get-cpu-regval 'x7)))))))

(fiveam:test femtorv32-test-instr-xori
  (let ((test-prog '((addi x5 x0 #xFF)
		     (xori x6 x5 #x0F)	; 0xFF ^ 0x0F = 0xF0
		     (nop) (nop))))
    (run-femtorv32-dram-tb
     test-prog 500
     (lambda ()
       (fiveam:is (sl= #xF0 (get-cpu-regval 'x6)))))))

(fiveam:test femtorv32-test-instr-ori
  (let ((test-prog '((addi x5 x0 #xA0)
		     (ori x6 x5 #x0F)	; 0xA0 | 0x0F = 0xAF
		     (nop) (nop))))
    (run-femtorv32-dram-tb
     test-prog 500
     (lambda ()
       (fiveam:is (sl= #xAF (get-cpu-regval 'x6)))))))

(fiveam:test femtorv32-test-instr-andi
  (let ((test-prog '((addi x5 x0 #xFF)
		     (andi x6 x5 #x0F)	; 0xFF & 0x0F = 0x0F
		     (nop) (nop))))
    (run-femtorv32-dram-tb
     test-prog 500
     (lambda ()
       (fiveam:is (sl= #x0F (get-cpu-regval 'x6)))))))

;;; ========== I-shift type instructions ==========

(fiveam:test femtorv32-test-instr-slli
  (let ((test-prog '((addi x5 x0 1)
		     (slli x6 x5 4)	; 1 << 4 = 16
		     (nop) (nop) (nop))))
    (run-femtorv32-dram-tb
     test-prog 1000
     (lambda ()
       (fiveam:is (sl= 16 (get-cpu-regval 'x6)))))))

(fiveam:test femtorv32-test-instr-srli
  (let ((test-prog '((addi x5 x0 128)
		     (srli x6 x5 4)	; 128 >> 4 = 8
		     (nop) (nop) (nop))))
    (run-femtorv32-dram-tb
     test-prog 1000
     (lambda ()
       (fiveam:is (sl= 8 (get-cpu-regval 'x6)))))))

(fiveam:test femtorv32-test-instr-srai
  (let ((test-prog '((li x5 -16)
		     (srai x6 x5 2)	; -16 >> 2 = -4 (arithmetic)
		     (nop) (nop) (nop))))
    (run-femtorv32-dram-tb
     test-prog 1000
     (lambda ()
       (fiveam:is (sl= #u32x_FFFFFFFC (get-cpu-regval 'x6)))))))

;;; ========== R-type instructions ==========

(fiveam:test femtorv32-test-instr-add
  (let ((test-prog '((addi x5 x0 10)
		     (addi x6 x0 20)
		     (add x7 x5 x6)	; 10 + 20 = 30
		     (nop) (nop))))
    (run-femtorv32-dram-tb
     test-prog 500
     (lambda ()
       (fiveam:is (sl= 30 (get-cpu-regval 'x7)))))))

(fiveam:test femtorv32-test-instr-sub
  (let ((test-prog '((addi x5 x0 20)
		     (addi x6 x0 7)
		     (sub x7 x5 x6)	; 20 - 7 = 13
		     (nop) (nop))))
    (run-femtorv32-dram-tb
     test-prog 500
     (lambda ()
       (fiveam:is (sl= 13 (get-cpu-regval 'x7)))))))

(fiveam:test femtorv32-test-instr-sll
  (let ((test-prog '((addi x5 x0 1)
		     (addi x6 x0 4)
		     (sll x7 x5 x6)	; 1 << 4 = 16
		     (nop) (nop) (nop))))
    (run-femtorv32-dram-tb
     test-prog 1000
     (lambda ()
       (fiveam:is (sl= 16 (get-cpu-regval 'x7)))))))

(fiveam:test femtorv32-test-instr-slt
  (let ((test-prog '((addi x5 x0 5)
		     (addi x6 x0 10)
		     (slt x7 x5 x6)	; 5 < 10 => 1
		     (slt x8 x6 x5)	; 10 < 5 => 0
		     (nop) (nop))))
    (run-femtorv32-dram-tb
     test-prog 500
     (lambda ()
       (fiveam:is (sl= 1 (get-cpu-regval 'x7)))
       (fiveam:is (sl= 0 (get-cpu-regval 'x8)))))))

(fiveam:test femtorv32-test-instr-sltu
  (let ((test-prog '((addi x5 x0 5)
		     (addi x6 x0 10)
		     (sltu x7 x5 x6)	; 5 < 10 (unsigned) => 1
		     (sltu x8 x6 x5)	; 10 < 5 (unsigned) => 0
		     (nop) (nop))))
    (run-femtorv32-dram-tb
     test-prog 500
     (lambda ()
       (fiveam:is (sl= 1 (get-cpu-regval 'x7)))
       (fiveam:is (sl= 0 (get-cpu-regval 'x8)))))))

(fiveam:test femtorv32-test-instr-xor
  (let ((test-prog '((addi x5 x0 #xFF)
		     (addi x6 x0 #x0F)
		     (xor x7 x5 x6)	; 0xFF ^ 0x0F = 0xF0
		     (nop) (nop))))
    (run-femtorv32-dram-tb
     test-prog 500
     (lambda ()
       (fiveam:is (sl= #xF0 (get-cpu-regval 'x7)))))))

(fiveam:test femtorv32-test-instr-srl
  (let ((test-prog '((addi x5 x0 128)
		     (addi x6 x0 4)
		     (srl x7 x5 x6)	; 128 >> 4 = 8
		     (nop) (nop) (nop))))
    (run-femtorv32-dram-tb
     test-prog 1000
     (lambda ()
       (fiveam:is (sl= 8 (get-cpu-regval 'x7)))))))

(fiveam:test femtorv32-test-instr-sra
  (let ((test-prog '((li x5 -16)
		     (addi x6 x0 2)
		     (sra x7 x5 x6)	; -16 >> 2 = -4 (arithmetic)
		     (nop) (nop) (nop))))
    (run-femtorv32-dram-tb
     test-prog 1000
     (lambda ()
       (fiveam:is (sl= #u32x_FFFFFFFC (get-cpu-regval 'x7)))))))

(fiveam:test femtorv32-test-instr-or
  (let ((test-prog '((addi x5 x0 #xA0)
		     (addi x6 x0 #x0F)
		     (or x7 x5 x6)	; 0xA0 | 0x0F = 0xAF
		     (nop) (nop))))
    (run-femtorv32-dram-tb
     test-prog 500
     (lambda ()
       (fiveam:is (sl= #xAF (get-cpu-regval 'x7)))))))

(fiveam:test femtorv32-test-instr-and
  (let ((test-prog '((addi x5 x0 #xFF)
		     (addi x6 x0 #x0F)
		     (and x7 x5 x6)	; 0xFF & 0x0F = 0x0F
		     (nop) (nop))))
    (run-femtorv32-dram-tb
     test-prog 500
     (lambda ()
       (fiveam:is (sl= #x0F (get-cpu-regval 'x7)))))))

;;; ========== Store/Load instructions ==========

(fiveam:test femtorv32-test-instr-sw-lw
  (let ((test-prog '((addi x5 x0 42)	  ; data
		     (li x6 256)	  ; address (0x100)
		     (sw x5 0 x6)	  ; mem[256] = 42
		     (lw x7 x6 0)	  ; x7 = mem[256]
		     (nop) (nop))))
    (run-femtorv32-dram-tb
     test-prog 1000
     (lambda ()
       (fiveam:is (sl= 42 (get-cpu-regval 'x7)))))))

(fiveam:test femtorv32-test-instr-sh-lh-lhu
  (let ((test-prog '((addi x5 x0 #x7AB)  ; data (1963, no sign extension)
		     (li x6 256)	  ; address
		     (sh x5 0 x6)	  ; store halfword
		     (lh x7 x6 0)	  ; load halfword (sign-extended)
		     (lhu x8 x6 0)	  ; load halfword (zero-extended)
		     (nop) (nop))))
    (run-femtorv32-dram-tb
     test-prog 1000
     (lambda ()
       (fiveam:is (sl= #x7AB (get-cpu-regval 'x7)))
       (fiveam:is (sl= #x7AB (get-cpu-regval 'x8)))))))

(fiveam:test femtorv32-test-instr-sb-lb-lbu
  (let ((test-prog '((addi x5 x0 -1)	  ; x5 = 0xFFFFFFFF
		     (li x6 256)	  ; address
		     (sb x5 0 x6)	  ; store byte 0xFF
		     (lb x7 x6 0)	  ; load byte sign-extended => 0xFFFFFFFF
		     (lbu x8 x6 0)	  ; load byte zero-extended => 0x000000FF
		     (nop) (nop))))
    (run-femtorv32-dram-tb
     test-prog 1000
     (lambda ()
       (fiveam:is (sl= #u32x_FFFFFFFF (get-cpu-regval 'x7)))
       (fiveam:is (sl= #xFF (get-cpu-regval 'x8)))))))

;;; ========== Branch instructions ==========

(fiveam:test femtorv32-test-instr-beq
  (let ((test-prog '((addi x5 x0 5)
		     (addi x6 x0 5)
		     (beq x5 x6 8)	; PC=8: branch to 16 (taken: 5==5)
		     (addi x7 x0 99)	; PC=12: skipped
		     (addi x8 x0 42)	; PC=16: branch target
		     (nop) (nop))))
    (run-femtorv32-dram-tb
     test-prog 500
     (lambda ()
       (fiveam:is (sl= 0 (get-cpu-regval 'x7)))
       (fiveam:is (sl= 42 (get-cpu-regval 'x8)))))))

(fiveam:test femtorv32-test-instr-bne
  (let ((test-prog '((addi x5 x0 5)
		     (addi x6 x0 10)
		     (bne x5 x6 8)	; branch taken: 5 != 10
		     (addi x7 x0 99)	; skipped
		     (addi x8 x0 42)	; branch target
		     (nop) (nop))))
    (run-femtorv32-dram-tb
     test-prog 500
     (lambda ()
       (fiveam:is (sl= 0 (get-cpu-regval 'x7)))
       (fiveam:is (sl= 42 (get-cpu-regval 'x8)))))))

(fiveam:test femtorv32-test-instr-blt
  (let ((test-prog '((li x5 -1)	  ; x5 = -1 (signed)
		     (addi x6 x0 1)
		     (blt x5 x6 8)	; -1 < 1 (signed), taken
		     (addi x7 x0 99)	; skipped
		     (addi x8 x0 42)	; branch target
		     (nop) (nop))))
    (run-femtorv32-dram-tb
     test-prog 500
     (lambda ()
       (fiveam:is (sl= 0 (get-cpu-regval 'x7)))
       (fiveam:is (sl= 42 (get-cpu-regval 'x8)))))))

(fiveam:test femtorv32-test-instr-bge
  (let ((test-prog '((addi x5 x0 10)
		     (addi x6 x0 5)
		     (bge x5 x6 8)	; 10 >= 5, taken
		     (addi x7 x0 99)	; skipped
		     (addi x8 x0 42)	; branch target
		     (nop) (nop))))
    (run-femtorv32-dram-tb
     test-prog 500
     (lambda ()
       (fiveam:is (sl= 0 (get-cpu-regval 'x7)))
       (fiveam:is (sl= 42 (get-cpu-regval 'x8)))))))

(fiveam:test femtorv32-test-instr-bltu
  (let ((test-prog '((li x5 -1)	  ; x5 = 0xFFFFFFFF (large unsigned)
		     (addi x6 x0 1)
		     (bltu x5 x6 8)	; 0xFFFFFFFF < 1 unsigned? NO, not taken
		     (addi x7 x0 42)	; NOT skipped
		     (nop) (nop))))
    (run-femtorv32-dram-tb
     test-prog 500
     (lambda ()
       (fiveam:is (sl= 42 (get-cpu-regval 'x7)))))))

(fiveam:test femtorv32-test-instr-bgeu
  (let ((test-prog '((li x5 -1)	  ; x5 = 0xFFFFFFFF (large unsigned)
		     (addi x6 x0 1)
		     (bgeu x5 x6 8)	; 0xFFFFFFFF >= 1 unsigned? YES, taken
		     (addi x7 x0 99)	; skipped
		     (addi x8 x0 42)	; branch target
		     (nop) (nop))))
    (run-femtorv32-dram-tb
     test-prog 500
     (lambda ()
       (fiveam:is (sl= 0 (get-cpu-regval 'x7)))
       (fiveam:is (sl= 42 (get-cpu-regval 'x8)))))))

;;; ========== Jump instructions ==========

(fiveam:test femtorv32-test-instr-jal
  (let ((test-prog '((jal x5 8)	  ; PC=0: x5=4, jump to 8
		     (addi x6 x0 99)	  ; PC=4: skipped
		     (addi x7 x0 42)	  ; PC=8: target
		     (nop) (nop))))
    (run-femtorv32-dram-tb
     test-prog 500
     (lambda ()
       (fiveam:is (sl= 4 (get-cpu-regval 'x5)))
       (fiveam:is (sl= 0 (get-cpu-regval 'x6)))
       (fiveam:is (sl= 42 (get-cpu-regval 'x7)))))))

(fiveam:test femtorv32-test-instr-jalr
  (let ((test-prog '((addi x5 x0 12)	  ; PC=0: x5 = 12 (target)
		     (jalr x6 x5 0)	  ; PC=4: x6=8, jump to x5+0=12
		     (addi x7 x0 99)	  ; PC=8: skipped
		     (addi x8 x0 42)	  ; PC=12: target
		     (nop) (nop))))
    (run-femtorv32-dram-tb
     test-prog 500
     (lambda ()
       (fiveam:is (sl= 8 (get-cpu-regval 'x6)))
       (fiveam:is (sl= 0 (get-cpu-regval 'x7)))
       (fiveam:is (sl= 42 (get-cpu-regval 'x8)))))))

;;; ========== Pseudoinstructions ==========

(fiveam:test femtorv32-test-pseudo-nop
  (let ((test-prog '((addi x5 x0 42)
		     (nop)
		     (addi x6 x0 99)
		     (nop) (nop))))
    (run-femtorv32-dram-tb
     test-prog 500
     (lambda ()
       (fiveam:is (sl= 42 (get-cpu-regval 'x5)))
       (fiveam:is (sl= 99 (get-cpu-regval 'x6)))))))

(fiveam:test femtorv32-test-pseudo-mv
  (let ((test-prog '((addi x5 x0 42)
		     (mv x6 x5)	; x6 = x5 = 42
		     (nop) (nop))))
    (run-femtorv32-dram-tb
     test-prog 500
     (lambda ()
       (fiveam:is (sl= 42 (get-cpu-regval 'x6)))))))

(fiveam:test femtorv32-test-pseudo-li-small
  (let ((test-prog '((li x5 42)
		     (li x6 -7)
		     (nop) (nop))))
    (run-femtorv32-dram-tb
     test-prog 500
     (lambda ()
       (fiveam:is (sl= 42 (get-cpu-regval 'x5)))))))

(fiveam:test femtorv32-test-pseudo-li-large
  (let ((test-prog '((li x5 #x12345678)
		     (nop) (nop) (nop))))
    (run-femtorv32-dram-tb
     test-prog 500
     (lambda ()
       (fiveam:is (sl= #u32x_12345678 (get-cpu-regval 'x5)))))))

(fiveam:test femtorv32-test-pseudo-j
  (let ((test-prog '((j 8)		  ; PC=0: jump to 8
		     (addi x5 x0 99)	  ; PC=4: skipped
		     (addi x6 x0 42)	  ; PC=8: target
		     (nop) (nop))))
    (run-femtorv32-dram-tb
     test-prog 500
     (lambda ()
       (fiveam:is (sl= 0 (get-cpu-regval 'x5)))
       (fiveam:is (sl= 42 (get-cpu-regval 'x6)))))))

(fiveam:test femtorv32-test-pseudo-ret
  (let ((test-prog '((addi x1 x0 12)	  ; ra = 12 (return address)
		     (ret)		  ; jump to ra
		     (addi x5 x0 99)	  ; PC=8: skipped
		     (addi x6 x0 42)	  ; PC=12: return target
		     (nop) (nop))))
    (run-femtorv32-dram-tb
     test-prog 500
     (lambda ()
       (fiveam:is (sl= 0 (get-cpu-regval 'x5)))
       (fiveam:is (sl= 42 (get-cpu-regval 'x6)))))))

;;; ========== Label + loop test ==========

(fiveam:test femtorv32-test-label-sum-loop
  ;; Compute sum of 1+2+3+4+5 = 15 using a backward branch to a label.
  ;; t0 = accumulator, t1 = counter (1..5), t2 = limit (6)
  (let ((test-prog '((li t0 0)		  ; PC=0:  sum = 0
		     (li t1 1)		  ; PC=4:  counter = 1
		     (li t2 6)		  ; PC=8:  limit = 6 (stop when counter reaches 6)
		     (label loop)	  ;         loop target at PC=12
		     (add t0 t0 t1)	  ; PC=12: sum += counter
		     (addi t1 t1 1)	  ; PC=16: counter++
		     (bne t1 t2 loop)	  ; PC=20: if counter != limit, goto loop
		     (nop) (nop))))
    (run-femtorv32-dram-tb
     test-prog 2000
     (lambda ()
       (fiveam:is (sl= 15 (get-cpu-regval 't0)))
       (fiveam:is (sl= 6 (get-cpu-regval 't1)))))))
