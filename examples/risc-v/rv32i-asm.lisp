(in-package :system-lisp-examples)

;;; Encoding for each instruction type
(defun r-type-instr (instr rd rs1 rs2)
  (let ((opcode (get-instr-opcode instr))
	(funct7 (get-funct7-value instr))
	(funct3 (get-funct3-value instr)))
    (concat (uint-cast funct7 :bits 7)
	    (uint-cast rs2 :bits 5)
	    (uint-cast rs1 :bits 5)
	    (uint-cast funct3 :bits 3)
	    (uint-cast rd :bits 5)
	    (uint-cast opcode :bits 7))))

(defun i-type-instr (instr rd rs1 imm)
  (let ((opcode (get-instr-opcode instr))
	(funct3 (get-funct3-value instr))
	(imm-int (int-cast imm :bits 12)))
    (concat imm-int
	    (uint-cast rs1 :bits 5)
	    (uint-cast funct3 :bits 3)
	    (uint-cast rd :bits 5)
	    (uint-cast opcode :bits 7))))

(defun i-shift-type-instr (instr rd rs1 shamt)
  (let ((opcode (get-instr-opcode instr))
	(funct7 (get-funct7-value instr))
	(funct3 (get-funct3-value instr)))
    (concat (uint-cast funct7 :bits 7)
	    (uint-cast shamt :bits 5)
	    (uint-cast rs1 :bits 5)
	    (uint-cast funct3 :bits 3)
	    (uint-cast rd :bits 5)
	    (uint-cast opcode :bits 7))))

(defun s-type-instr (instr rs2 imm  rs1)
  (let ((opcode (get-instr-opcode instr))
	(funct3 (get-funct3-value instr))
	(imm-int (int-cast imm :bits 12)))
    (concat (bits imm-int 11 5)
	    (uint-cast rs2 :bits 5)
	    (uint-cast rs1 :bits 5)
	    (uint-cast funct3 :bits 3)
	    (bits imm-int 4 0)
	    (uint-cast opcode :bits 7))))

(defun b-type-instr (instr rs1 rs2 imm)
  (let ((opcode (get-instr-opcode instr))
	(funct3 (get-funct3-value instr))
	(imm-int (int-cast imm :bits 13)))
    (concat (bit imm-int 12)
	    (bits imm-int 10 5)
	    (uint-cast rs2 :bits 5)
	    (uint-cast rs1 :bits 5)
	    (uint-cast funct3 :bits 3)
	    (bits imm-int 4 1)
	    (bit imm-int 11)
	    (uint-cast opcode :bits 7))))

(defun u-type-instr (instr rd imm)
  (let ((opcode (get-instr-opcode instr))
	(imm-int (int-cast imm :bits 20)))
    (concat imm-int
	    (uint-cast rd :bits 5)
	    (uint-cast opcode :bits 7))))

(defun j-type-instr (instr rd imm)
  (let ((opcode (get-instr-opcode instr))
	(imm-int (int-cast imm :bits 21)))
    (concat (bit imm-int 20)
	    (bits imm-int 10 1)
	    (bit imm-int 11)
	    (bits imm-int 19 12)
	    (uint-cast rd :bits 5)
	    (uint-cast opcode :bits 7))))

(defun fence-type-instr (opcode fm pred succ rs1 rd))

(defparameter *rv32i-instr-type-nr-args*
  (let ((result (make-hash-table)))
    (setf (gethash 'r-type-instr result) 3)
    (setf (gethash 'i-type-instr result) 3)
    (setf (gethash 'i-shift-type-instr result) 3)
    (setf (gethash 's-type-instr result) 3)
    (setf (gethash 'b-type-instr result) 3)
    (setf (gethash 'u-type-instr result) 2)
    (setf (gethash 'j-type-instr result) 2)
    (setf (gethash 'fence-type-instr result) 5)
    result))

(defun get-instr-type-nr-args (instr-type)
  (let ((result (gethash instr-type  *rv32i-instr-type-nr-args*)))
    (if result
	result
	;; else
	(error "Invalid instruction type: ~a" instr-type))))

;;; Instruction opcodes
(defparameter *rv32i-instr-opcodes*
  (let ((result (make-hash-table)))
    (setf (gethash 'lui   result) #b0110111)
    (setf (gethash 'auipc result) #b0010111)

    (setf (gethash 'jal   result) #b1101111)
    (setf (gethash 'jalr  result) #b1100111)

    (setf (gethash 'beq   result) #b1100011)
    (setf (gethash 'bne   result) #b1100011)
    (setf (gethash 'blt   result) #b1100011)
    (setf (gethash 'bge   result) #b1100011)
    (setf (gethash 'bltu  result) #b1100011)
    (setf (gethash 'bgeu  result) #b1100011)

    (setf (gethash 'lb    result) #b0000011)
    (setf (gethash 'lh    result) #b0000011)
    (setf (gethash 'lw    result) #b0000011)
    (setf (gethash 'lbu   result) #b0000011)
    (setf (gethash 'lhu   result) #b0000011)

    (setf (gethash 'sb    result) #b0100011)
    (setf (gethash 'sh    result) #b0100011)
    (setf (gethash 'sw    result) #b0100011)

    (setf (gethash 'addi  result) #b0010011)
    (setf (gethash 'slti  result) #b0010011)
    (setf (gethash 'sltiu result) #b0010011)
    (setf (gethash 'xori  result) #b0010011)
    (setf (gethash 'ori   result) #b0010011)
    (setf (gethash 'andi  result) #b0010011)
    (setf (gethash 'slli  result) #b0010011)
    (setf (gethash 'srli  result) #b0010011)
    (setf (gethash 'srai  result) #b0010011)

    (setf (gethash 'add   result) #b0110011)
    (setf (gethash 'sub   result) #b0110011)
    (setf (gethash 'sll   result) #b0110011)
    (setf (gethash 'slt   result) #b0110011)
    (setf (gethash 'sltu  result) #b0110011)
    (setf (gethash 'xor   result) #b0110011)
    (setf (gethash 'srl   result) #b0110011)
    (setf (gethash 'sra   result) #b0110011)
    (setf (gethash 'or    result) #b0110011)
    (setf (gethash 'and   result) #b0110011)

    (setf (gethash 'fence result) #b0001111)

    (setf (gethash 'ecall  result) #b1110011)
    (setf (gethash 'ebreak result) #b1110011)
    
    result))

(defun get-instr-opcode (instr)
  (let ((result (gethash instr *rv32i-instr-opcodes*)))
    (if result
	result
	;; else
	(error "Invalid instruction name: ~a" instr))))

;;; Instruction types
(defparameter *rv32i-instr-types*
  (let ((result (make-hash-table)))
    (setf (gethash 'lui   result) 'u-type-instr)
    (setf (gethash 'auipc result) 'u-type-instr)

    (setf (gethash 'jal   result) 'j-type-instr)
    (setf (gethash 'jalr  result) 'i-type-instr)

    (setf (gethash 'beq   result) 'b-type-instr)
    (setf (gethash 'bne   result) 'b-type-instr)
    (setf (gethash 'blt   result) 'b-type-instr)
    (setf (gethash 'bge   result) 'b-type-instr)
    (setf (gethash 'bltu  result) 'b-type-instr)
    (setf (gethash 'bgeu  result) 'b-type-instr)

    (setf (gethash 'lb    result) 'i-type-instr)
    (setf (gethash 'lh    result) 'i-type-instr)
    (setf (gethash 'lw    result) 'i-type-instr)
    (setf (gethash 'lbu   result) 'i-type-instr)
    (setf (gethash 'lhu   result) 'i-type-instr)

    (setf (gethash 'sb    result) 's-type-instr)
    (setf (gethash 'sh    result) 's-type-instr)
    (setf (gethash 'sw    result) 's-type-instr)

    (setf (gethash 'addi  result) 'i-type-instr)
    (setf (gethash 'slti  result) 'i-type-instr)
    (setf (gethash 'sltiu result) 'i-type-instr)
    (setf (gethash 'xori  result) 'i-type-instr)
    (setf (gethash 'ori   result) 'i-type-instr)
    (setf (gethash 'andi  result) 'i-type-instr)
    (setf (gethash 'slli  result) 'i-shift-type-instr)
    (setf (gethash 'srli  result) 'i-shift-type-instr)
    (setf (gethash 'srai  result) 'i-shift-type-instr)

    (setf (gethash 'add   result) 'r-type-instr)
    (setf (gethash 'sub   result) 'r-type-instr)
    (setf (gethash 'sll   result) 'r-type-instr)
    (setf (gethash 'slt   result) 'r-type-instr)
    (setf (gethash 'sltu  result) 'r-type-instr)
    (setf (gethash 'xor   result) 'r-type-instr)
    (setf (gethash 'srl   result) 'r-type-instr)
    (setf (gethash 'sra   result) 'r-type-instr)
    (setf (gethash 'or    result) 'r-type-instr)
    (setf (gethash 'and   result) 'r-type-instr)

    (setf (gethash 'fence result) 'fence-type-instr)

    (setf (gethash 'ecall  result) 'i-type-instr)
    (setf (gethash 'ebreak result) 'i-type-instr)
    
    result))

(defun get-instr-type (instr)
  (let ((result (gethash instr *rv32i-instr-types*)))
    (if result
	result
	;; else
	(error "Invalid instruction name: ~a" instr))))

;;; funct3 values
(defparameter *rv32i-instr-funct3-values*
  (let ((result (make-hash-table)))
    (setf (gethash 'jalr  result) #b000)

    (setf (gethash 'beq   result) #b000)
    (setf (gethash 'bne   result) #b001)
    (setf (gethash 'blt   result) #b100)
    (setf (gethash 'bge   result) #b101)
    (setf (gethash 'bltu  result) #b110)
    (setf (gethash 'bgeu  result) #b111)

    (setf (gethash 'lb    result) #b000)
    (setf (gethash 'lh    result) #b001)
    (setf (gethash 'lw    result) #b010)
    (setf (gethash 'lbu   result) #b100)
    (setf (gethash 'lhu   result) #b101)

    (setf (gethash 'sb    result) #b000)
    (setf (gethash 'sh    result) #b001)
    (setf (gethash 'sw    result) #b010)

    (setf (gethash 'addi  result) #b000)
    (setf (gethash 'slti  result) #b010)
    (setf (gethash 'sltiu result) #b011)
    (setf (gethash 'xori  result) #b100)
    (setf (gethash 'ori   result) #b110)
    (setf (gethash 'andi  result) #b111)
    (setf (gethash 'slli  result) #b001)
    (setf (gethash 'srli  result) #b101)
    (setf (gethash 'srai  result) #b101)

    (setf (gethash 'add   result) #b000)
    (setf (gethash 'sub   result) #b000)
    (setf (gethash 'sll   result) #b001)
    (setf (gethash 'slt   result) #b010)
    (setf (gethash 'sltu  result) #b011)
    (setf (gethash 'xor   result) #b100)
    (setf (gethash 'srl   result) #b101)
    (setf (gethash 'sra   result) #b101)
    (setf (gethash 'or    result) #b110)
    (setf (gethash 'and   result) #b111)

    (setf (gethash 'fence result) #b000)

    (setf (gethash 'ecall  result) #b000)
    (setf (gethash 'ebreak result) #b000)
    
    result))

(defun get-funct3-value (instr)
  (assert (member instr *rv32i-instr-names*) (instr) "Invalid instruction name: ~a" instr)
  (let ((result (gethash instr *rv32i-instr-funct3-values*)))
    (if result
	result
	;;else
	(error "Instruction ~a does not require a funct3 argument" instr))))

;;; funct7 values
(defparameter *rv32i-instr-funct7-values*
  (let ((result (make-hash-table)))
    (setf (gethash 'slli  result) #b0000000)
    (setf (gethash 'srli  result) #b0000000)
    (setf (gethash 'srai  result) #b0100000)

    (setf (gethash 'add   result) #b0000000)
    (setf (gethash 'sub   result) #b0100000)
    (setf (gethash 'sll   result) #b0000000)
    (setf (gethash 'slt   result) #b0000000)
    (setf (gethash 'sltu  result) #b0000000)
    (setf (gethash 'xor   result) #b0000000)
    (setf (gethash 'srl   result) #b0000000)
    (setf (gethash 'sra   result) #b0100000)
    (setf (gethash 'or    result) #b0000000)
    (setf (gethash 'and   result) #b0000000)
    
    result))

(defun get-funct7-value (instr)
  (assert (member instr *rv32i-instr-names*) (instr) "Invalid instruction name: ~a" instr)
  (let ((result (gethash instr *rv32i-instr-funct7-values*)))
    (if result
	result
	;;else
	(error "Instruction ~a does not require a funct7 argument" instr))))

(defparameter *rv32i-instr-names*
  '(LUI AUIPC JAL JALR BEQ
    BNE BLT BGE BLTU BGEU
    LB LH LW LBU LHU SB SH
    SW ADDI SLTI SLTIU XORI
    ORI ANDI SLLI SRLI SRAI
    ADD SUB SLL SLT SLTU
    XOR SRL SRA OR AND FENCE
    ECALL EBREAK))

(defclass rv32i-asm-state ()
  ((symtab
    :accessor symtab
    :initform (make-hash-table))
   (instr-count
    :accessor instr-count
    :initform 0)
   (pc0
    :accessor pc0
    :initarg :pc0
    :initform 0)
   (prog-mem
    :accessor prog-mem
    :initform (make-array 0 :adjustable t :fill-pointer 0))
   (data0
    :accessor data0
    :initarg :data0
    :initform #x800000)
   (data-offset
    :accessor data-offset
    :initform 0)
   (data-mem
    :accessor data-mem
    :initform (make-array 0 :adjustable t :fill-pointer 0))))

(defmethod reset-asm-symtab ((asm-state rv32i-asm-state))
  (with-slots (symtab) asm-state
    ;; x0 - x31 register mnemonics
    (loop for i from 0 to 31 do
      (setf (gethash (intern (format nil "X~a" i)) symtab) i))
    ;; Hard-wired zero
    (setf (gethash 'zero symtab) 0)
    ;; Return address
    (setf (gethash 'ra symtab)   1)
    ;; Stack pointer
    (setf (gethash 'sp symtab)   2)
    ;; Global pointer
    (setf (gethash 'gp symtab)   3)
    ;; Thread pointer
    (setf (gethash 'tp symtab)   4)
    ;; Temporary/alternate link register
    (setf (gethash 't0 symtab)   5)
    ;; Temporaries t1-2
    (setf (gethash 't1 symtab)   6)
    (setf (gethash 't2 symtab)   7)
    ;; Saved register s0/frame pointer
    (setf (gethash 's0 symtab)   8)
    (setf (gethash 'fp symtab)   8)
    ;; Saved register s1
    (setf (gethash 's1 symtab)   9)
    ;; Function arguments/return values
    (setf (gethash 'a0 symtab)   10)
    (setf (gethash 'a1 symtab)   11)
    ;; Function arguments 
    (setf (gethash 'a2 symtab)   12)
    (setf (gethash 'a3 symtab)   13)
    (setf (gethash 'a4 symtab)   14)
    (setf (gethash 'a5 symtab)   15)
    (setf (gethash 'a6 symtab)   16)
    (setf (gethash 'a7 symtab)   17)
    ;; Saved registers s2-11
    (setf (gethash 's2 symtab)    18)
    (setf (gethash 's3 symtab)    19)
    (setf (gethash 's4 symtab)    20)
    (setf (gethash 's5 symtab)    21)
    (setf (gethash 's6 symtab)    22)
    (setf (gethash 's7 symtab)    23)
    (setf (gethash 's8 symtab)    24)
    (setf (gethash 's9 symtab)    25)
    (setf (gethash 's10 symtab)   26)
    (setf (gethash 's11 symtab)   27)
    ;; Temporaries t3-6
    (setf (gethash 't3 symtab)   28)
    (setf (gethash 't4 symtab)   29)
    (setf (gethash 't5 symtab)   30)
    (setf (gethash 't6 symtab)   31)))

(defmethod reset-asm-state ((asm-state rv32i-asm-state))
  (reset-asm-symtab asm-state)
  (setf (fill-pointer (data-mem asm-state)) 0)
  (setf (fill-pointer (prog-mem asm-state)) 0)
  (setf (pc0 asm-state) 0)
  (setf (data0 asm-state) 0)
  (setf (data-offset asm-state) 0))

(defmethod check-asm-symbol-available ((asm-state rv32i-asm-state) (symbol symbol))
  (assert (null (gethash symbol (symtab asm-state))) (asm-state symbol) "Symbol ~a already in use."))

(defmethod check-instr-args ((asm-state rv32i-asm-state) (instr symbol) (args list))
  (let ((result (make-array 0 :adjustable t :fill-pointer 0)))
    (assert (= (length args) (get-instr-type-nr-args (get-instr-type instr)))
	    (instr args)
	    "Invalid number of arguments (~a) given to ~a expecting ~a"
	    (length args)
	    instr
	    (get-instr-type-nr-args (get-instr-type instr)))
    (loop for arg in args do
      (assert (typep arg '(or integer sl-uint sl-int symbol)) (instr args)
	      "Argument ~a is of type ~a, expected '(or integer sl-uint sl-int)"
	      arg (type-of arg))
      ;; Replace mnemonics with numeric values
      (if (symbolp arg)
	  (let ((symval (gethash arg (symtab asm-state))))
	    (assert (not (null symval)) (asm-state instr args)
		    "Symbol ~a not found in symtab" arg)
	    (vector-push-extend symval result))
	  ;; else
	  (vector-push-extend arg result)))
    (coerce result 'list)))

(defmethod  encode-instr ((asm-state rv32i-asm-state) (asm-instr list))
  "Convert an S-expr asm command into a 32 bit encoding"
  (let ((instr-name (car asm-instr))
	(instr-args (cdr asm-instr))
	(instr-args-final nil))
    (cond
      ;; Primitive instructions
      ((and (symbolp instr-name)
	    (member instr-name *rv32i-instr-names*))
       (let ((instr-encoding 0)
	     (instr-type (get-instr-type instr-name)))
	 (setf instr-args-final (check-instr-args asm-state instr-name instr-args))
	 (incf (instr-count asm-state))
	 ;; For B-type and J-type: if the immediate arg was a label symbol,
	 ;; convert its absolute byte address to a PC-relative offset.
	 (let ((current-pc (+ (pc0 asm-state) (* 4 (1- (instr-count asm-state))))))
	   (when (and (eql instr-type 'b-type-instr)
		      (symbolp (nth 2 instr-args)))
	     (setf (nth 2 instr-args-final)
		   (- (nth 2 instr-args-final) current-pc)))
	   (when (and (eql instr-type 'j-type-instr)
		      (symbolp (nth 1 instr-args)))
	     (setf (nth 1 instr-args-final)
		   (- (nth 1 instr-args-final) current-pc))))
	 (setf instr-encoding (apply instr-type
				     (cons
				      instr-name
				      instr-args-final)))
	 (vector-push-extend instr-encoding (-> asm-state prog-mem))))
      ;; Label declaration
      ((eql instr-name 'label)
       (progn
	 (assert (= (length instr-args) 1) (instr-name instr-args)
		 "Label declaration only takes 1 argument.")
	 (assert (symbolp (car instr-args)) (instr-name instr-args)
		 "Label name must be a symbol.")
	 (check-asm-symbol-available asm-state (car instr-args))
	 (setf (gethash (car instr-args) (symtab asm-state))
	       (+ (pc0 asm-state) (* 4 (instr-count asm-state))))
	 t))
      ;; Variable declaration (var var-type var-name &optional initial-value)
      ((and (symbolp instr-name)
	    (eql instr-name 'var)
	    (member (car instr-args) '(u8 u16 u32))
	    (member (length instr-args) '(2 3))
	    (if (= 3 (length instr-args)) (numberp (third instr-args)) t))
       (let* ((type-size (parse-integer (symbol-name (car instr-args))))
	      (var-name (cadr instr-args))
	      (init-val (if (= 3 (length instr-args)) (third instr-args) 0)))
	 (setf (gethash var-name (symtab asm-state)) (+ (data0 asm-state) (data-offset asm-state)))
	 (incf (data-offset asm-state))
	 (vector-push-extend (uint-cast init-val :bits type-size) (-> asm-state data-mem))
	 t))
      ;; Pseudoinstructions
      ;; nop => addi x0, x0, 0
      ((eql instr-name 'nop)
       (assert (= (length instr-args) 0) (instr-name instr-args)
	       "nop takes no arguments")
       (encode-instr asm-state '(addi x0 x0 0)))
      ;; mv rd, rs => addi rd, rs, 0
      ((eql instr-name 'mv)
       (assert (= (length instr-args) 2) (instr-name instr-args)
	       "mv takes 2 arguments: rd rs")
       (encode-instr asm-state (list 'addi (first instr-args) (second instr-args) 0)))
      ;; li rd, imm => addi rd, x0, imm  (for -2048 <= imm <= 2047)
      ;;            => lui rd, upper20 + addi rd, rd, lower12  (otherwise)
      ((eql instr-name 'li)
       (assert (= (length instr-args) 2) (instr-name instr-args)
	       "li takes 2 arguments: rd imm")
       (let* ((rd (first instr-args))
	      (imm (second instr-args))
	      (imm-val (if (typep imm '(or sl-uint sl-int))
			   (sl-uint-value (uint-cast imm :bits 32))
			   imm)))
	 (if (and (sl>= imm-val -2048) (sl<= imm-val 2047))
	     ;; Small immediate: single addi
	     (encode-instr asm-state (list 'addi rd 'x0 imm-val))
	     ;; Large immediate: lui + addi
	     (let* ((lower (sl-logand imm-val #xFFF))
		    (lower-val (sl-uint-value (uint-cast lower :bits 12)))
		    (lower-signed (if (sl>= lower-val #x800)
				     (sl- lower-val #x1000)
				     lower-val))
		    (upper (sl-logand (ash (sl+ imm-val (if (sl>= lower-val #x800) #x1000 0)) -12)
				      #xFFFFF)))
	       (encode-instr asm-state (list 'lui rd upper))
	       (encode-instr asm-state (list 'addi rd rd lower-signed))))))
      ;; j offset => jal x0, offset
      ((eql instr-name 'j)
       (assert (= (length instr-args) 1) (instr-name instr-args)
	       "j takes 1 argument: offset")
       (encode-instr asm-state (list 'jal 'x0 (first instr-args))))
      ;; ret => jalr x0, ra, 0
      ((eql instr-name 'ret)
       (assert (= (length instr-args) 0) (instr-name instr-args)
	       "ret takes no arguments")
       (encode-instr asm-state '(jalr x0 ra 0)))
      ;; Invalid statement
      (t (error "Invalid asm statement: ~a" asm-instr)))))

(defmethod encode-program ((asm-state rv32i-asm-state) (asm-prog list))
  (loop for stmt in asm-prog do
	(encode-instr asm-state stmt)))
