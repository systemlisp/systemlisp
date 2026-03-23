(in-package :system-lisp-examples)

;;; Top-level component: FemtoRV32 + DRAM behavioral model
(defclass femtorv32-dram-top (sl-component)
  (;; Child components
   (cpu :accessor cpu :type femtorv32)
   (dram :accessor dram :type dram-beh)
   ;; Top-level signals
   (clk :accessor clk
    :initform (make-instance 'sl-signal-binary :bit-width 1))
   (reset :accessor reset
    :initform (make-instance 'sl-signal-binary :bit-width 1))))

(defmethod build ((self femtorv32-dram-top))
  (setf (-> self cpu) (create-component femtorv32 self))
  (setf (-> self dram) (create-component dram-beh self)))

(defmethod connect ((self femtorv32-dram-top))
  ;; Clock and reset to both children
  (connect-driver-load (-> self clk) (-> self cpu clk))
  (connect-driver-load (-> self clk) (-> self dram clk))
  (connect-driver-load (-> self reset) (-> self cpu reset))
  (connect-driver-load (-> self reset) (-> self dram reset))
  ;; CPU outputs -> DRAM inputs
  (connect-driver-load (-> self cpu mem-addr) (-> self dram mem-addr))
  (connect-driver-load (-> self cpu mem-wdata) (-> self dram mem-wdata))
  (connect-driver-load (-> self cpu mem-wmask) (-> self dram mem-wmask))
  (connect-driver-load (-> self cpu mem-rstrb) (-> self dram mem-rstrb))
  ;; DRAM outputs -> CPU inputs
  (connect-driver-load (-> self dram mem-rdata) (-> self cpu mem-rdata))
  (connect-driver-load (-> self dram mem-rbusy) (-> self cpu mem-rbusy))
  (connect-driver-load (-> self dram mem-wbusy) (-> self cpu mem-wbusy)))

(defmethod-res clk-gen ((self femtorv32-dram-top))
  (setb (-> self clk) 0)
  (forever
    (sim-delay 5)
    (setb (-> self clk) (sl-lognot (-> self clk)))))

(defmethod-res reset-sequence ((self femtorv32-dram-top))
  ;; Assert reset
  ;; (setb (-> self reset) 1)
  ;; (repeat 5 (sim-wait (posedge (-> self clk))))
  ;; Deassert reset
  (setb (-> self reset) 0)
  (repeat 5 (sim-wait (posedge (-> self clk))))
  ;; Re-assert reset (active high for femtorv32)
  (setb (-> self reset) 1))

(defmethod run ((self femtorv32-dram-top))
  (spawn #'clk-gen self)
  (spawn #'reset-sequence self))

;;; Simple RV32I test programs
(defparameter *lui-test-program*
  '((lui x5 #x12345)))

(defparameter *dram-tb-program*
  '(;; Simple arithmetic test
    (li t0 1)				; t0 = 1
    (li t1 2)				; t1 = 2
    (add t2 t0 t1)			; t2 = t0 + t1 = 3
    (add t3 t2 t1)			; t3 = t2 + t1 = 5
    (addi t4 t3 10)			; t4 = t3 + 10 = 15
    (sub t5 t4 t0)			; t5 = t4 - t0 = 14
    (nop)
    (nop)
    (nop)))

;;; Load assembled program and data into DRAM memory
(defun load-program-into-dram (dram-component asm-state)
  "Load prog-mem at word address 0, data-mem at word address (data0/4)."
  (let ((mem (mem dram-component)))
    ;; Load program instructions (word-addressed, starting at 0)
    (loop for i from 0 below (length (prog-mem asm-state)) do
      (sl-memory-write mem i (aref (prog-mem asm-state) i)))
    ;; Load data (word-addressed, starting at data0/4)
    (let ((data-base-word (ash (data0 asm-state) -2)))
      (loop for i from 0 below (length (data-mem asm-state)) do
	(sl-memory-write mem (+ data-base-word i) (aref (data-mem asm-state) i))))))

(defvar *asm-state* (make-instance 'rv32i-asm-state))

;;; Run the simulation
(defun run-femtorv32-dram-tb (dram-tb-program time &optional (finalize (lambda ())))
  ;; Assemble the program
  (let ((asm-state (make-instance 'rv32i-asm-state)))
    (reset-asm-state asm-state)
    (encode-program asm-state dram-tb-program)
    ;; Create simulation
    (let* ((*sim* (make-instance 'sl-sim))
	   (top (create-component femtorv32-dram-top nil)))
      (reset-sim)
      (load-component top)
      (elaborate)
      ;; Load program and data into DRAM after elaboration
      (load-program-into-dram (dram top) asm-state)
      ;; Trace and run
      (vcd-trace-component top "top")
      (run time)
      (funcall finalize))))
