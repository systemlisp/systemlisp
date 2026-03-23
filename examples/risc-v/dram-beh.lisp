(in-package :system-lisp-examples)

;; DRAM behavioral component
(defclass dram-beh (sl-component)
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
    :initform (make-instance 'sl-signal-binary :bit-width 32 :dir 'input))
   (mem-wdata
    :accessor mem-wdata
    :initform (make-instance 'sl-signal-binary :bit-width 32 :dir 'input))
   (mem-wmask
    :accessor mem-wmask
    :initform (make-instance 'sl-signal-binary :bit-width 4 :dir 'input))
   (mem-rdata
    :accessor mem-rdata
    :initform (make-instance 'sl-signal-binary :bit-width 32 :dir 'output))
   (mem-rstrb
    :accessor mem-rstrb
    :initform (make-instance 'sl-signal-binary :bit-width 1 :dir 'input))
   (mem-rbusy
    :accessor mem-rbusy
    :initform (make-instance 'sl-signal-binary :bit-width 1 :dir 'output))
   (mem-wbusy
    :accessor mem-wbusy
    :initform (make-instance 'sl-signal-binary :bit-width 1 :dir 'output))
   ;; Memory model
   (mem
    :accessor mem
    :initform (make-instance 'sl-memory :data-bits 32 :addr-bits 32 :word-count (ash 1 24)))
   ;; State machine
   (state
    :accessor state
    :initform (make-instance 'sl-signal-binary :bit-width 2))))

(defconstant +wait-for-req+ 0)
(defconstant +serve-read-req+ 1)
(defconstant +serve-write-req+ 2)

(defun byte-enable-to-bit-mask (be)
  "Convert a 4-bit byte enable mask to a 32-bit bit mask.
   Each set bit in BE enables the corresponding byte (8 bits) in the result."
  (loop for i from 0 to 3
	when (logbitp i be)
	  sum (ash #xFF (* i 8))))

(defmethod-res mem-logic ((self dram-beh))
  (forever
    (sim-wait (posedge (-> self clk)))
    (when (sl/= 0 (-> self reset value))
      (when (sl= (-> self state value) +wait-for-req+)
	(if (sl= (-> self mem-rstrb value) 1)
	    ;; Read req
	    (progn
	      (setnb (-> self mem-rbusy) 1)
	      (setnb (-> self state) +serve-read-req+)
	      (setnb (-> self mem-rdata)
		     (sl-memory-read (-> self mem)
				     (ash (-> self mem-addr value sl-uint-value) -2))))
	    ;; else
	    ;; Write req
	    (when (sl/= 0 (-> self mem-wmask value))
	      (setnb (-> self mem-wbusy) 1)
	      (setnb (-> self state) +serve-write-req+)
	      (let* ((addr (ash (-> self mem-addr value sl-uint-value) -2))
		     (old-val (sl-memory-read (-> self mem) addr))
		     (bit-mask (byte-enable-to-bit-mask (-> self mem-wmask value sl-uint-value))))
		(sl-memory-write (-> self mem) addr
				 (logior (logand old-val (lognot bit-mask))
					 (logand (-> self mem-wdata value sl-uint-value) bit-mask)))))))
      
      (when (sl= (-> self state) +serve-read-req+)
	(setnb (-> self mem-rbusy) 0)
	(setnb (-> self state) +wait-for-req+))

      (when (sl= (-> self state) +serve-write-req+)
	(setnb (-> self mem-wbusy) 0)
	(setnb (-> self state) +wait-for-req+)))))

(defmethod run ((self dram-beh))
  (spawn #'mem-logic self))
