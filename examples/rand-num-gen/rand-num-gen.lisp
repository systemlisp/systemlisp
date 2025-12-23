(in-package :system-lisp-examples)

(defclass rand-num-gen (sl-component)
  ;; Ports
  ((clk :accessor clk
	:initform (make-instance 'sl-signal-binary :bit-width 1))
   (reset :accessor reset
	  :initform (make-instance 'sl-signal-binary :bit-width 1))
   (enable :accessor enable
	   :initform (make-instance 'sl-signal-binary :bit-width 1))
   (data-out :accessor data-out
	     :initform (make-instance 'sl-signal-binary-lambda :bit-width 10 :value 0))

   ;; Internal signals and registers
   (r-reg :accessor r-reg
	  :initform (make-instance 'sl-signal-binary :bit-width 10 :value 0))
   (r-next :accessor r-next
	   :initform (make-instance 'sl-signal-binary-lambda :bit-width 10 :value 0))
   (feedback-value :accessor feedback-value
		   :initform (make-instance 'sl-signal-binary-lambda :bit-width 1 :value 0))))

;;; Combinatorial logic 
(defmethod connect ((self rand-num-gen))
  ;; assign feeback_value = r_reg[9] ^ r_reg[5] ^ r_reg[0];
  (connect-comb (-> self feedback-value)
		(lambda ()
		  (sl-logxor (bit (-> self r-reg value) 9)
			     (bit (-> self r-reg value) 5)
			     (bit (-> self r-reg value) 0)))
		
		(vector (-> self r-reg)))

  ;; assign r_next = %{feedback_value, r_reg[9:1]};
  (connect-comb (-> self r-next)
		(lambda ()
		  (concat (-> self feedback-value value)
			  (bits (-> self r-reg value) 9 1)))
		
		(vector
		 (-> self feedback-value)
		 (-> self r-reg)))

  ;; assign data_out = r_reg
  ;; Alternative: declare data-out as sl-signal-binary and (connect-driver-load (-> self r-reg) (-> self data-out))
  (connect-comb (-> self data-out)
		(lambda () (-> self r-reg value))

		(vector (-> self r-reg))))

;;; Sequential logic
(defmethod-res rand-num-gen-seq-logic ((self rand-num-gen))
  (forever
   (sim-wait (posedge (clk self)))
   (if (sl= 1 (reset self))
       (setnb (r-reg self) 1)
       ;; else
       (when (sl= 1 (enable self))
	 (setnb (r-reg self) (r-next self))))))

(defmethod run ((self rand-num-gen))
  (spawn #'rand-num-gen-seq-logic self))
