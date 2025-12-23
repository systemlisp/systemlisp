(in-package :system-lisp-examples)

(defclass counter (sl-component)
  ((clk :accessor clk
	:initform (make-instance 'sl-signal-binary :bit-width 1))
   (reset :accessor reset
	  :initform (make-instance 'sl-signal-binary :bit-width 1))
   (enable :accessor enable
	   :initform (make-instance 'sl-signal-binary :bit-width 1))
   (data-out :accessor data-out
	     :initform (make-instance 'sl-signal-binary :bit-width 8 :reset-value 0))))

(defmethod-res counter-logic ((self counter))
  (forever
   (sim-wait (posedge (clk self)))
   (if (sl= (value (reset self)) 1)
       (setnb (data-out self) 0)
       (when (sl= (enable self) 1)
	 (setnb (data-out self) (sl1+ (data-out self)))))))

(defmethod run ((self counter))
  (spawn #'counter-logic self))
