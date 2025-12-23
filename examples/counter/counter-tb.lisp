(in-package :system-lisp-examples)

(defclass counter-tb (sl-component)
  ((dut :accessor dut
	:initform nil)

   (clk :accessor clk
	:initform (make-instance 'sl-signal-binary :bit-width 1))
   (reset :accessor reset
	  :initform (make-instance 'sl-signal-binary :bit-width 1))
   (enable :accessor enable
	   :initform (make-instance 'sl-signal-binary :bit-width 1))
   (data-out :accessor data-out
	     :initform (make-instance 'sl-signal-binary :bit-width 8))))

(defmethod build ((self counter-tb))
  (setf (dut self) (create-component counter self)))

(defmethod connect ((self counter-tb))
  (connect-driver-load (clk self) (clk (dut self)))
  (connect-driver-load (reset self) (reset (dut self)))
  (connect-driver-load (enable self) (enable (dut self)))
  (connect-load-driver (data-out self) (data-out (dut self))))

(defmethod-res counter-clk-gen ((self counter-tb))
  (setb (clk self) 0)
  (forever (sim-delay 5)
	   (setb (clk self) (sl-lognot (clk self)))))

(defmethod-res simple-counter-test ((self counter-tb))
  (setnb (enable self) 0)
  (setnb (reset self) 0)

  (repeat 3 (sim-wait (posedge (clk self))))
  (setnb (enable self) 1)

  (repeat 5 (sim-wait (posedge (clk self))))

  (setnb (reset self) 1)
  (sim-wait (posedge (clk self)))
  (setnb (reset self) 0)

  (repeat 5 (sim-wait (posedge (clk self))))

  (setnb (enable self) 0)
  (repeat 5 (sim-wait (posedge (clk self))))
  (setnb (enable self) 1)

  (repeat 10 (sim-wait (posedge (clk self))))

  (sim-finish))

(defmethod run ((self counter-tb))
  (spawn #'counter-clk-gen self)
  (spawn #'simple-counter-test self))



