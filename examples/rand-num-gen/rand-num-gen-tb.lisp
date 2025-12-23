(in-package :system-lisp-examples)

(defclass rand-num-gen-tb (sl-component)
  ((dut :accessor dut
	:initform nil)

   (clk :accessor clk
	:initform (make-instance 'sl-signal-binary :bit-width 1))
   (reset :accessor reset
	  :initform (make-instance 'sl-signal-binary :bit-width 1))
   (enable :accessor enable
	   :initform (make-instance 'sl-signal-binary :bit-width 1))
   (data-out :accessor data-out
	     :initform (make-instance 'sl-signal-binary :bit-width 10))))

(defmethod build ((self rand-num-gen-tb))
  (setf (dut self) (create-component rand-num-gen self)))

(defmethod connect ((self rand-num-gen-tb))
  (connect-driver-load (clk self) (clk (dut self)))
  (connect-driver-load (reset self) (reset (dut self)))
  (connect-driver-load (enable self) (enable (dut self)))
  (connect-load-driver (data-out self) (data-out (dut self))))

(defmethod-res rand-num-gen-clk-gen ((self rand-num-gen-tb))
  (setb (clk self) 0)
  (forever (sim-delay 5)
	   (setb (clk self) (sl-lognot (clk self)))))

(defmethod-res simple-rand-num-gen-test ((self rand-num-gen-tb))
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

  (repeat 100 (sim-wait (posedge (clk self))))

  (sim-finish))

(defmethod run ((self rand-num-gen-tb))
  (spawn #'rand-num-gen-clk-gen self)
  (spawn #'simple-rand-num-gen-test self))

;; Run the simulation
(defun rand-num-gen-run-sim ()
  (let ((*sim* (make-instance 'sl-sim))
	(tb-inst (create-component rand-num-gen-tb nil)))
    (reset-sim)
    (load-component tb-inst)
    (elaborate)
    (vcd-trace-component tb-inst "top")
    (run 50000)))
