(in-package :system-lisp-examples)

;; DUT
(defclass d-flip-flop (sl-component)
  ((clk
    :accessor clk
    :initform (make-instance 'sl-signal-binary :bit-width 1))
   (reset
    :accessor reset
    :initform (make-instance 'sl-signal-binary :bit-width 1))
   (in-data
    :accessor in-data
    :initform (make-instance 'sl-signal-binary :bit-width 1))
   (out-data
    :accessor out-data
    :initform (make-instance 'sl-signal-binary :bit-width 1))))

(defmethod-res d-flip-flop-logic ((self d-flip-flop))
  (forever
   (sim-wait (posedge (clk self)))
   (if (sl= (value (reset self)) 1)
       (setnb (out-data self) 0)
       ;; else
       (setnb (out-data self) (in-data self)))))

(defmethod run ((self d-flip-flop))
  (spawn #'d-flip-flop-logic self))

;; TB
(defclass d-flip-flop-tb (sl-component)
  ;; TB signals
  ((clk
    :accessor clk
    :initform (make-instance 'sl-signal-binary :bit-width 1))
   (reset
    :accessor reset
    :initform (make-instance 'sl-signal-binary :bit-width 1))
   (in-data
    :accessor in-data
    :initform (make-instance 'sl-signal-binary :bit-width 1))
   (out-data
    :accessor out-data
    :initform (make-instance 'sl-signal-binary :bit-width 1))
   ;; DUT
   (dut
       :accessor dut
       :initform nil)))

(defmethod build ((self d-flip-flop-tb))
  (setf (dut self) (create-component d-flip-flop self)))

(defmethod connect ((self d-flip-flop-tb))
  (connect-driver-load (clk self) (clk (dut self)))
  (connect-driver-load (reset self) (reset (dut self)))
  (connect-driver-load (in-data self) (in-data (dut self)))
  (connect-load-driver (out-data self) (out-data (dut self))))

;; Generate clock signal
(defmethod-res clk-generate ((self d-flip-flop-tb))
  (setb (clk self) 0)
  (forever
   (sim-delay 5)
   (setb (clk self)
	 (sl-lognot (value (clk self))))))

;; Drive DUT input(s)
(defmethod-res simple-test ((self d-flip-flop-tb))
  (setnb (reset (dut self)) 0)
  (setnb (in-data (dut self)) 0)

  (repeat 2 (sim-wait (posedge (clk self))))
  (setnb (in-data self) 1)
	
  (repeat 3 (sim-wait (posedge (clk self))))
  (setnb (in-data self) 0)
  
  (repeat 3 (sim-wait (posedge (clk self))))
  (setnb (in-data self) 1)

  (repeat 5 (sim-wait (posedge (clk self))))
  (setnb (reset self) 1)
  (sim-wait (posedge (clk self)))
  (setnb (reset self) 0)

  (repeat 5 (sim-wait (posedge (clk self))))
  (sim-finish))

(defmethod run ((self d-flip-flop-tb))
  (spawn #'clk-generate self)
  (spawn #'simple-test self))

;; Run the simulation
(defun d-flip-flop-run-sim ()
  (let ((*sim* (make-instance 'sl-sim))
	(tb-inst (create-component d-flip-flop-tb nil)))
    (reset-sim)
    (load-component tb-inst)
    (elaborate)
    (vcd-trace-component tb-inst "top")
    (run 500)))
