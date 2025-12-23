(in-package :system-lisp-examples)

(defclass spi-test-1master-1slave (sl-component)
  (;; Components
   (master-driver :accessor master-driver :type spi-master-driver)
   (master-monitor :accessor master-monitor :type spi-master-monitor)
   (slave-driver :accessor slave-driver :type spi-slave-driver)
   (slave-monitor :accessor slave-monitor :type spi-slave-monitor)
   (master-intf :accessor master-intf :type spi-master-interface)
   (slave-intf :accessor slave-intf :type spi-slave-interface)
   (cfg :accessor cfg :type spi-config)
   ;; Clock and reset
   (ref-clk :accessor ref-clk :initform (make-instance 'sl-signal-binary :bit-width 1))
   (reset :accessor reset :initform (make-instance 'sl-signal-binary :bit-width 1))
   ;; Wires
   (ssn :accessor ssn :initform (make-instance 'sl-signal-binary-lambda :bit-width 1))))

(defmethod build ((self spi-test-1master-1slave))
  (sl-message 'none "Running build")
  ;; Create components
  (setf (-> self cfg) (make-instance 'spi-config :cpol 1 :cpha 0))
  (setf (-> self master-driver) (create-component spi-master-driver self))
  (setf (-> self master-monitor) (create-component spi-master-monitor self))
  (setf (-> self slave-driver) (create-component spi-slave-driver self))
  (setf (-> self slave-monitor) (create-component spi-slave-monitor self))
  (setf (-> self master-intf) (create-component spi-master-interface self :nr-slaves 1))
  (setf (-> self slave-intf) (create-component spi-slave-interface self))
  ;; Create config
  (setf (-> self cfg) (make-instance 'spi-config))  
  ;; Pointer connections
  (setf (-> self master-driver intf) (-> self master-intf))
  (setf (-> self master-monitor intf) (-> self master-intf))
  (setf (-> self slave-driver intf) (-> self slave-intf))
  (setf (-> self slave-monitor intf) (-> self slave-intf))
  (setf (-> self master-driver cfg) (-> self cfg))
  (setf (-> self master-monitor cfg) (-> self cfg))
  (setf (-> self slave-driver cfg) (-> self cfg))
  (setf (-> self slave-monitor cfg) (-> self cfg)))

(defmethod connect ((self spi-test-1master-1slave))
  (sl-message 'none "Running connect")
  ;; Signal connections
  (connect-driver-load (-> self ref-clk) (-> self master-intf ref-clk))
  (connect-driver-load (-> self ref-clk) (-> self slave-intf ref-clk))
  (connect-driver-load (-> self reset) (-> self master-intf reset))
  (connect-driver-load (-> self reset) (-> self slave-intf reset))
  (connect-driver-load (-> self master-intf mosi) (-> self slave-intf mosi))
  (connect-driver-load (-> self slave-intf miso) (-> self master-intf miso))
  (connect-driver-load (-> self master-intf sclk) (-> self slave-intf sclk))
  ;;(connect-driver-load (bit (-> self master-intf ssn) 0) (-> self slave-intf ssn)) ;; TODO: how could we make this work?
  (connect-comb (-> self ssn) (lambda () (bit (-> self master-intf ssn value) 0)) (vector (-> self master-intf ssn)))
  (connect-driver-load (-> self ssn) (-> self slave-intf ssn)))

(defmethod-res ref-clk-gen ((self spi-test-1master-1slave))
  (setb (-> self ref-clk) 0)
  (forever
   (sim-delay 5)
   (setnb (-> self ref-clk) (sl-lognot (-> self ref-clk)))))

(defmethod-res reset-gen ((self spi-test-1master-1slave))
  (setnb (-> self reset) 0)
  (repeat 5 (sim-wait (posedge (-> self ref-clk))))
  (setnb (-> self reset) 1)
  (repeat 5 (sim-wait (posedge (-> self ref-clk))))
  (setnb (-> self reset) 0))

(defmethod-res main-phase ((self spi-test-1master-1slave))
  (sl-message 'none "Wait for reset negedge")
  (sim-wait (negedge (-> self reset)))
  (sl-message 'none "Got reset negedge")
  
  (sl-message 'none "Resetting signals")
  (reset-signals (-> self master-driver))
  (sl-message 'none "Done resetting signals")

  (sl-message 'none "Wait 5 ref-clk cycles")
  (repeat 5 (sim-wait (posedge (-> self ref-clk))))
  (sl-message 'none "Done waiting 5 ref-clk cycles")

  (sl-message 'none "Driving transaction")
  (let ((trans (make-instance 'spi-generic-transaction :slave-index 0 :mosi-bits #(1 1 0 0 1 0 1 1))))
    (pcall #'drive-transaction (-> self master-driver) trans))
  (sl-message 'none "Done driving transaction.")

  (sl-message 'none "Wait 10 ref-clk cycles")
  (repeat 10 (sim-wait (posedge (-> self ref-clk))))
  (sl-message 'none "Done waiting 5 ref-clk cycles")
  
  (sl-message 'none "Driving transaction")
  (let ((trans (make-instance 'spi-generic-transaction :slave-index 0 :mosi-bits #(0 0 0 1 1 0 1 0))))
    (pcall #'drive-transaction (-> self master-driver) trans))
  (sl-message 'none "Done driving transaction.")
  
  (repeat 100 (sim-wait (posedge (-> self ref-clk))))

  (sim-finish))

(defmethod run ((self spi-test-1master-1slave))
  (spawn #'ref-clk-gen self)
  (spawn #'reset-gen self)
  (spawn #'main-phase self))

;; Run the simulation
(defun run-spi-test-1master-1slave-sim ()
  (let ((*sim* (make-instance 'sl-sim))
	(tb-inst (create-component spi-test-1master-1slave nil)))
    (reset-sim)
    (load-component tb-inst)
    (sl-message 'none "Running elaborate")
    (elaborate)
    (sl-message 'none "Done running elaborate")
    (vcd-trace-component tb-inst "top")
    (run 50000)))

