(in-package :system-lisp-examples)

(defclass spi-master-driver (sl-component)
  ((intf :accessor intf :initarg :intf :type spi-master-interface)
   (cfg :accessor cfg :initarg :cfg :type spi-config)))

(defmethod reset-signals ((me spi-master-driver))
  (setnb (-> me intf sclk) 0)
  (setnb (-> me intf mosi) 0)
  (setnb (-> me intf miso) 0)
  (loop for i from 0 to (1- (-> me intf nr-slaves)) do
    (sl-messagef 'none "Resetting ssn ~a to 1" i)
    (setnb (bit (-> me intf ssn) i) 1)))

(defmethod-res drive-transaction ((self spi-master-driver) (trans spi-generic-transaction))
  ;; Drive slave select
  (setnb (bit (-> self intf ssn) (-> trans slave-index)) 0)

  ;; Initialize sclk to idle
  (setnb (-> self intf sclk) (-> self cfg cpol))

  ;; Wait initial delay
  (repeat (-> self cfg wait-bef-start)
	  (sim-wait (posedge (-> self intf ref-clk))))

  ;; Drive mosi bits
  (let ((mosi-bit-index 0)
	(nr-mosi-bits (-> trans mosi-bits length)))
    (c-for ((setf mosi-bit-index 0) (sl< mosi-bit-index nr-mosi-bits) (incf mosi-bit-index))
	   (if (sl= (-> self cfg cpha) 1)
	       (progn
		 (repeat (-> self cfg sclk-half-period)
			 (sim-wait (posedge (-> self intf ref-clk))))
		 
		 (setnb (-> self intf sclk) (sl-lognot (-> self intf sclk value)))

		 (setnb (-> self intf mosi)
			(aref (-> trans mosi-bits) mosi-bit-index))

		 (repeat (-> self cfg sclk-half-period)
			 (sim-wait (posedge (-> self intf ref-clk))))

		 (setnb (-> self intf sclk) (sl-lognot (-> self intf sclk value))))
	       
	       ;; else
	       (progn
		 (setnb (-> self intf mosi)
			(aref (-> trans mosi-bits) mosi-bit-index))

		 (repeat (-> self cfg sclk-half-period)
			 (sim-wait (posedge (-> self intf ref-clk))))

		 (setnb (-> self intf sclk) (sl-lognot (-> self intf sclk value)))

		 (repeat (-> self cfg sclk-half-period)
			 (sim-wait (posedge (-> self intf ref-clk))))

		 (setnb (-> self intf sclk) (sl-lognot (-> self intf sclk value)))))))

  ;; Drive one last half-period for cpha=0
  (when (sl= (-> self cfg cpha) 0)
    (repeat (-> self cfg sclk-half-period)
			 (sim-wait (posedge (-> self intf ref-clk)))))
  ;; Deselect slave
  (setnb (bit (-> self intf ssn) (-> trans slave-index)) 1)

  ;; Bring sclk back to idle
  (setnb (-> self intf sclk) (-> self cfg cpol)))

;;; Master monitor
(defclass spi-master-monitor (sl-component)
  ((intf :accessor intf :initarg :intf :type spi-master-interface)
   (cfg :accessor cfg :initarg :cfg :type spi-config)
   (ap :accessor ap :type sl-analysis-port)))

(defmethod build ((self spi-master-monitor))
  (setf (-> self ap) (make-instance 'sl-analysis-port :parent self :name "ap")))

(defmethod get-active-slave ((ssn sl-uint))
  (assert (sl<= (one-count (sl-lognot ssn)) 1) () "[~a] Error: only one slave can be selected ssn= ~a" (time-now *sim*) ssn)
  (let ((slave-index nil))
    (loop for i from 0 to (1- (sl-uint-bits ssn)) do
      (when (= (bit ssn i) 0)
	(setf slave-index i)
	(return)))
    slave-index))

(defmethod-res collect-transactions ((self spi-master-monitor))
  (let ((trans nil))
    (forever
      ;; Wait for slave select
      (sl-message 'none "spi-master-monitor: Waiting for slave select to go low")
      (pcall relative-edge-wait 'edge (-> self intf ssn) (posedge (-> self intf ref-clk)))
      
      (let ((slave-index (get-active-slave (-> self intf ssn value)))
	    (sampling-edge (get-sampling-edge (-> self cfg)))
	    (driving-edge (get-driving-edge (-> self cfg)))
	    (got-stop-cond nil))

	(when (not slave-index) (continue-loop))

	(sl-messagef 'none "spi-master-monitor: Slave ~a has been selected" slave-index)

	;; Create new transaction
	(setf trans (make-instance 'spi-generic-transaction :slave-index slave-index))

	(forever
	  ;; Wait for sampling edge
	  (sl-messagef 'none "spi-master-monitor: Waiting sampling edge on slave ~a" slave-index)
	  (pcallr got-stop-cond
		  relative-edge-wait sampling-edge
		  (-> self intf sclk) (posedge (-> self intf ref-clk))
		  :stop-condition (lambda () (not (sl= 0 (bit (-> self intf ssn value) slave-index)))))
	  (setf got-stop-cond (not got-stop-cond))
	  ;; Check for stop condition
	  (sl-messagef 'none "spi-master-monitor: Got sampling edge on slave ~a with stop condition ~a" slave-index (if got-stop-cond "true" "false"))
	  (when got-stop-cond
	    ;; Sample last bit when cpha == 0
	    (when (sl= (-> self cfg cpha) 0)
	      (vector-push-extend (-> self intf miso) (-> trans miso-bits))
	      (vector-push-extend (-> self intf mosi) (-> trans mosi-bits)))
	    (break-loop))
	  ;; Sample mosi and miso
	  (vector-push-extend (-> self intf miso) (-> trans miso-bits))
	  (vector-push-extend (-> self intf mosi) (-> trans mosi-bits)))

	;; Broadcast collected transaction to subscribers
	(sl-message 'none "spi-master-monitor: Broadcasting transaction")
	(sl-write (-> self ap) trans)))))

(defmethod-res main-phase ((self spi-master-monitor))
  (forever
   ;; Wait for exit from reset
   (sim-wait (negedge (-> self intf reset)))
    (fork-join-first
      ;; Collect transactions
      (lambda-res () (pcall collect-transactions self))
      ;; Wait for reset
      (lambda-res () (sim-wait (posedge (-> self intf reset)))))))

(defmethod run ((self spi-master-monitor))
  (spawn #'main-phase self))

