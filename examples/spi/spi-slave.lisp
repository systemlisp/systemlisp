(in-package :system-lisp-examples)

(defclass spi-slave-driver (sl-component)
  ((intf :accessor intf :initarg :intf :type spi-slave-interface)
   (cfg :accessor cfg :initarg :cfg :type spi-config)
   (state :accessor state :initform 'idle)
   (curent-byte :accessor current-byte :initform (uint 0 :bits 8))))

(defmethod-res wait-data-sample-event ((self spi-slave-driver))
  (let ((prev-sclk (-> self intf sclk value)))
    (forever
     (sim-wait (posedge (-> self intf ref-clk)))
     (if (and (sl= prev-sclk (sl-lognot (-> self cfg cpol)))
	      (sl= (-> self intf sclk) (-> self cfg cpol)))
	 ;; then
	 (break-loop)
	 ;; else
	 (setf prev-sclk (-> self intf sclk value))))))

(defmethod-res wait-ssn-negedge ((self spi-slave-driver))
  (let ((prev-ssn (-> self intf ssn value)))
    (forever
     (sim-wait (posedge (-> self intf ref-clk)))
     (if (and (sl= prev-ssn 1)
	      (sl= (-> self intf ssn) 0))
	 ;; then
	 (progn
	   (sl-message 'none "Slave got ssn negedge")
	   (break-loop))
	 ;; else
	 (setf prev-ssn (-> self intf ssn value))))))

(defmethod-res wait-ssn-posedge ((self spi-slave-driver))
  (let ((prev-ssn (-> self intf ssn value)))
    (forever
     (sim-wait (posedge (-> self intf ref-clk)))
     (if (and (sl= prev-ssn 0)
	      (sl= (-> self intf ssn) 1))
	 ;; then
	 (break-loop)
	 ;; else
	 (setf prev-ssn (-> self intf ssn value))))))

(defmethod sample-data ((self spi-slave-driver))
  (setf (bit (-> self current-byte) 0) (-> self intf mosi value sl-uint-value))
  (setf (-> self current-byte) (lsh (-> self current-byte) 1)))

(defmethod drive-random-miso ((self spi-slave-driver))
  (let ((miso-val (random 2)))
    (sl-messagef 'none "driving miso-val ~a" miso-val)
    (setb (-> self intf miso) miso-val)))

(defmethod-res main-phase ((self spi-slave-driver))
  (forever
   ;; Idle state - just wait for ssn negedge
   (when (eql (-> self state) 'idle)
     (progn
       (sl-message 'none "Waiting for ssn negedge")
       (pcall #'wait-ssn-negedge self)
       (sl-message 'none "Done waiting for ssn negedge")
       (setf (-> self state) 'sampling)))
   ;; Sampling state - sample data on sclk edge and send back a radom response
   (if (eql (-> self state) 'sampling)
       (progn
	 (fork-join-first
	   ;; Sample data
	   (lambda-res ()
	     (sl-message 'none "Sampling data")
	     (forever
	      (sl-message 'none "Waiting sampling event")
	      (pcall #'wait-data-sample-event self)
	      (sl-message 'none "Got sampling event")
	      (sample-data self)
	      (drive-random-miso self)))
	   ;; Wait for sn deassert
	   (lambda-res ()
	     (sl-message 'none "Waiting for ssn posedge")
	     (pcall #'wait-ssn-posedge self)
	     (sl-message 'none "Got an ssn posedge")))
	 (setf (-> self state) 'idle))
       ;;else  
       (error "Invalid state"))))

(defmethod run ((self spi-slave-driver))
  (spawn #'main-phase self))

;;; SPI slave monitor
(defclass spi-slave-monitor (sl-component)
  ((intf :accessor intf :initarg :intf :type spi-slave-interface)
   (cfg :accessor cfg :initarg :cfg :type spi-config)
   (ap :accessor ap :type sl-analysis-port)))

(defmethod build ((self spi-slave-monitor))
  (setf (-> self ap) (make-instance 'sl-analysis-port :parent self :name "ap")))

(defmethod-res collect-transactions ((self spi-slave-monitor))
  (let ((trans nil))
    (forever
      ;; Wait for slave select
      (sl-message 'none "spi-slave-monitor: Waiting for slave select to go low")
      (pcall relative-edge-wait 'negedge (-> self intf ssn) (posedge (-> self intf ref-clk)))
      
      (let ((sampling-edge (get-sampling-edge (-> self cfg)))
	    (got-stop-cond nil))

	(sl-message 'none "spi-slave-monitor: Slave has been selected")

	;; Create new transaction
	(setf trans (make-instance 'spi-generic-transaction))

	(forever
	  ;; Wait for sampling edge
	  (sl-message 'none "spi-slave-monitor: Waiting sampling edge on slave")
	  (pcallr got-stop-cond
		  relative-edge-wait sampling-edge
		  (-> self intf sclk) (posedge (-> self intf ref-clk))
		  :stop-condition (lambda () (not (sl= 0 (-> self intf ssn value)))))
	  (setf got-stop-cond (not got-stop-cond))
	  ;; Check for stop condition
	  (sl-messagef 'none "spi-slave-monitor: Got sampling edge on slave with stop condition ~a" (if got-stop-cond "true" "false"))
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
	(sl-message 'none "spi-slave-monitor: Broadcasting transaction")
	(sl-write (-> self ap) trans)))))

(defmethod-res main-phase ((self spi-slave-monitor))
  (forever
   ;; Wait for exit from reset
   (sim-wait (negedge (-> self intf reset)))
    (fork-join-first
      ;; Collect transactions
      (lambda-res () (pcall collect-transactions self))
      ;; Wait for reset
      (lambda-res () (sim-wait (posedge (-> self intf reset)))))))

(defmethod run ((self spi-slave-monitor))
  (spawn #'main-phase self))
