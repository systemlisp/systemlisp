(in-package :system-lisp-test)

(def-suite* sl-port-test
  :description "Tests for TLM ports"
  :in sl-main-test)

;;; Single port connection single trans type
(defclass port-test1-source (sl-component)
  ((ap :accessor ap :type sl-analysis-port)))

(defclass port-test1-dest (sl-component)
  ((imp :accessor imp :type sl-analysis-imp)
   (trans-count :accessor trans-count :initform 0)))

(defmethod build ((self port-test1-source))
  (setf (ap self) (make-instance 'sl-analysis-port :parent self :name "ap")))

(defmethod build ((self port-test1-dest))
  (setf (imp self) (make-instance 'sl-analysis-imp :parent self :name "imp")))

(defclass port-test1-trans ()
  ((content :accessor content :initarg :content :initform 0)))

(defparameter *port-test1-sb* (make-instance 'sl-fifo-ll))

(defmethod sl-write-imp ((port sl-analysis-imp) (comp port-test1-dest) (data port-test1-trans))
  (sl-messagef 'none "Got one: ~a" (content data))
  (incf (-> comp trans-count))
  (let ((exp-trans (fifo-pop *port-test1-sb*)))
    (is (= (content exp-trans) (content data)))))

(defmethod-res main-phase ((self port-test1-source))
  (sl-message 'none "Starting simulation")
  (sim-delay 10)
  (let ((trans (make-instance 'port-test1-trans :content 5)))
    (sl-messagef 'none "Sending transaction with content ~a" (content trans))
    (fifo-push *port-test1-sb* trans)
    (sl-write (ap self) trans))

  (sim-delay 20)
  (let ((trans (make-instance 'port-test1-trans :content 6)))
    (sl-messagef 'none "Sending transaction with content ~a" (content trans))
    (fifo-push *port-test1-sb* trans)
    (sl-write (ap self) trans))

  (sim-delay 15)
  (let ((trans (make-instance 'port-test1-trans :content 7)))
    (sl-messagef 'none "Sending transaction with content ~a" (content trans))
    (fifo-push *port-test1-sb* trans)
    (sl-write (ap self) trans))

  (sim-finish))

(defmethod sl:run ((self port-test1-source))
  (spawn #'main-phase self))

(test test1-single-conn-single-trans-type
  "Single port connection single trans type"
  (let ((*sim* (make-instance 'sl-sim))
	(source (create-component port-test1-source nil))
	(dest (create-component port-test1-dest nil)))
    (reset-sim)
    (fifo-clear *port-test1-sb*)
    (load-component source)
    (load-component dest)
    (sl-message 'none "Running elaborate")
    (elaborate)
    (connect-port (-> source ap) (-> dest imp))
    (sl-message 'none "Done running elaborate")
    (sl:run 100)

    (is (= 45 (time-now *sim*)))
    (is (= 3 (trans-count dest)))))

;;; Single port connection multiple trans types
(defclass port-test2-source (port-test1-source) ())

(defclass port-test2-dest (port-test1-dest)
  ((trans1-count :accessor trans1-count :initform 0)
   (trans2-count :accessor trans2-count :initform 0)))

(defclass port-test2-trans1 ()
  ((content :accessor content :initarg :content :initform 0)))

(defclass port-test2-trans2 ()
  ((content :accessor content :initarg :content :initform 0)))

(defmethod sl-write-imp ((port sl-analysis-imp) (comp port-test2-dest) (data port-test2-trans1))
  (sl-messagef 'none "Got a port-test2-trans1 one: ~a" (content data))
  (incf (-> comp trans-count))
  (incf (-> comp trans1-count)))

(defmethod sl-write-imp ((port sl-analysis-imp) (comp port-test2-dest) (data port-test2-trans2))
  (sl-messagef 'none "Got a port-test2-trans2 one: ~a" (content data))
  (incf (-> comp trans-count))
  (incf (-> comp trans2-count)))

(defmethod-res main-phase ((self port-test2-source))
  (sl-message 'none "Starting simulation")
  (sim-delay 10)
  (let ((trans (make-instance 'port-test2-trans1 :content 5)))
    (sl-messagef 'none "Sending transaction with content ~a" (content trans))
    (sl-write (ap self) trans))

  (sim-delay 20)
  (let ((trans (make-instance 'port-test2-trans2 :content 6)))
    (sl-messagef 'none "Sending transaction with content ~a" (content trans))
    (sl-write (ap self) trans))

  (sim-delay 15)
  (let ((trans (make-instance 'port-test2-trans1 :content 7)))
    (sl-messagef 'none "Sending transaction with content ~a" (content trans))
    (sl-write (ap self) trans))

  (sim-delay 10)
  (let ((trans (make-instance 'port-test2-trans2 :content 8)))
    (sl-messagef 'none "Sending transaction with content ~a" (content trans))
    (sl-write (ap self) trans))

  (sim-finish))

(test test2-single-conn-multiple-trans-types
  "Single port connection multiple trans types"
  (let ((*sim* (make-instance 'sl-sim))
	(source (create-component port-test2-source nil))
	(dest (create-component port-test2-dest nil)))
    (reset-sim)
    (load-component source)
    (load-component dest)
    (sl-message 'none "Running elaborate")
    (elaborate)
    (connect-port (-> source ap) (-> dest imp))
    (sl-message 'none "Done running elaborate")
    (sl:run 100)

    (is (= 55 (time-now *sim*)))
    (is (= 4 (trans-count dest)))
    (is (= 2 (trans1-count dest)))
    (is (= 2 (trans2-count dest)))))

;;; Multiple port connections multiple trans types
(defclass port-test3-imp-type1 (sl-analysis-imp)
  ((trans-count :accessor trans-count :initform 0)))
(defclass port-test3-imp-type2 (sl-analysis-imp)
  ((trans-count :accessor trans-count :initform 0)))

(defclass port-test3-source (sl-component)
  ((ap1 :accessor ap1 :type sl-analysis-port)
   (ap2 :accessor ap2 :type sl-analysis-port)))

(defclass port-test3-dest (sl-component)
  ((imp1 :accessor imp1 :type port-test3-imp-type1)
   (imp2 :accessor imp2 :type port-test3-imp-type2)))

(defmethod build ((self port-test3-source))
  (setf (-> self ap1) (make-instance 'sl-analysis-port :parent self :name "ap1"))
  (setf (-> self ap2) (make-instance 'sl-analysis-port :parent self :name "ap2")))

(defmethod build ((self port-test3-dest))
  (setf (-> self imp1) (make-instance 'port-test3-imp-type1 :parent self :name "imp1"))
  (setf (-> self imp2) (make-instance 'port-test3-imp-type2 :parent self :name "imp2")))

(defclass port-test3-trans1 ()
  ((content :accessor content :initarg :content :initform 0)))
(defclass port-test3-trans2 ()
  ((content :accessor content :initarg :content :initform 0)))

(defparameter *port-test3-trans1-sb* (make-instance 'sl-fifo-ll))
(defparameter *port-test3-trans2-sb* (make-instance 'sl-fifo-ll))

(defmethod sl-write-imp ((port port-test3-imp-type1) (comp port-test3-dest) (data port-test3-trans1))
  (sl-messagef 'none "imp1: got a port-test3-trans1 with content ~a" (content data))
  (incf (-> port trans-count))
  (let ((exp-trans (fifo-pop *port-test3-trans1-sb*)))
    (is (typep exp-trans 'port-test3-trans1))
    (is (= (content exp-trans) (content data)))))

(defmethod sl-write-imp ((port port-test3-imp-type1) (comp port-test3-dest) (data port-test3-trans2))
  (sl-messagef 'none "imp1: got a port-test3-trans2 with content ~a" (content data))
  (incf (-> port trans-count))
  (let ((exp-trans (fifo-pop *port-test3-trans2-sb*)))
    (is (typep exp-trans 'port-test3-trans2))
    (is (= (content exp-trans) (content data)))))

(defmethod sl-write-imp ((port port-test3-imp-type2) (comp port-test3-dest) (data port-test3-trans1))
  (sl-messagef 'none "imp2: got a port-test3-trans1 with content ~a" (content data))
  (incf (-> port trans-count))
  (let ((exp-trans (fifo-pop *port-test3-trans1-sb*)))
    (is (typep exp-trans 'port-test3-trans1))
    (is (= (content exp-trans) (content data)))))

(defmethod sl-write-imp ((port port-test3-imp-type2) (comp port-test3-dest) (data port-test3-trans2))
  (sl-messagef 'none "imp2: got a port-test3-trans2 with content ~a" (content data))
  (incf (-> port trans-count))
  (let ((exp-trans (fifo-pop *port-test3-trans2-sb*)))
    (is (typep exp-trans 'port-test3-trans2))
    (is (= (content exp-trans) (content data)))))

(defmethod-res main-phase ((self port-test3-source))
  (sl-message 'none "Starting simulation")
  (sim-delay 10)
  (let ((trans (make-instance 'port-test3-trans1 :content 5)))
    (sl-messagef 'none "Sending transaction with content ~a" (content trans))
    (fifo-push *port-test3-trans1-sb* trans)
    (sl-write (ap1 self) trans))

  (sim-delay 20)
  (let ((trans (make-instance 'port-test3-trans2 :content 6)))
    (sl-messagef 'none "Sending transaction with content ~a" (content trans))
    (fifo-push *port-test3-trans2-sb* trans)
    (sl-write (ap1 self) trans))

  (sim-delay 15)
  (let ((trans (make-instance 'port-test3-trans1 :content 7)))
    (sl-messagef 'none "Sending transaction with content ~a" (content trans))
    (fifo-push *port-test3-trans1-sb* trans)
    (sl-write (ap2 self) trans))

  (sim-delay 10)
  (let ((trans (make-instance 'port-test3-trans2 :content 8)))
    (sl-messagef 'none "Sending transaction with content ~a" (content trans))
    (fifo-push *port-test3-trans2-sb* trans)
    (sl-write (ap2 self) trans))

  (sim-delay 15)
  (let ((trans (make-instance 'port-test3-trans1 :content 9)))
    (sl-messagef 'none "Sending transaction with content ~a" (content trans))
    (fifo-push *port-test3-trans1-sb* trans)
    (sl-write (ap1 self) trans))

  (sim-delay 20)
  (let ((trans (make-instance 'port-test3-trans2 :content 10)))
    (sl-messagef 'none "Sending transaction with content ~a" (content trans))
    (fifo-push *port-test3-trans2-sb* trans)
    (sl-write (ap1 self) trans))

  (sim-finish))

(defmethod sl:run ((self port-test3-source))
  (spawn #'main-phase self))

(test test3-multiple-conns-multiple-trans-types
  "Multiple port connections multiple trans types"
  (let ((*sim* (make-instance 'sl-sim))
	(source (create-component port-test3-source nil))
	(dest (create-component port-test3-dest nil)))
    (reset-sim)
    (fifo-clear *port-test3-trans1-sb*)
    (fifo-clear *port-test3-trans2-sb*)
    (load-component source)
    (load-component dest)
    (sl-message 'none "Running elaborate")
    (elaborate)
    (connect-port (-> source ap1) (-> dest imp1))
    (connect-port (-> source ap2) (-> dest imp2))
    (sl-message 'none "Done running elaborate")
    (sl:run 100)

    (is (= 90 (time-now *sim*)))
    (is (= 4 (trans-count (-> dest imp1))))
    (is (= 2 (trans-count (-> dest imp2))))))


;;; Analysis port bcast to multiple imp ports

;;; Multiple subscribers to same ap

;;; Multiple ap's to same imp

;;; Analysis exports

;;; Single analysis fifo single trans type
(defclass port-test8-source (sl-component)
  ((ap :accessor ap :type sl-analysis-port)))

(defclass port-test8-trans-type ()
  ((content :accessor content :initarg :content :initform 0)))

(declare-sl-analysis-fifo port-test8-analysis-fifo port-test8-trans-type)

(defclass port-test8-dest (sl-component)
  ((fifo :accessor fifo :type port-test8-analysis-fifo)
   (trans-count :accessor trans-count :initform 0)))

(defmethod build ((self port-test8-source))
  (setf (-> self ap) (make-instance 'sl-analysis-port :parent self :name "ap")))

(defmethod build ((self port-test8-dest))
  (setf (-> self fifo) (make-instance 'port-test8-analysis-fifo :parent self :name "fifo")))

(defmethod-res main-phase ((self port-test8-source))
  (sl-message 'none "Starting simulation")
  (sim-delay 10)

  (let ((trans (make-instance 'port-test8-trans-type :content 1)))
    (sl-messagef 'none "Sending transaction with content ~a" (content trans))
    (sl-write (-> self ap) trans))

  (sim-delay 5)

  (let ((trans (make-instance 'port-test8-trans-type :content 2)))
    (sl-messagef 'none "Sending transaction with content ~a" (content trans))
    (sl-write (-> self ap) trans))

  (sim-delay 15)

  (let ((trans (make-instance 'port-test8-trans-type :content 3)))
    (sl-messagef 'none "Sending transaction with content ~a" (content trans))
    (sl-write (-> self ap) trans))

  (sim-delay 10)
  (sim-finish))

(defmethod-res main-phase ((self port-test8-dest))
  (sl-message 'none "Collecting transactions from analysis fifo")
  (let ((trans nil))
    (forever
      (sl-message 'none "Waiting for trasaction")
      (pcallr trans #'fifo-get (-> self fifo))
      (sl-messagef 'none "Received transaction with content ~a" (-> trans content))
      (incf (trans-count self)))))

(defmethod sl:run ((self port-test8-source))
  (spawn #'main-phase self))

(defmethod sl:run ((self port-test8-dest))
  (spawn #'main-phase self))

(test test8-single-conn-single-trans-type
  "Single port connection with analysis fifo single trans type"
  (let ((*sim* (make-instance 'sl-sim))
	(source (create-component port-test8-source nil))
	(dest (create-component port-test8-dest nil)))
    (reset-sim)
    (load-component source)
    (load-component dest)
    (sl-message 'none "Running elaborate")
    (elaborate)
    (connect-port (-> source ap) (-> dest fifo))
    (sl-message 'none "Done running elaborate")
    (sl:run 100)

    (is (= 40 (time-now *sim*)))
    (is (= 3 (trans-count dest)))))

;;; Single analysis fifo single trans type, many transactions in same time slot
(defclass port-test9-source (port-test8-source) ())
(defclass port-test9-dest (port-test8-dest) ())

(defmethod-res main-phase ((self port-test9-source))
  (sl-message 'none "Starting simulation")
  (sim-delay 10)

  (let ((trans (make-instance 'port-test8-trans-type :content 1)))
    (sl-messagef 'none "Sending transaction with content ~a" (content trans))
    (sl-write (-> self ap) trans))

  (sim-delay 5)

  (let ((trans (make-instance 'port-test8-trans-type :content 2)))
    (sl-messagef 'none "Sending transaction with content ~a" (content trans))
    (sl-write (-> self ap) trans))

  (let ((trans (make-instance 'port-test8-trans-type :content 3)))
    (sl-messagef 'none "Sending transaction with content ~a" (content trans))
    (sl-write (-> self ap) trans))

  (sim-delay 10)
  (sim-finish))

(test test9-single-conn-single-trans-type-many-trans-same-tslot
  "Single port connection with analysis fifo single trans type many transactions in same time slot"
  (let ((*sim* (make-instance 'sl-sim))
	(source (create-component port-test9-source nil))
	(dest (create-component port-test9-dest nil)))
    (reset-sim)
    (load-component source)
    (load-component dest)
    (sl-message 'none "Running elaborate")
    (elaborate)
    (connect-port (-> source ap) (-> dest fifo))
    (sl-message 'none "Done running elaborate")
    (sl:run 100)

    (is (= 25 (time-now *sim*)))
    ;; TODO: clarify if it's okay to not be able to pop 2 transactions in the same timeslot
    (is (= 2 (trans-count dest)))))

;;; Single analysis fifo multiple trans types

;;; Multiple analysis fifos
