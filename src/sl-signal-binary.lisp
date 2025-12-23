(in-package :system-lisp)

;;; Binary signal

(defclass sl-signal-binary (sl-signal-number)
  ((bit-width :initarg :bit-width :accessor bit-width :type unsigned-byte :initform 1)
   (signed :initarg :signed :accessor signed :type boolean :initform nil)
   (high :accessor high :initform (make-instance 'sl-sync-event :name (gensym "high-")))
   (low :accessor low :initform (make-instance 'sl-sync-event :name (gensym "low-")))))

(defmethod initialize-instance :after ((self sl-signal-binary) &key)
  (typecase (value self)
    (integer (setf (value self) (if (signed self)
				    (int (value self) :bits (bit-width self))
				    (uint (value self) :bits (bit-width self)))))
    (sl-int (setf (value self) (if (signed self)
				   (int (sl-int-value (value self)) :bits (bit-width self))
				   (uint (sl-int-value (value self)) :bits (bit-width self)))))
    (sl-uint (setf (value self) (if (signed self)
				    (int (sl-uint-value (value self)) :bits (bit-width self))
				    (uint (sl-uint-value (value self)) :bits (bit-width self)))))
    (null (setf (values self) (if (signed self)
				  (int 0 :bits (bit-width self))
				  (uint 0 :bits (bit-width self)))))
    (t (error (format t "Invalid type for the value of an sl-signal-binary: ~a" (type-of (value self)))))))

(defmethod test-high-low ((sig sl-signal-binary))
  (cond
    ((sl= 0 (value sig)) (emit-event (false sig)))
    (t (emit-event (true sig)))))

;;; Blocking assignment - assign value to signal
(defmethod setb ((sig sl-signal-binary) (val integer))
  (let ((new-val (if (signed sig)
		     (int val :bits (bit-width sig))
		     (uint val :bits (bit-width sig)))))
    (cond ((funcall (gt-predicate sig) new-val (value sig)) (progn (emit-event (posedge sig))
								   (emit-event (edge sig))))
	  ((funcall (lt-predicate sig) new-val (value sig)) (progn (emit-event (negedge sig))
								   (emit-event (edge sig)))))
    (setf (value sig) new-val)
    (test-high-low sig)
    (update-vcd-db sig)
    (propagate sig)))

(defmethod setb ((sig sl-signal-binary) (val sl-uint))
  (let ((new-val (if (signed sig)
		     (int (sl-uint-value val) :bits (bit-width sig))
		     (uint (sl-uint-value val) :bits (bit-width sig)))))
    (cond ((funcall (gt-predicate sig) new-val (value sig)) (progn (emit-event (posedge sig))
								   (emit-event (edge sig))))
	  ((funcall (lt-predicate sig) new-val (value sig)) (progn (emit-event (negedge sig))
								   (emit-event (edge sig)))))
    (setf (value sig) new-val)
    (test-high-low sig)
    (update-vcd-db sig)
    (propagate sig)))

(defmethod setb ((sig sl-signal-binary) (val sl-int))
  (let ((new-val (if (signed sig)
		     (int (sl-int-value val) :bits (bit-width sig))
		     (uint (sl-int-value val) :bits (bit-width sig)))))
    (cond ((funcall (gt-predicate sig) new-val (value sig)) (progn (emit-event (posedge sig))
								   (emit-event (edge sig))))
	  ((funcall (lt-predicate sig) new-val (value sig)) (progn (emit-event (negedge sig))
								   (emit-event (edge sig)))))
    (setf (value sig) new-val)
    (test-high-low sig)
    (update-vcd-db sig)
    (propagate sig)))

;;; Blocking assignment - assign signal to signal
(defmethod setb ((sig sl-signal-binary) (val sl-signal-binary))
  (let ((new-val (if (signed sig)
		     (to-int (value val) (bit-width sig))
		     (to-uint (value val) (bit-width sig)))))
    (cond ((funcall (gt-predicate sig) new-val (value sig)) (progn (emit-event (posedge sig))
								   (emit-event (edge sig))))
	  ((funcall (lt-predicate sig) new-val (value sig)) (progn (emit-event (negedge sig))
								   (emit-event (edge sig)))))
    (setf (value sig) new-val)
    (update-vcd-db sig)
    (propagate sig)))

(defmacro define-operators-for-int-class (class-name op-name op-sign)
  (let ((sl-op-name (read-from-string (concatenate 'string "sl-" (string op-name))))
	(sl-op-sign (read-from-string (concatenate 'string "sl" (string op-sign)))))
    `(progn
       (defgeneric ,sl-op-name (a b))

       (defmethod ,sl-op-name ((a ,class-name) (b ,class-name))
	 (let* ((max-bit-width (max (bit-width a) (bit-width b)))
		(a-val (if (or (not (signed a)) (not (signed b))) (to-uint (value a) max-bit-width) (to-int (value a) max-bit-width)))
		(b-val (if (or (not (signed a)) (not (signed b))) (to-uint (value b) max-bit-width) (to-int (value b) max-bit-width))))
	   (,op-sign a-val b-val)))

       (defmethod ,sl-op-name ((a ,class-name) (b number))
	 (let ((b-val (if (signed a) (to-int b (bit-width a)) (to-uint b (bit-width a)))))
	   (,op-sign (value a) b)))

       (defmethod ,sl-op-name ((a number) (b ,class-name))
	(let ((a-val (if (signed b) (to-int a (bit-width b)) (to-uint a (bit-width b)))))
	  (,op-sign a-val (value b))))

       (defun ,sl-op-sign (&rest operands)
	 (reduce (function ,sl-op-name) (cdr operands) :initial-value (car operands))))))

;;(define-operators-for-int-class sl-signal-binary add +)
;;(define-operators-for-int-class sl-signal-binary sub -)
;;(define-operators-for-int-class sl-signal-binary mul *)
;;(define-operators-for-int-class sl-signal-binary div /)

;;; Interoperability with sl-int, sl-uint and native lisp integers

;; setb methods for sl-int and sl-signal-binary interoperability
(defmethod setb ((sig sl-signal-binary) (val sl-int))
  (let ((new-val (if (signed sig)
		     (int (sl-int-value val) :bits (bit-width sig))
		     (uint (sl-int-value val) :bits (bit-width sig)))))
    (cond ((funcall (gt-predicate sig) new-val (value sig)) (progn (emit-event (posedge sig))
								   (emit-event (edge sig))))
	  ((funcall (lt-predicate sig) new-val (value sig)) (progn (emit-event (negedge sig))
								   (emit-event (edge sig)))))
    (setf (value sig) new-val)
    (test-high-low sig)
    (update-vcd-db sig)
    (propagate sig)))

;; Arithmetic operations between sl-signal-binary and sl-int/sl-uint
(defmethod binary+ ((a sl-signal-binary) (b sl-int))
  (sl+ (value a) b))

(defmethod binary+ ((a sl-int) (b sl-signal-binary))
  (sl+ a (value b)))

(defmethod binary+ ((a sl-signal-binary) (b sl-uint))
  (sl+ (value a) b))

(defmethod binary+ ((a sl-uint) (b sl-signal-binary))
  (sl+ a (value b)))

(defmethod binary- ((a sl-signal-binary) (b sl-int))
  (sl- (value a) b))

(defmethod binary- ((a sl-int) (b sl-signal-binary))
  (sl- a (value b)))

(defmethod binary- ((a sl-signal-binary) (b sl-uint))
  (sl- (value a) b))

(defmethod binary- ((a sl-uint) (b sl-signal-binary))
  (sl- a (value b)))

(defmethod binary* ((a sl-signal-binary) (b sl-int))
  (sl* (value a) b))

(defmethod binary* ((a sl-int) (b sl-signal-binary))
  (sl* a (value b)))

(defmethod binary* ((a sl-signal-binary) (b sl-uint))
  (sl* (value a) b))

(defmethod binary* ((a sl-uint) (b sl-signal-binary))
  (sl* a (value b)))

(defmethod binary/ ((a sl-signal-binary) (b sl-int))
  (sl/ (value a) b))

(defmethod binary/ ((a sl-int) (b sl-signal-binary))
  (sl/ a (value b)))

(defmethod binary/ ((a sl-signal-binary) (b sl-uint))
  (sl/ (value a) b))

(defmethod binary/ ((a sl-uint) (b sl-signal-binary))
  (sl/ a (value b)))

;; Logical operations between sl-signal-binary and sl-int/sl-uint
(defmethod logand-binary ((a sl-signal-binary) (b sl-int))
  (sl-logand (value a) b))

(defmethod logand-binary ((a sl-int) (b sl-signal-binary))
  (sl-logand a (value b)))

(defmethod logand-binary ((a sl-signal-binary) (b sl-uint))
  (sl-logand (value a) b))

(defmethod logand-binary ((a sl-uint) (b sl-signal-binary))
  (sl-logand a (value b)))

(defmethod logior-binary ((a sl-signal-binary) (b sl-int))
  (sl-logior (value a) b))

(defmethod logior-binary ((a sl-int) (b sl-signal-binary))
  (sl-logior a (value b)))

(defmethod logior-binary ((a sl-signal-binary) (b sl-uint))
  (sl-logior (value a) b))

(defmethod logior-binary ((a sl-uint) (b sl-signal-binary))
  (sl-logior a (value b)))

(defmethod logxor-binary ((a sl-signal-binary) (b sl-int))
  (sl-logxor (value a) b))

(defmethod logxor-binary ((a sl-int) (b sl-signal-binary))
  (sl-logxor a (value b)))

(defmethod logxor-binary ((a sl-signal-binary) (b sl-uint))
  (sl-logxor (value a) b))

(defmethod logxor-binary ((a sl-uint) (b sl-signal-binary))
  (sl-logxor a (value b)))

;; Comparison operations between sl-signal-binary and sl-int/sl-uint
(defmethod binary= ((a sl-signal-binary) (b sl-int))
  (sl= (value a) b))

(defmethod binary= ((a sl-int) (b sl-signal-binary))
  (sl= a (value b)))

(defmethod binary= ((a sl-signal-binary) (b sl-uint))
  (sl= (value a) b))

(defmethod binary= ((a sl-uint) (b sl-signal-binary))
  (sl= a (value b)))

(defmethod binary> ((a sl-signal-binary) (b sl-int))
  (sl> (value a) b))

(defmethod binary> ((a sl-int) (b sl-signal-binary))
  (sl> a (value b)))

(defmethod binary> ((a sl-signal-binary) (b sl-uint))
  (sl> (value a) b))

(defmethod binary> ((a sl-uint) (b sl-signal-binary))
  (sl> a (value b)))

(defmethod binary< ((a sl-signal-binary) (b sl-int))
  (sl< (value a) b))

(defmethod binary< ((a sl-int) (b sl-signal-binary))
  (sl< a (value b)))

(defmethod binary< ((a sl-signal-binary) (b sl-uint))
  (sl< (value a) b))

(defmethod binary< ((a sl-uint) (b sl-signal-binary))
  (sl< a (value b)))

(defmethod binary>= ((a sl-signal-binary) (b sl-int))
  (sl>= (value a) b))

(defmethod binary>= ((a sl-int) (b sl-signal-binary))
  (sl>= a (value b)))

(defmethod binary>= ((a sl-signal-binary) (b sl-uint))
  (sl>= (value a) b))

(defmethod binary>= ((a sl-uint) (b sl-signal-binary))
  (sl>= a (value b)))

(defmethod binary<= ((a sl-signal-binary) (b sl-int))
  (sl<= (value a) b))

(defmethod binary<= ((a sl-int) (b sl-signal-binary))
  (sl<= a (value b)))

(defmethod binary<= ((a sl-signal-binary) (b sl-uint))
  (sl<= (value a) b))

(defmethod binary<= ((a sl-uint) (b sl-signal-binary))
  (sl<= a (value b)))

;; Bit operations between sl-signal-binary and sl-int/sl-uint
(defmethod signal-bit ((sig sl-signal-binary) (n number))
  (bit (value sig) n))

(defmethod (setf bit) ((val number) (sig sl-signal-binary) (n number))
  (setf (bit (value sig) n) val))

(defmethod bits ((sig sl-signal-binary) (hi number) (lo number))
  (bits (value sig) hi lo))

(defmethod (setf bits) ((val number) (sig sl-signal-binary) (hi number) (lo number))
  (setf (bits (value sig) hi lo) val))

(defmethod (setf bits) ((val sl-int) (sig sl-signal-binary) (hi number) (lo number))
  (setf (bits (value sig) hi lo) (sl-int-value val)))

(defmethod (setf bits) ((val sl-uint) (sig sl-signal-binary) (hi number) (lo number))
  (setf (bits (value sig) hi lo) (sl-uint-value val)))

;; Additional interoperability with native lisp integers
(defmethod binary+ ((a sl-signal-binary) (b integer))
  (sl+ (value a) b))

(defmethod binary+ ((a integer) (b sl-signal-binary))
  (sl+ a (value b)))

(defmethod binary- ((a sl-signal-binary) (b integer))
  (sl- (value a) b))

(defmethod binary- ((a integer) (b sl-signal-binary))
  (sl- a (value b)))

(defmethod binary* ((a sl-signal-binary) (b integer))
  (sl* (value a) b))

(defmethod binary* ((a integer) (b sl-signal-binary))
  (sl* a (value b)))

(defmethod binary/ ((a sl-signal-binary) (b integer))
  (sl/ (value a) b))

(defmethod binary/ ((a integer) (b sl-signal-binary))
  (sl/ a (value b)))

(defmethod logand-binary ((a sl-signal-binary) (b integer))
  (sl-logand (value a) b))

(defmethod logand-binary ((a integer) (b sl-signal-binary))
  (sl-logand a (value b)))

(defmethod logior-binary ((a sl-signal-binary) (b integer))
  (sl-logior (value a) b))

(defmethod logior-binary ((a integer) (b sl-signal-binary))
  (sl-logior a (value b)))

(defmethod logxor-binary ((a sl-signal-binary) (b integer))
  (sl-logxor (value a) b))

(defmethod logxor-binary ((a integer) (b sl-signal-binary))
  (sl-logxor a (value b)))

(defmethod binary= ((a sl-signal-binary) (b integer))
  (sl= (value a) b))

(defmethod binary= ((a integer) (b sl-signal-binary))
  (sl= a (value b)))

(defmethod binary> ((a sl-signal-binary) (b integer))
  (sl> (value a) b))

(defmethod binary> ((a integer) (b sl-signal-binary))
  (sl> a (value b)))

(defmethod binary< ((a sl-signal-binary) (b integer))
  (sl< (value a) b))

(defmethod binary< ((a integer) (b sl-signal-binary))
  (sl< a (value b)))

(defmethod binary>= ((a sl-signal-binary) (b integer))
  (sl>= (value a) b))

(defmethod binary>= ((a integer) (b sl-signal-binary))
  (sl>= a (value b)))

(defmethod binary<= ((a sl-signal-binary) (b integer))
  (sl<= (value a) b))

(defmethod binary<= ((a integer) (b sl-signal-binary))
  (sl<= a (value b)))

;; Increment and decrement operations for sl-signal-binary
(defmethod sl1+ ((sig sl-signal-binary))
  (sl1+ (value sig)))

(defmethod sl1- ((sig sl-signal-binary))
  (sl1- (value sig)))

;; Logical NOT operation for sl-signal-binary
(defmethod lognot-unary ((sig sl-signal-binary))
  (sl-lognot (value sig)))

;; Auxiliary method to sample a signal on the edge of another signal
(defmethod-res relative-edge-wait
    (edge-to-detect (sig sl-signal-binary) (sampling-ev sl-sync-event) &key (stop-condition (lambda () nil)))
  (assert (member edge-to-detect '(edge negedge posedge)) () "Invalid edge name: ~a" edge-to-detect)
  (let ((prev-sig-val (-> sig value))
	(edge-cond nil))
    (forever
     (sim-wait sampling-ev)

     (when (funcall stop-condition) (return nil))
      
     (when (eql edge-to-detect 'edge)
       (setf edge-cond (not (sl= sig prev-sig-val))))
      
     (when (eql edge-to-detect 'posedge)
       (setf edge-cond (and (sl= prev-sig-val 0) (sl= sig 1))))
      
     (when (eql edge-to-detect 'negedge)
       (setf edge-cond (and (sl= prev-sig-val 1) (sl= sig 0))))
      
     (setf prev-sig-val (-> sig value))

     (when edge-cond (break-loop)))
    (return t)))
