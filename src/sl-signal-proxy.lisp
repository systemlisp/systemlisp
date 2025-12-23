(in-package :system-lisp)

(defclass sl-signal-proxy-base (sl-signal-binary)
  ((expr :initarg :expr :accessor expr :type function :initform (lambda () 0))
   (has-lambda :accessor has-lambda :type boolean :initform nil)))

(defclass sl-signal-bit-proxy (sl-signal-proxy-base)
  ((sig-ref :initarg :sig-ref :accessor sig-ref :type sl-signal-binary)
   (bit-index :initarg :bit-index :accessor bit-index :type integer)
   (bit-width :accessor bit-width :initform 1)))

(defclass sl-signal-slice-proxy (sl-signal-proxy-base)
  ((sig-ref :initarg :sig-ref :accessor sig-ref :type sl-signal-binary)
   (msb-index :initarg :msb-index :accessor msb-index :type integer)
   (lsb-index :initarg :lsb-index :accessor lsb-index :type integer)))

(defmethod initialize-instance :after ((obj sl-signal-slice-proxy) &key)
  (setf (bit-width obj) (abs (- (msb-index obj) (lsb-index obj)))))

(defclass sl-signal-concat-proxy (sl-signal-proxy-base)
  ((sig-refs :initarg :sig-refs :accessor sig-refs :type vector)))

(defmethod initialize-instance :after ((obj sl-signal-concat-proxy) &key)
  (setf (bit-width obj) (reduce (lambda (x y) (+ (bit-width x) (bit-width y))) (sig-refs obj) :initial-value 0)))

(defmethod bit ((sig sl-signal-binary) (bit integer))
  (let ((result (make-instance 'sl-signal-bit-proxy :sig-ref sig :bit-index bit)))
    result))

(defmethod slice ((sig sl-signal-binary) (msb integer) (lsb integer))
  (let ((result (make-instance 'sl-signal-slice-proxy :sig-ref sig :msb-index msb :lsb-index lsb)))
    result))

(defun signal-concat (&rest sigs)
  (let ((result (make-instance 'sl-signal-concat-proxy :sig-refs (make-array 0 :adjustable t :fill-pointer 0))))
    (loop for sig in sigs do
	  (if (typep sig 'sl-signal-binary)
	      (vector-push-extend sig (sig-refs result))
	      (error "~a is not of type sl-signal-binary" sig)))
    result))

(defmethod setb ((obj sl-signal-bit-proxy) (val integer))
  (let ((new-val (if (has-lambda obj) (funcall (expr obj)) val)))
    (if (-> obj sig-ref signed)
	(progn
	  (setb (-> obj sig-ref) (int-bit-set (bit-index obj)
					      (-> obj sig-ref value sl-int-value)
					      (-> obj sig-ref value sl-int-bits)
					      (not (zerop new-val)))))
	(progn
	  (setb (-> obj sig-ref) (uint-bit-set (bit-index obj)
					       (-> obj sig-ref value sl-uint-value)
					       (-> obj sig-ref value sl-uint-bits)
					       (not (zerop new-val))))))))

(defmethod setb ((obj sl-signal-slice-proxy) (val integer))
  (let ((new-val (if (has-lambda obj) (funcall (expr obj)) val)))
    (if (-> obj sig-ref signed)
	(setb (-> obj sig-ref) (int-set-slice (msb-index obj)
					      (lsb-index obj)
					      new-val
					      (-> obj sig-ref value sl-int-value)
					      (-> obj sig-ref value sl-int-bits)))
	(setb (-> obj sig-ref) (uint-set-slice (msb-index obj)
					       (lsb-index obj)
					       new-val
					       (-> obj sig-ref value sl-uint-value)
					       (-> obj sig-ref value sl-uint-bits))))))

(defmethod setb ((obj sl-signal-concat-proxy) (val integer))
  (let ((new-val (if (has-lambda obj) (funcall (expr obj)) val)))
    (let ((crt-lsb 0))
      (loop for sig across (-> obj sig-refs) do
	(setb sig (uint-get-slice (+ crt-lsb (-> sig bit-width) -1)
				  crt-lsb new-val
				  (-> sig bit-width)))
	(incf crt-lsb (-> sig bit-width))))))

(defmethod connect-comb ((sig sl-signal-bit-proxy) (expr function) (sense-sig sl-signal-binary))
  (setf (has-lambda sig) t)
  (setf (expr sig) expr)
  (connect-driver-load sense-sig sig))

(defmethod connect-comb ((sig sl-signal-slice-proxy) (expr function) (sense-sig sl-signal-binary))
  (setf (has-lambda sig) t)
  (setf (expr sig) expr)
  (connect-driver-load sense-sig sig))

(defmethod connect-comb ((sig sl-signal-concat-proxy) (expr function) (sense-list vector))
  (setf (has-lambda sig) t)
  (setf (expr sig) expr)
  (loop for sense across sense-list do
    (connect-driver-load sense sig)))
