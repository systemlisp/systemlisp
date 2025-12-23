(in-package :system-lisp)

;;; Number signal
(defclass sl-signal-number (sl-signal-object)
  ((posedge :initarg :posedge :accessor posedge :initform (make-instance 'sl-sync-event))
   (negedge :initarg :negedge :accessor negedge :initform (make-instance 'sl-sync-event))
   (gt-predicate :initarg :gt-predicate :accessor gt-predicate :initform #'binary>)
   (lt-predicate :initarg :lt-predicate :accessor lt-predicate :initform #'binary<)
   (reset-value :initarg :reset-value :accessor reset-value :initform 0)))

(defmethod initialize-instance :after ((sig sl-signal-number) &key)
  (setf (value sig) (reset-value sig)))

(defmethod update-vcd-db ((sig sl-signal-number))
  (when (and *sim* (vcd-db-id sig))
    (vcd-db-add-change (sim-db *sim*) sig (time-now *sim*))))

(defmethod setb ((sig sl-signal-number) (val number))
  (cond ((funcall (gt-predicate sig) val (value sig)) (progn (emit-event (posedge sig))
							     (emit-event (edge sig))))
	((funcall (lt-predicate sig) val (value sig)) (progn (emit-event (negedge sig))
							     (emit-event (edge sig)))))
  (setf (value sig) val)
  (update-vcd-db sig)
  (propagate sig))

(defmethod setb ((sig sl-signal-number) (val sl-uint))
  (cond ((funcall (gt-predicate sig) val (value sig)) (progn (emit-event (posedge sig))
							     (emit-event (edge sig))))
	((funcall (lt-predicate sig) val (value sig)) (progn (emit-event (negedge sig))
							     (emit-event (edge sig)))))
  (setf (value sig) val)
  (update-vcd-db sig)
  (propagate sig))

(defmethod setb ((sig sl-signal-number) (val sl-int))
  (cond ((funcall (gt-predicate sig) val (value sig)) (progn (emit-event (posedge sig))
							     (emit-event (edge sig))))
	((funcall (lt-predicate sig) val (value sig)) (progn (emit-event (negedge sig))
							     (emit-event (edge sig)))))
  (setf (value sig) val)
  (update-vcd-db sig)
  (propagate sig))

