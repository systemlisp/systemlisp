(in-package :system-lisp)

;;; Object signal

(defclass sl-signal-object (abstract-signal)
  ((name :initarg :name :accessor sl-signal-name :initform (string (gensym "signal-")))
   (value :initarg :value :accessor value :initform nil)
   (edge :accessor edge :initform (make-instance 'sl-sync-event))
   (true :accessor true :initform (make-instance 'sl-sync-event))
   (false :accessor false :initform (make-instance 'sl-sync-event))
   (vcd-id :accessor vcd-id :initform (get-vcd-name-code) :type string)
   (vcd-db-id :accessor vcd-db-id :initform nil)
   (loads :accessor loads :initform (make-array 10 :adjustable t :fill-pointer 0))
   (equal-predicate :initarg :equal-predicate :accessor equal-predicate :initform #'equalp)
   (dir :accessor dir :initform nil)))

(defmethod propagate ((sig sl-signal-object))
  (loop for l across (loads sig) do
    (setb l (value sig))))

(defmethod test-edge ((sig sl-signal-object) val)
  (when (not (funcall (equal-predicate sig) val (value sig)))
    (emit-event (edge sig))))

(defmethod test-true-false ((sig sl-signal-object))
  (cond
    ((eql nil (value sig)) (emit-event (false sig)))
    (t (emit-event (true sig)))))

(defmethod setb ((sig sl-signal-object) val)
  (test-edge sig val)
  (setf (value sig) val)
  (test-true-false sig)
  (propagate sig))

(defmethod setnb ((sig sl-signal-object) val)
  (let* ((current-time-slot (get-current-time-slot))
	 (sig-val (if (typep val 'sl-signal-object) (value val) val))
	 (update-event (make-instance 'sl-sim-event-update :update-lambda
				      (lambda ()
					(setb sig sig-val)))))
    (push-event update-event (nba-region current-time-slot))))

(defmethod connect-driver-load ((driver sl-signal-object) (load sl-signal-object))
  (vector-push-extend load (loads driver)))

(defmethod connect-load-driver ((load sl-signal-object) (driver sl-signal-object))
  (vector-push-extend load (loads driver)))
