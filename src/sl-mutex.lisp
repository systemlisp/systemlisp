(in-package :system-lisp)

(defclass sl-mutex (sl-sync-event-group)
  ((is-locked :accessor is-locked
	      :initform nil)
   (owner-proc :accessor owner-proc
	       :initform nil)))

(defmethod-res lock ((self sl-mutex))
  (if (not (is-locked self))
      (progn
	(setf (is-locked self) t)
	(setf (owner-proc self) *process-self*))
      ;; else
      (when (not (eq *process-self* (owner-proc self)))
	(sim-wait self))))

(defmethod unlock ((self sl-mutex))
  (when (eq *process-self* (owner-proc self))
    (setf (is-locked self) t)
    (emit-event self)))
