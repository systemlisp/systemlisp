(in-package :system-lisp)

;;; Evaluation event
(defclass sl-sim-event-eval (abstract-sim-event)
  ((proc :initarg :proc :accessor proc :type sl-sim-process)))

(defmethod evaluate ((event sl-sim-event-eval))
  (case  (status (proc event))
    ((running waiting)
     (let ((result (continue-process (proc event))))
       (if (not (subtypep (type-of result) 'abstract-timing-control))
	   (progn
	     (setf (status (proc event)) 'done)
	     (setf (return-val (proc event)) result)
	     (emit-event (process-done (proc event))))
	   ;; else
	   (process-timing-control (proc event) result))))))

(defmethod execute ((event sl-sim-event-eval))
  (evaluate event))
