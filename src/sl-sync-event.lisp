(in-package :system-lisp)

;;; Sync event
(defclass sl-sync-event (abstract-sync-event)
  ((name :initarg :name :accessor sl-sync-event-name :initform (string (gensym "sl-sync-event-")))
   (procs :accessor sl-sync-event-procs :initform (make-array 100 :adjustable t :fill-pointer 0))
   (triggered :accessor triggered :initform nil)
   (event-groups :accessor event-groups :initform (make-array 100 :adjustable t :fill-pointer 0))))

(defmethod emit-event ((event sl-sync-event))
  (let ((current-time-slot (get-current-time-slot)))
    (map nil (lambda (it)
	       (push-event (make-instance 'sl-sim-event-eval :proc it) (active-region current-time-slot)))
	 (sl-sync-event-procs event))
    (setf (fill-pointer (sl-sync-event-procs event)) 0)

    (when (not (triggered event))
      (setf (triggered event) t)
      (map nil (lambda (it)
		 (when (funcall (emit-cond it) it)
		   (emit-event it)))
	   (event-groups event))
      
      (push-event
       (make-instance 'sl-sim-event-update :update-lambda (lambda () (setf (triggered event) nil)))
       (postponed-region current-time-slot)))))

(defmethod emit-event-nb ((event sl-sync-event))
  (let ((current-time-slot (get-current-time-slot))
	(update-event (make-instance 'sl-sim-event-update :update-lambda (lambda () (emit-event event)))))
    (push-event update-event (active-region current-time-slot))))


