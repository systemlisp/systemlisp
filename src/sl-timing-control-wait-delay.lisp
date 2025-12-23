(in-package :system-lisp)

;;; Delay timig control
(defclass sl-timing-control-wait-delay (abstract-timing-control)
  ((delay :initarg :delay :accessor delay :type integer)))

(defmethod process-timing-control ((proc sl-sim-process) (delay-ctrl sl-timing-control-wait-delay))
  (let ((eval-event (make-instance 'sl-sim-event-eval :proc proc)))
    (if (= (delay delay-ctrl) 0)
	(let ((current-time-slot (get-current-time-slot)))
	  (push-event eval-event (inactive-region current-time-slot)))
	(let* ((future-time (+ (time-now *sim*) (delay delay-ctrl)))
	       (future-time-slot (get-time-slot future-time)))
	  (insert-item-unique (time-slot-times *sim*) future-time)

	  (push-event eval-event (active-region future-time-slot))))))
