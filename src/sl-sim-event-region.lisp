(in-package :system-lisp)

;;; Event region
(defclass sl-sim-event-region (abstract-event-region) 
  ((event-list :accessor event-list
	       :initform nil)
   (event-list-tail :accessor event-list-tail
		    :initform nil)))

(defmethod is-empty ((region sl-sim-event-region))
  (uiop:emptyp (event-list region)))

(defmethod mvevents ((from-region sl-sim-event-region) (to-region sl-sim-event-region))
  (loop while (not (is-empty from-region)) do
    (let ((event (pop-event from-region)))
      (push-event event to-region))))

(defmethod execute ((region sl-sim-event-region))
  (loop while (not (is-empty region)) do
    (let ((event (pop-event region)))
      (execute event))))

(defmethod pop-event ((region sl-sim-event-region))
  (pop (event-list region)))

(defmethod push-event ((ev abstract-sim-event) (region sl-sim-event-region))
  (with-slots (event-list event-list-tail) region
    (let ((new-tail (list ev)))
      (if (null event-list)
	  (setf event-list new-tail)
	  (setf (cdr event-list-tail) new-tail))
      (setf event-list-tail new-tail))))


