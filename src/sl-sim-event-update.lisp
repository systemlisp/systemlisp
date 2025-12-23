(in-package :system-lisp)

;;; Update event
(defclass sl-sim-event-update (abstract-sim-event)
  ((update-lambda :initarg :update-lambda :accessor update-lambda :type function)))

(defmethod execute ((event sl-sim-event-update))
  (funcall (update-lambda event)))
