(in-package :system-lisp)

;;; Wait for event timing control
(defclass sl-wait-event-timing-control (abstract-timing-control)
  ((event :initarg :event :accessor event :type sl-sync-event)))

(defmethod process-timing-control ((proc sl-sim-process) (wait-event-ctrl sl-wait-event-timing-control))
  (vector-push-extend proc (sl-sync-event-procs (event wait-event-ctrl))))
