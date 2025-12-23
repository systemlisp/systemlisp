(in-package :system-lisp)

;;; Time slot
(defclass sl-time-slot (abstract-time-slot)
  ((sim-time :initarg :sim-time :accessor sim-time :initform 0)
   (preponed-region :accessor preponed-region :initform (make-instance 'sl-sim-event-region) :type sl-sim-event-region)
   (active-region :accessor active-region :initform (make-instance 'sl-sim-event-region) :type sl-sim-event-region)
   (inactive-region :accessor inactive-region :initform (make-instance 'sl-sim-event-region) :type sl-sim-event-region)
   (nba-region :accessor nba-region :initform (make-instance 'sl-sim-event-region) :type sl-sim-event-region)
   (postponed-region :accessor postponed-region :initform (make-instance 'sl-sim-event-region) :type sl-sim-event-region)))

(defmethod execute ((tslot sl-time-slot))
  (execute (preponed-region tslot))
  (loop while (or (not (is-empty (active-region tslot))) (not (is-empty (inactive-region tslot)))) do
    (execute (active-region tslot))
    (when (not (is-empty (inactive-region tslot)))
      (mvevents (inactive-region tslot) (active-region tslot)))
    (execute (nba-region tslot)))
  (execute (postponed-region tslot)))
