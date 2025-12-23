(in-package :system-lisp)

(declaim (optimize (speed 0) (space 0) (debug 3)))
  
(require 'cl-generator)
;(use-package 'cl-generator)
;(use-package 'cl-generator-util)
(require 'cl-containers)

;;; Useful enums
(deftype sl-proc-status () '(member running done terminated suspended waiting))

(deftype sl-sim-status () '(member sim-running sim-paused sim-stopped))

;;; Auxiliary method for making heaps with unique elements
(defmethod insert-item-unique ((cont containers:priority-queue-on-container) item)
  (when (not (containers:find-item cont item))
    (containers:insert-item cont item)))

;;; Simulator
(defclass sl-sim (abstract-sim)
  ((time-now :accessor time-now :initform 0)
   (time-next :accessor time-next :initform 0)
   (time-slots :accessor time-slots :initform (make-hash-table))
   (time-slot-times :accessor time-slot-times :initform (make-instance 'containers:priority-queue-on-container :sorter #'< :test #'=))
   (simlog :accessor simlog :initform (make-array 100 :adjustable t :fill-pointer 0 :element-type 'string :initial-element ""))
   (sim-state :accessor sim-state :initform 'sim-running)
   (sim-tops :accessor sim-tops :initform '())
   (sim-db :accessor sim-db :initform nil)))

;; The simulator instance
(defparameter *sim* (make-instance 'sl-sim))

(defun get-current-time-slot ()
  (gethash (time-now *sim*) (time-slots *sim*)))

(defmethod get-time-slot ((time integer))
  "Get the timeslot for a given simulation time. If it doesn't exist create it."
  (let ((time-slot
	  (gethash time (time-slots *sim*))))
    (if (null time-slot)
	(let ((new-time-slot
		(make-instance 'sl-time-slot :sim-time time)))
	  (setf (gethash time (time-slots *sim*)) new-time-slot)
	  new-time-slot)
	time-slot)))

(defun elaborate ()
  "Elaborates the loaded components"
  (loop for top in (sim-tops *sim*) do
    (do-build top)
    (do-connect top)
    (do-run top)))

(defmethod load-component ((comp sl-component))
  "Loads a component into the simulator"
  (push comp (sim-tops *sim*)))

(defmethod vcd-trace-component ((comp sl-component) &optional (name "" name-p))
  (if name-p
      (vcd-db-add-component (sim-db *sim*) comp nil :comp-name name)
      (vcd-db-add-component (sim-db *sim*) comp)))

(defmethod run ((time-limit integer))
  (loop while (and (not (= (hash-table-count (time-slots *sim*)) 0)) (eq (sim-state *sim*) 'sim-running)) do
    (progn
      ; Update time
      (setf (time-now *sim*) (containers:first-element (time-slot-times *sim*)))
      ; If time limit exceeded stop simulation
      (when (> (time-now *sim*) time-limit)
	(progn (setf (time-now *sim*) time-limit)
	       (loop-finish)))
      ; Execute current time slot
      (execute (get-current-time-slot))
      ; Delete executed time slot and its time value
      (containers:delete-first (time-slot-times *sim*))
      (remhash (time-now *sim*) (time-slots *sim*))))
  (vcd-db-handle-to-file (sim-db *sim*) "waves.vcd" (get-timeprecision-string) (time-now *sim*)))

(defun reset-sim ()
  (clrhash (time-slots *sim*))
  (setf (time-now *sim*) 0)
  (containers:empty! (time-slot-times *sim*))
  (insert-item-unique (time-slot-times *sim*) 0)
  (setf (gethash 0 (time-slots *sim*)) (make-instance 'sl-time-slot))
  (setf (sim-state *sim*) 'sim-running)
  (setf (sim-db *sim*) (create-vcd-db (format nil "sim-~a.db" (local-time:format-timestring nil (local-time:now) :format '(:year "-" :month "-" :day "_" :hour "-" :min "-" :sec "-" :usec)))))
  (clear-vcd-db (sim-db *sim*))
  (setf (sim-state *sim*) 'sim-running)
  (setf (sim-tops *sim*) '())
  (values))

(defun sim-finish ()
  (setf (sim-state *sim*) 'sim-stopped)
  (values))

(defun sim-log (msg)
  (let ((full-msg
	  (format nil "Time ~a: ~a" (time-now *sim*) msg)))
    (format t "~a~%" full-msg)
    (vector-push-extend full-msg (simlog *sim*)))
  (values))

(defun sl-dump-waves ()
  (vcd-db-handle-to-file (sim-db *sim*) "waves.vcd" "1ps" (time-now *sim*)))


;;; Macros
(defmacro sim-delay (delay-val)
  (cond
    ((numberp delay-val)
     `(progn
	(setf (status *process-self*) 'waiting)
	(yield (make-instance 'sl-timing-control-wait-delay :delay (sl-time ,delay-val *timeunit*)))))
    ((and (listp delay-val) (= 2 (length delay-val)) (numberp (first delay-val)) (symbolp (second delay-val)))
     `(progn
	(setf (status *process-self*) 'waiting)
	(yield (make-instance 'sl-timing-control-wait-delay :delay (sl-time ,(first delay-val) ,(second delay-val))))))
    (t (error "Invalid delay value: ~a" delay-val))))

(defmacro sim-wait (event-val)
  `(progn
     (setf (status *process-self*) 'waiting)
     (yield (make-instance 'sl-wait-event-timing-control :event ,event-val))))
