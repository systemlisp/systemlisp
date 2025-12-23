(in-package :system-lisp)

;; Generic event group
(defclass sl-sync-event-group (sl-sync-event)
  ((emit-cond :accessor emit-cond
	      :initform (lambda (self) (declare (ignore self)) nil))))

(defmethod connect-event ((event abstract-sync-event) (event-group sl-sync-event-group))
  (vector-push-extend event-group (event-groups event)))

(defmethod connect-event ((event-group sl-sync-event-group) (event abstract-sync-event))
  (vector-push-extend event-group (event-groups event)))

;; OR event group
(defun or-events (&rest events)
  (let ((result (make-instance 'sl-sync-event-group :emit-cond (lambda (self) (declare (ignore self)) t))))
    (loop for event in events do
      (connect-event event result))
    result))

;; Repeat N times event group
(defclass sl-sync-event-repeat-group (sl-sync-event-group)
  ((repeat-count-now :accessor repeat-count-now
		       :initform 0)
   (repeat-count :accessor repeat-count
		 :initarg :repeat-count
		 :initform 0)))

(defmethod initialize-instance :after ((self sl-sync-event-repeat-group) &key)
  (setf (emit-cond self)
	(lambda (self)
	  (incf (repeat-count-now self))
	  (if (= (repeat-count-now self) (repeat-count self))
	      (progn
		(setf (repeat-count-now self) 0)
		t)
	      ;;else
	      nil))))

(defmethod repeat-event ((event abstract-sync-event) (n integer))
  (let ((result (make-instance 'sl-sync-event-repeat-group :repeat-count n)))
    (connect-event event result)
    result))

;; Sync event
(defmacro sim-sync (event)
  (let ((event-name (gensym "event-")))
    `(let ((,event-name ,event))
	 (when (not (triggered ,event-name))
	   (sim-wait ,event-name)))))

;; Wait contition
(defmacro sim-wait-cond (cond-body)
  (let ((cond-event (gensym "cond-event-")))
    `(let ((,cond-event (make-instance 'sl-sync-event-group :emit-cond (lambda (self) ,@cond-body))))
       (sim-wait ,cond-event))))

;; Sync condition
(defmacro sim-sync-cond (cond-body)
  (let ((cond-event (gensym "cond-event-")))
    `(let ((,cond-event (make-instance 'sl-sync-event-group :emit-cond (lambda (self) ,@cond-body))))
       (sim-sync ,cond-event))))
