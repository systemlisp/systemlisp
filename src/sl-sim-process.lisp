(in-package :system-lisp)

;; Pointer to the self process handle
(defparameter *process-self* nil)

;;; Simulation process class
(defclass sl-sim-process (abstract-sim-event)
  ((func :initarg :func :accessor func)
   (status :initarg :status :accessor status :type sl-proc-status :initform 'running)
   (return-val :initform nil :accessor return-val)
   (process-done :accessor process-done :initform (make-instance 'sl-sync-event :name (gensym "process-done-")))
   (forked-procs :accessor forked-procs :initform (make-array 100 :adjustable t :fill-pointer 0))))

(defmethod continue-process ((proc sl-sim-process))
  (let ((*process-self* proc))
    (typecase (func proc)
      (resumable-state-env (resume (func proc)))
      (cl-generator:iter (funcall (iter-next (func proc)))))))

(defmethod terminate-process ((proc sl-sim-process))
  (setf (status proc) 'done)
  (loop while (not (uiop:emptyp (forked-procs proc))) do
	(terminate-process (vector-pop (forked-procs proc))))
  (emit-event (process-done proc)))

(defmethod suspend-process ((proc sl-sim-process))
  (setf (status proc) 'suspended))

(defmethod resume-process ((proc sl-sim-process))
  (setf (status proc) 'running))

(defmethod spawn ((fproc function) &rest args)
  "Spawns a process"
  (let ((current-time-slot
	  (get-current-time-slot))
	(eval-event (make-instance 'sl-sim-event-eval
				   :proc (make-instance 'sl-sim-process :func (apply fproc args) :status 'running))))
    (when *process-self*
      (vector-push-extend (proc eval-event) (forked-procs *process-self*)))
    (push-event eval-event (active-region current-time-slot))
    (proc eval-event)))

(defmacro pcall (proc-func &rest args)
  (let ((proc-func-alias (gensym "proc-func-alias-"))
	(pobj (gensym "pobj-"))
	(pres (gensym "pres-")))
      `(let ((,proc-func-alias ,(if (symbolp proc-func) `(function ,proc-func) proc-func))
	     (,pobj nil)
	     (,pres nil))
	 (setf ,pobj (apply ,proc-func-alias (list ,@args)))
	 (setf ,pres (resume ,pobj))
	 (while (resumable-state ,pobj)
	   (yield ,pres)
	   (setf ,pres (resume ,pobj))))))

(defmacro pcallr (var proc-func &rest args)
  (let ((proc-func-alias (gensym "proc-func-alias-"))
	(pobj (gensym "pobj-"))
	(pres (gensym "pres-")))
    `(let ((,proc-func-alias ,(if (symbolp proc-func)
				  `(function ,proc-func)
				  proc-func))
	   (,pobj nil)
	   (,pres nil))
       (setf ,pobj (apply ,proc-func-alias (list ,@args)))
       (setf ,pres (resume ,pobj))
       (while (resumable-state ,pobj)
	      (yield ,pres)
	      (setf ,pres (resume ,pobj)))
       (setf ,var ,pres))))

(defmacro wait-proc-handle (proc-handle)
  `(sim-wait (process-done ,proc-handle)))

(defclass sl-join-group (sl-sync-event-group)
  ((join-proc-list :accessor join-proc-list
		   :initform (make-array 100 :adjustable t :fill-pointer 0))))

(defmethod add-proc-to-group ((proc sl-sim-process) (group sl-join-group))
  (vector-push-extend proc (join-proc-list group))
  (connect-event (process-done proc) group))

(defmethod initialize-instance :after ((self sl-join-group) &key)
  (setf (emit-cond self)
	(lambda (self)
	  (let ((result t))
	    (loop for proc across (join-proc-list self) do
	      (when (not (eql (status proc) 'done))
		(setf result nil)
		(return)))
	    result))))

(defmacro fork-join (&body body)
  (let ((join-group-name (gensym "join-group-"))
	 (group-connections '()))
    (loop for func in body do
	  (uiop:appendf group-connections
			(list `(add-proc-to-group (spawn ,func) ,join-group-name))))
    `(let ((,join-group-name (make-instance 'sl-join-group)))
       ,@group-connections
       (sim-wait ,join-group-name))))

(defclass sl-join-any-group (sl-join-group) ())

(defmethod initialize-instance :after ((self sl-join-any-group) &key)
  (setf (emit-cond self)
	(lambda (self)
	  (let ((result nil))
	    (loop for proc across (join-proc-list self) do
	      (when (eql (status proc) 'done)
		(setf result t)
		(return)))
	    result))))

(defmacro fork-join-any (&body body)
  (let ((join-group-name (gensym "join-group-"))
	 (group-connections '()))
    (loop for func in body do
	  (uiop:appendf group-connections
			(list `(add-proc-to-group (spawn ,func) ,join-group-name))))
    `(let ((,join-group-name (make-instance 'sl-join-any-group)))
       ,@group-connections
       (sim-wait ,join-group-name))))

(defclass sl-join-first-group (sl-join-group) ())

(defmethod initialize-instance :after ((self sl-join-first-group) &key)
  (setf (emit-cond self)
	(lambda (self)
	  (let ((result nil))
	    (loop for proc across (join-proc-list self) do
	      (when (eql (status proc) 'done)
		(setf result t)
		(return)))
	    (when result
	      (loop for proc across (join-proc-list self) do
		(when (not (eql (status proc) 'done))
		  (terminate-process proc))))
	    result))))

(defmacro fork-join-first (&body body)
  (let ((join-group-name (gensym "join-group-"))
	 (group-connections '()))
    (loop for func in body do
	  (uiop:appendf group-connections
			(list `(add-proc-to-group (spawn ,func) ,join-group-name))))
    `(let ((,join-group-name (make-instance 'sl-join-first-group)))
       ,@group-connections
       (sim-wait ,join-group-name))))

