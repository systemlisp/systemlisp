(in-package system-lisp)

(defclass sl-port-base ()
  ((name :initarg :name :accessor name :initform "")
   (parent :initarg :parent :accessor parent :type sl-component)))

;;; Imp port
(defclass sl-analysis-imp (sl-port-base) ())

(defgeneric sl-write-imp (port comp data))

;;; Analysis port
(defclass sl-analysis-port (sl-port-base)
  ((others :accessor others :initform (make-array 0 :adjustable t :fill-pointer 0))))

(defgeneric connect-port (src dest))

(defmethod connect-port ((src sl-analysis-port) (dest sl-analysis-imp))
  (vector-push-extend dest (others src)))

(defmethod connect-port ((dest sl-analysis-imp) (src sl-analysis-port))
  (vector-push-extend dest (others src)))

(defmethod sl-write ((port sl-analysis-port) data)
  (loop for other across (others port) do
    (sl-write-imp other (parent other) data)))

;;; Analysis export
(defclass sl-analysis-export (sl-port-base)
  ((others :accessor others :initform (make-array 0 :adjustable t :fill-pointer 0))))

(defmethod connect-port ((src sl-analysis-port) (dest sl-analysis-export))
  (vector-push-extend dest (others src)))

(defmethod connect-port ((dest sl-analysis-export) (src sl-analysis-port))
  (vector-push-extend dest (others src)))

(defmethod connect-port ((src sl-analysis-export) (dest sl-analysis-imp))
  (vector-push-extend dest (others src)))

(defmethod connect-port ((dest sl-analysis-imp) (src sl-analysis-export))
  (vector-push-extend dest (others src)))

(defmethod sl-write-imp ((port sl-analysis-port) (comp sl-component) data)
  (loop for other across (others port) do
    (sl-write-imp other (parent other) data)))

;;; Analysis FIFO
(defclass sl-analysis-fifo (sl-port-base)
  ((fifo :accessor fifo :initform '())
   (fifo-tail :accessor fifo-tail :initform '())
   (fifo-push-event :accessor fifo-push-event :initform (make-instance 'sl-sync-event :name "fifo-push-event"))))

(defmethod fifo-pop ((self sl-analysis-fifo))
  (pop (-> self fifo)))

(defmethod fifo-push ((self sl-analysis-fifo) trans)
  (with-slots (fifo fifo-tail) self
    (let ((new-tail (list trans)))
      (if (null fifo)
	  (setf fifo new-tail)
	  (setf (cdr fifo-tail) new-tail))
      (setf fifo-tail new-tail)))
  self)

(defmethod fifo-clear ((self sl-analysis-fifo))
  (setf (-> self fifo) '())
  (setf (-> self fifo-tail) '())
  self)

(defmethod print-object ((self sl-analysis-fifo) stream)
  (print-unreadable-object (self stream :type t)
    (with-slots (fifo fifo-tail) self
      (cond ((cddddr fifo)
	     ;; at least five elements, so print ellipsis
	     (format stream "(~{~S ~}... ~S)"
		     (subseq fifo 0 3) (first fifo-tail)))
	    ;; otherwise print whole list
	    (t (format stream "~:S" fifo))))))

(defmethod sl-write-imp-base ((self sl-analysis-fifo) (comp sl-component) trans)
  (fifo-push self trans)
  (emit-event (-> self fifo-push-event)))

(defmethod-res fifo-get ((self sl-analysis-fifo))
  (sim-wait (fifo-push-event self))
  (return (fifo-pop self)))

(defmethod connect-port ((src sl-analysis-port) (dest sl-analysis-fifo))
  (vector-push-extend dest (others src)))

(defmethod connect-port ((dest sl-analysis-fifo) (src sl-analysis-port))
  (vector-push-extend dest (others src)))

(defmethod connect-port ((src sl-analysis-export) (dest sl-analysis-fifo))
  (vector-push-extend dest (others src)))

(defmethod connect-port ((dest sl-analysis-fifo) (src sl-analysis-export))
  (vector-push-extend dest (others src)))

(defmacro declare-sl-analysis-fifo (fifo-type trans-type)
  `(progn
     (defclass ,fifo-type (sl-analysis-fifo) ())
     (defmethod sl-write-imp ((self ,fifo-type) (comp sl-component) (trans ,trans-type))
       (sl-write-imp-base self comp trans))))

(defmacro set-analysis-fifo-trans-type (fifo-type trans-type)
  `(defmethod sl-write-imp ((self ,fifo-type) (comp sl-component) (trans ,trans-type))
     (sl-write-imp-base self comp trans)))


