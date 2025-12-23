(in-package :system-lisp)

;;; A simple fifo implemented using linked lists
(defclass sl-fifo-ll ()
  ((list :initform nil)
   (tail :initform nil)))

(defmethod print-object ((queue sl-fifo-ll) stream)
  (print-unreadable-object (queue stream :type t)
    (with-slots (list tail) queue
      (cond ((cddddr list)
	     ;; at least five elements, so print ellipsis
	     (format stream "(~{~S ~}... ~S)"
		     (subseq list 0 3) (first tail)))
	    ;; otherwise print whole list
	    (t (format stream "~:S" list))))))

(defmethod fifo-pop ((queue sl-fifo-ll))
  (with-slots (list) queue
    (pop list)))

(defmethod fifo-push ((queue sl-fifo-ll) new-item)
  (with-slots (list tail) queue
    (let ((new-tail (list new-item)))
      (cond ((null list) (setf list new-tail))
	    (t (setf (cdr tail) new-tail)))
      (setf tail new-tail)))
  queue)

(defmethod fifo-clear ((self sl-fifo-ll))
  (with-slots (list tail) self
    (setf list  '())
    (setf tail  '())
    self))

(defmethod fifo-length ((self sl-fifo-ll))
  (with-slots (list) self
    (length list)))
