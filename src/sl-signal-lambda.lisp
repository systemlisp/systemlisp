(in-package :system-lisp)

;;; Lambda signals

(defclass sl-signal-lambda (sl-signal-object)
  ((expr :initarg :expr :accessor expr :initform (lambda ()))))

(defmethod setb ((sig sl-signal-lambda) val)
  (let ((new-val (funcall (expr sig))))
    (when (not (funcall (equal-predicate sig) new-val (value sig)))
      (emit-event (edge sig)))
    (setf (value sig) new-val)
    (test-true-false sig)))
