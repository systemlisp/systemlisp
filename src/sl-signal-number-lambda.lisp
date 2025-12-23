(in-package :system-lisp)

;;; Number lambda signal

(defclass sl-signal-number-lambda (sl-signal-lambda sl-signal-number) ())

(defmethod setb ((sig sl-signal-number-lambda) val)
  (let ((new-val (funcall (expr sig))))
    (cond ((funcall (gt-predicate sig) new-val (value sig)) (progn (emit-event (posedge sig))
								   (emit-event (edge sig))))
	  ((funcall (lt-predicate sig) new-val (value sig)) (progn (emit-event (negedge sig))
								   (emit-event (edge sig)))))
    (setf (value sig) new-val)
    (update-vcd-db sig)
    (propagate sig)))
