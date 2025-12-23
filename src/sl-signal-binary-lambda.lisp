(in-package :system-lisp)

;;; Binary lambda signal
(defclass sl-signal-binary-lambda (sl-signal-lambda sl-signal-binary) ())

(defmethod setb ((sig sl-signal-binary-lambda) val)
  (let ((new-val (funcall (expr sig))))
    (cond ((funcall (gt-predicate sig) new-val (value sig)) (progn (emit-event (posedge sig))
								   (emit-event (edge sig))))
	  ((funcall (lt-predicate sig) new-val (value sig)) (progn (emit-event (negedge sig))
								   (emit-event (edge sig)))))
    (setf (value sig) new-val)
    (test-high-low sig)
    (update-vcd-db sig)
    (propagate sig)))

;;; Connect combinatorial logic (equivalent of assign from Verilog)
(defmethod connect-comb ((sig sl-signal-binary-lambda) (expr function) (sense-list vector))
  (setf (-> sig expr) expr)
  (loop for sense across sense-list do
    (connect-driver-load sense sig)))

