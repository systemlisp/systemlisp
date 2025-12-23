(in-package :system-lisp-examples)

;; Run the simulation
(defun counter-run-sim ()
  (let ((*sim* (make-instance 'sl-sim))
	(tb-inst (create-component counter-tb nil)))
    (reset-sim)
    (load-component tb-inst)
    (elaborate)
    (vcd-trace-component tb-inst "top")
    (run 500)))
