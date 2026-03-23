(in-package :system-lisp-examples)

(defun run-femtorv32-tb ()
  (let* ((*sim* (make-instance 'sl-sim))
	 (dut (create-component femtorv32 nil))
	 ;; Clock generation
	 (clk-gen
	   (lambda-res ()
	     (setb (clk dut) 0)
	     (forever
	       (sim-delay 5)
	       (setb (clk dut)
		     (sl-lognot (value (clk dut)))))))
	 ;; The test logic
	 (simple-bringup-test
	   (lambda-res ()
	     (setb (-> dut reset) 1)
	     (setb (-> dut mem-rbusy) 1)
	     (repeat 5  (sim-wait (posedge (-> dut clk))))
	     (setb (-> dut reset) 0)
	     (repeat 5 (sim-wait (posedge (-> dut clk))))
	     (setb (-> dut reset) 1)

	     (repeat 100 (sim-wait (posedge (-> dut clk)))))))
    (reset-sim)
    (load-component dut)
    (elaborate)
    (spawn clk-gen)
    (spawn simple-bringup-test)
    (vcd-trace-component dut "dut")
    (run 1000)))
