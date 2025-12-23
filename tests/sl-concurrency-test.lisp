(in-package :system-lisp-test)

(def-suite* sl-concurrency-test
  :description "Tests for the concurrency features such as processes, forks, time consuming actions"
  :in sl-main-test)

;;; Test delays
(defun-res proc1 ()
  (print (format nil "Time: ~a Start proc1" (time-now *sim*)))
  (sim-delay 10)
  (print (format nil "Time: ~a End proc1" (time-now *sim*))))

(defun-res proc2 ()
  (print (format nil "Time: ~a Start proc2" (time-now *sim*)))
  (sim-delay 5)
  (print (format nil "Time: ~a End proc2" (time-now *sim*))))

(defun-res proc3 ()
  (print (format nil "Time: ~a Start proc3" (time-now *sim*)))
  (sim-delay 20)
  (print (format nil "Time: ~a End proc3" (time-now *sim*)))
  (sim-delay 40)
  (print (format nil "Time: ~a End proc3 a second time" (time-now *sim*))))

(defun-res proc4 (&aux (i 0))
  (print (format nil "Time: ~a Start proc4" (time-now *sim*)))
  (sim-delay 2)
  (c-for ((setf i 0) (<= i 10) (incf i))
    (print (format nil "Time: ~a proc4 Iteration ~a start" (time-now *sim*) i))
    (if (= (rem i 2) 0)
	(sim-delay 2)
	(sim-delay 3))
    (print (format nil "Time: ~a proc4 Iteration ~a end" (time-now *sim*) i)))
  (print (format nil "Time: ~a End proc4" (time-now *sim*))))

(test test-delays
  "Test delay timing controls"
  (let* ((*sim* (make-instance 'sl-sim)))
    (reset-sim)
    (spawn #'proc1)
    (spawn #'proc2)
    (spawn #'proc3)
    (spawn #'proc4)
    (sl:run 1000)

    (is (= 60 (time-now *sim*)))))
 
;;; Test sync events 
(defvar clk-event (make-instance 'sl-sync-event :name "clk-event"))
(defun-res clk-gen ()
  (let ((emit-count 0))
    (forever
     (format t "Time: ~a clk-gen: Waiting 5 time units ~%" (time-now *sim*))
     (sim-delay 5)
     (format t "Time ~a clk-gen: Emitting clk-event ~%" (time-now *sim*))
     (emit-event clk-event)
     (incf emit-count)
     (save-message 'emit-count emit-count))))

(defun-res proc-waiting-for-clk ()
    (format t "Time ~a proc-waiting-for-clk: Waiting 3 clk-events ~%" (time-now *sim*))
  (let ((wait-count 0))
    (repeat 3
	    (format t "Time ~a proc-waiting-for-clk: Waiting event ~%" (time-now *sim*))
	    (sim-wait clk-event)
	    (incf wait-count)
	    (format t "Time ~a proc-waiting-for-clk: Got event ~%" (time-now *sim*)))
    (save-message 'wait-count wait-count))
    (format t "Time ~a proc-waiting-for-clk: Done waiting 3 clk-events ~%" (time-now *sim*))
    (sim-finish))

(test test-sync-events
  "Test synchronization events"
  (let* ((*sim* (make-instance 'sl-sim)))
    (reset-sim)
    (spawn #'clk-gen)
    (spawn #'proc-waiting-for-clk)
    (sl:run 1000)

    (is (= 15 (time-now *sim*)))
    (is (= (gethash 'emit-count *debug-messages*) (gethash 'wait-count *debug-messages*)))
    (is (= 3 (gethash 'emit-count *debug-messages*)))

    (clear-debug)))

;;; Test fork-join
(defun-res test-fork-join ()
  (let ((x 57))
    (format t "x = ~a ~%" x)
    (fork-join 
      (lambda-res ()
	(format t "(boundp x) = ~a ~%" (boundp 'x))
	(format t "x = ~a ~%" x)
	(save-message 'x x)
	(format t "~a func1 before delay~%" (time-now *sim*))
	(sim-delay 5)
	(format t "~a func1 after delay~%" (time-now *sim*)))
      
      (lambda-res ()
	(format t "~a func2 before delay~%" (time-now *sim*))
	(sim-delay 10)
	(format t "~a func2 after delay~%" (time-now *sim*))))
    (save-timestamp 'after-fork-join (time-now *sim*)))
  (sim-finish))

(test test-fork-join
  "Test forking and joining threads"
  (let ((*sim* (make-instance 'sl-sim)))
    (reset-sim)
    (spawn #'test-fork-join)
    (sl:run 100)

    (is (= 10 (time-now *sim*)))
    (is (= 10 (gethash 'after-fork-join *debug-timestamps*)))
    (is (= 57 (gethash 'x *debug-messages*)))

    (clear-debug)))

;;; Test fork-join-any
(defun-res test-fork-join-any ()
  (fork-join-any 
    (lambda-res ()
      (format t "~a func1 before delay~%" (time-now *sim*))
      (sim-delay 5)
      (format t "~a func1 after delay~%" (time-now *sim*)))
    
    (lambda-res ()
      (format t "~a func2 before delay~%" (time-now *sim*))
      (sim-delay 10)
      (format t "~a func2 after delay~%" (time-now *sim*))))
  (save-timestamp 'after-fork-join-any (time-now *sim*))
  (format t "Main proc stopped at  time is ~a ~%" (time-now *sim*)))

(test test-fork-join-any
  "Test fork join-any - main thread should continue after the first sub-thread finishes, but the others continue"
  (let ((*sim* (make-instance 'sl-sim)))
    (reset-sim)
    (spawn #'test-fork-join-any)
    (sl:run 100)

    (is (= 10 (time-now *sim*)))
    (is (= 5 (gethash 'after-fork-join-any *debug-timestamps*)))

    (clear-debug)))

;;; Test fork-join-first
(defun-res test-fork-join-first ()
  (fork-join-first 
    (lambda-res ()
      (format t "~a func1 before delay~%" (time-now *sim*))
      (sim-delay 5)
      (format t "~a func1 after delay~%" (time-now *sim*)))
    
    (lambda-res ()
      (format t "~a func2 before delay~%" (time-now *sim*))
      (sim-delay 10)
      (format t "~a func2 after delay~%" (time-now *sim*))))
  (save-timestamp 'after-fork-join-first (time-now *sim*))
  (format t "Main proc stopped at time is ~a ~%" (time-now *sim*)))

(test test-fork-join-first
  "Test fork join-first, main thread should resume after the first sub-thread finishes, other sub-threads are terminated"
  (let ((*sim* (make-instance 'sl-sim)))
    (reset-sim)
    (spawn #'test-fork-join-first)
    (sl:run 100)
    (format t "Stop time is ~a ~%" (time-now *sim*))

    (is (= 5 (time-now *sim*)))
    (is (= 5 (gethash 'after-fork-join-first *debug-timestamps*)))

    (clear-debug)))

;;; Test terminate-process
(defun-res test-terminate-process ()
  (let ((func1-proc nil))
    (fork-join
      (lambda-res ()
	(setf func1-proc *process-self*)
	(format t "~a func1 before delay ~%" (time-now *sim*))
	(sim-delay 20)
	(format t "~a func1 after delay ~%" (time-now *sim*)))

      (lambda-res ()
	(sim-delay 10)
	(format t "~a Time's up, stopping func1 ~%" (time-now *sim*))
	(terminate-process func1-proc)))
    (save-timestamp 'after-fork-join (time-now *sim*))
    (format t "Main proc stopped at  time is ~a ~%" (time-now *sim*))))

(test test-terminate-process
  "Test the termination of a process from another process using its handle"
  (let ((*sim* (make-instance 'sl-sim)))
    (reset-sim)
    (spawn #'test-terminate-process)
    (sl:run 100)
    (format t "Stop time is ~a ~%" (time-now *sim*))

    (is (= 10 (gethash 'after-fork-join *debug-timestamps*)))
    (is (= 10 (time-now *sim*)))

    (clear-debug)))

;;; Test pcall of lambda
(defun-res test-pcall-lambda ()
  (format t "Waiting before pcall ~a ~%" (time-now *sim*))
  (sim-delay 100)
  (format t "Done waiting before pcall ~a ~%" (time-now *sim*))
  (pcall (lambda-res ()
	   (format t "Before delay ~a ~%" (time-now *sim*))
	   (sim-delay 50)
	   (format t "After delay ~a ~%" (time-now *sim*))))
  (format t "Waiting after pcall ~a ~%" (time-now *sim*))
  (sim-delay 200)
  (format t "Done waiting after pcall ~a ~%" (time-now *sim*)))

(test test-pcall-lambda
  "Test pcall on a lambda-res expression"
  (let ((*sim* (make-instance 'sl-sim)))
    (reset-sim)
    (spawn #'test-pcall-lambda)
    (sl:run 1000)
    (format t "Stop time is ~a ~%" (time-now *sim*))

    (is (= 350 (time-now *sim*)))

    (clear-debug)))

;;; Test pcall of function
(defun-res dummy-pcall-proc ()
  (format t "Before delay ~a ~%" (time-now *sim*))
  (sim-delay 50)
  (format t "After delay ~a ~%" (time-now *sim*)))

(defun-res test-pcall-res ()
  (format t "Waiting before pcall ~a ~%" (time-now *sim*))
  (sim-delay 100)
  (format t "Done waiting before pcall ~a ~%" (time-now *sim*))
  (pcall dummy-pcall-proc)
  (format t "Waiting after pcall ~a ~%" (time-now *sim*))
  (sim-delay 200)
  (format t "Done waiting after pcall ~a ~%" (time-now *sim*)))

(test test-pcall
  "Test pcall on a defun-res declared function"
  (let ((*sim* (make-instance 'sl-sim)))
    (reset-sim)
    (spawn #'test-pcall-res)
    (sl:run 1000)
    (format t "Stop time is ~a ~%" (time-now *sim*))

    (is (= 350 (time-now *sim*)))

    (clear-debug)))

;; Test procs that return values
(defun-res proc-returning-value ()
  (sim-delay 10)
  (return 5))

(defun-res proc-consuming-value ()
  (let ((x 0))
    (sl-message 'none "Before call")
    (pcallr x proc-returning-value)
    (sl-messagef 'none "After call received ~a~%" x)
    (is (= x 5))))

(test test-pcallr
  "Test pcallr"
  (let ((*sim* (make-instance 'sl-sim)))
    (reset-sim)
    (spawn #'proc-consuming-value)
    (sl:run 50)
    (is (= 10 (time-now *sim*)))))
