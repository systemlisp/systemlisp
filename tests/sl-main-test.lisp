
(in-package :system-lisp-test)

(def-suite sl-main-test
  :description "The master suite of all SL tests")

(in-suite sl-main-test)

(defun test-sl ()
  (run! 'sl-main-test))

(defparameter *debug-timestamps* (make-hash-table))
(defparameter *debug-messages* (make-hash-table))

(defun save-timestamp (key value)
  (setf (gethash key *debug-timestamps*) value))

(defun save-message (key value)
  (setf (gethash key *debug-messages*) value))

(defun clear-debug ()
  (setf *debug-timestamps* (make-hash-table))
  (setf *debug-messages* (make-hash-table)))

(load (truename "tests/sl-data-types-test.lisp") :verbose t :print t)
(load (truename "tests/sl-concurrency-test.lisp") :verbose t :print t)
(load (truename "tests/sl-dv-sim-test.lisp") :verbose t :print t)
(load (truename "tests/sl-signal-test.lisp") :verbose t :print t)
(load (truename "tests/sl-syntax-test.lisp") :verbose t :print t)
(load (truename "tests/sl-port-test.lisp") :verbose t :print t)

