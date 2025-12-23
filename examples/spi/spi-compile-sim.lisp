#!/usr/bin/sbcl --script

;; Setup quicklisp (must be installed together with system-lisp)
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; Optimize for speed
(declaim (optimize (speed 3) (space 0) (debug 0)))

;; Get executable name from command line
(defparameter *exe-name* (second sb-ext:*posix-argv*))
(defparameter *test-name* (third sb-ext:*posix-argv*))

(export '*test-name*)

(when (< (length sb-ext:*posix-argv*) 3)
  (format t "ERROR: must provide executable name and SPI test name as argument to this script~%")
  (format t "Usage: spi-compile-sim.lisp <executable-name> <spi-test-run-function-to-compile>~%")
  (uiop:quit 1))

(when (not (member (string-downcase *test-name*) (list "run-spi-test-1master-1slave-sim") :test #'string=))
  (format t "ERROR: invalid test run function ~a~%" *test-name*)
  (format t "Supported test run functions ~a~%" (list "run-spi-test-1master-1slave-sim"))
  (uiop:quit 1))

;; Load system-lisp and examples
(ql:quickload :system-lisp)
(asdf:load-system :system-lisp/examples :force t :verbose t)

(in-package :system-lisp-examples)

(setq uiop:*image-entry-point* (eval (list 'function (read-from-string cl-user:*test-name*))))
(uiop:dump-image (second sb-ext:*posix-argv*) :executable t)
