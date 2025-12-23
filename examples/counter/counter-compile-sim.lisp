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

(when (not *exe-name*)
  (format t "ERROR: must provide executable name as argument to this script~%")
  (uiop:quit 1))

;; Load system-lisp and examples
(ql:quickload :system-lisp)
(asdf:load-system :system-lisp/examples :force t :verbose t)

(in-package :system-lisp-examples)

(setq uiop:*image-entry-point* #'counter-run-sim)
(uiop:dump-image (second sb-ext:*posix-argv*) :executable t)
