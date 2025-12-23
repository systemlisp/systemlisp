(in-package :system-lisp)

(defparameter *sl-logger-verbosity* 'none)

(defun verb-to-num (verb)
  (case verb
    (none 0)
    (low 1)
    (medium 2)
    (high 3)
    (full 4)
    (debug 5)
    (otherwise (error (format nil "Invalid verbosity value: ~a" verb)))))

(defun sl-message (verb msg)
  (if (<= (verb-to-num verb) (verb-to-num *sl-logger-verbosity*))
      (format t "[~a] ~a~%" (time-now *sim*) msg)))

(defun sl-messagef (verb msgf &rest args)
  (if (<= (verb-to-num verb) (verb-to-num *sl-logger-verbosity*))
      (apply #'format t (concatenate 'string "[~a] " msgf "~%") (concatenate 'list (list (time-now *sim*)) args))))


(defparameter *sl-break-on-debug* nil)
(defparameter *sl-print-on-debug* nil)

(defmacro sl-debug (msg)
  `(if *sl-break-on-debug*
       (break ,msg)
       (when *sl-print-on-debug*
	 (format t "[~a] DEBUG: ~a~%" (time-now *sim*) ,msg))))

(defmacro sl-debug-on-cond (cond msg)
  `(if (and ,cond *sl-break-on-debug*)
     (break ,msg)
     (when *sl-print-on-debug*
       (format t "[~a] DEBUG: ~a COND ~a IS ~a~%" (time-now *sim*) ,msg (format nil "~a" (quote ,cond)) ,cond))))
