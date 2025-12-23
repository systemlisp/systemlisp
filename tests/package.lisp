
(defpackage :system-lisp-test
  (:use
   :cl
   :fiveam
   :sl)

  (:shadowing-import-from :fiveam :run)
  (:shadowing-import-from :sl :bit)
  
  (:export :test-sl :sl-main-test))

(in-package :system-lisp-test)

